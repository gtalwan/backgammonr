// Legal-move generation kernels.
#include "bg_movegen.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <unordered_set>
#include <utility>
#include <vector>

#include "bg_rules.h"

namespace {

// Validate that the engine-facing player id is one of the two supported values.
void validate_player(const int player) {
  if (player != 1 && player != -1) {
    throw std::range_error("`player` must be either 1L or -1L.");
  }
}

// Compact key used to deduplicate move sequences that are equivalent after
// forced-use filtering.
struct SequenceSignature {
  int player{1};
  int n_steps{0};
  std::array<std::uint16_t, backgammonr::kDoubleExpandedDice> from{};
  std::array<std::uint16_t, backgammonr::kDoubleExpandedDice> to{};
  std::array<std::uint8_t, backgammonr::kDoubleExpandedDice> die{};
  std::array<std::uint8_t, backgammonr::kDoubleExpandedDice> hit{};

  bool operator==(const SequenceSignature& other) const {
    return player == other.player &&
        n_steps == other.n_steps &&
        from == other.from &&
        to == other.to &&
        die == other.die &&
        hit == other.hit;
  }
};

struct SequenceSignatureHash {
  std::size_t operator()(const SequenceSignature& sig) const {
    // Seed hash with player and sequence length.
    std::size_t h = static_cast<std::size_t>(sig.player * 131 + sig.n_steps);
    for (int i = 0; i < sig.n_steps; ++i) {
      // Mix each step field into the rolling hash.
      h ^= static_cast<std::size_t>(sig.from[i]) + 0x9e3779b9U + (h << 6) + (h >> 2);
      h ^= static_cast<std::size_t>(sig.to[i]) + 0x9e3779b9U + (h << 6) + (h >> 2);
      h ^= static_cast<std::size_t>(sig.die[i]) + 0x9e3779b9U + (h << 6) + (h >> 2);
      h ^= static_cast<std::size_t>(sig.hit[i]) + 0x9e3779b9U + (h << 6) + (h >> 2);
    }
    return h;
  }
};

SequenceSignature sequence_signature(const backgammonr::MoveSequence& sequence) {
  SequenceSignature sig;
  sig.player = sequence.player;
  // Only up to kDoubleExpandedDice steps can matter in this engine.
  sig.n_steps = static_cast<int>(std::min<std::size_t>(
      sequence.steps.size(), backgammonr::kDoubleExpandedDice));

  for (int i = 0; i < sig.n_steps; ++i) {
    const backgammonr::MoveStep& step = sequence.steps[i];
    sig.from[i] = static_cast<std::uint16_t>(step.from);
    sig.to[i] = static_cast<std::uint16_t>(step.to);
    sig.die[i] = static_cast<std::uint8_t>(step.die);
    sig.hit[i] = static_cast<std::uint8_t>(step.hit ? 1 : 0);
  }

  return sig;
}

struct RandomSelectionResult {
  // Selected step path (fixed storage to avoid heap allocation in tight loops).
  std::array<backgammonr::MoveStep, backgammonr::kDoubleExpandedDice> steps{};
  int n_steps{0};
};

struct RandomSelectionReservoir {
  // Longest legal sequence depth seen so far.
  int best_depth{0};
  // Roll metadata needed for forced-use handling.
  bool roll_is_double{false};
  int higher_die{backgammonr::kMinDieValue};
  // Reservoir sample counters.
  std::uint64_t count_any{0ULL};
  std::uint64_t count_high{0ULL};
  std::uint64_t count_low{0ULL};
  // Current reservoir representatives.
  RandomSelectionResult selected_any{};
  RandomSelectionResult selected_high{};
  RandomSelectionResult selected_low{};
};

// Expand a 2-die roll into either 2 values (normal) or 4 values (double).
void fill_dice_values_from_roll(
    const backgammonr::DiceRoll& roll,
    std::array<int, backgammonr::kDoubleExpandedDice>& dice_values,
    int& n_dice) {
  if (roll.is_double()) {
    // Double uses the same die value four times.
    n_dice = backgammonr::kDoubleExpandedDice;
    for (int i = 0; i < n_dice; ++i) {
      dice_values[i] = roll.dice[0];
    }
    return;
  }

  n_dice = backgammonr::kNumDice;
  dice_values[0] = roll.dice[0];
  dice_values[1] = roll.dice[1];
}

// Undo a previously applied step in-place.
// This is the key primitive that allows DFS without board copying.
void undo_move_step_unchecked_inplace(
    backgammonr::BoardState& board,
    const int player,
    const backgammonr::MoveStep& step) {
  const int current_player_index = player == 1 ? 0 : 1;
  const int other_player_index = 1 - current_player_index;

  if (step.to == backgammonr::kOffPosition) {
    // Undo bear off.
    board.off[current_player_index] -= 1;
  } else if (step.hit) {
    // Undo hit: restore opponent checker from bar back to destination point.
    board.bar[other_player_index] -= 1;
    board.points[step.to - 1] = -player;
  } else {
    // Undo normal destination occupancy change.
    board.points[step.to - 1] -= player;
  }

  if (step.from == backgammonr::kBarPosition) {
    // Undo entry from bar.
    board.bar[current_player_index] += 1;
  } else {
    // Undo checker removal from original point.
    board.points[step.from - 1] += player;
  }
}

// Reservoir-sampling update for one completed legal path.
void maybe_update_reservoir_choice(
    RandomSelectionReservoir& reservoir,
    const std::array<backgammonr::MoveStep, backgammonr::kDoubleExpandedDice>& current_steps,
    const int depth,
    std::mt19937& rng) {
  if (depth > reservoir.best_depth) {
    // Found deeper legal use of dice: reset all reservoirs to this new depth.
    reservoir.best_depth = depth;
    reservoir.count_any = 0ULL;
    reservoir.count_high = 0ULL;
    reservoir.count_low = 0ULL;
    reservoir.selected_any.n_steps = 0;
    reservoir.selected_high.n_steps = 0;
    reservoir.selected_low.n_steps = 0;
  } else if (depth < reservoir.best_depth) {
    // Ignore shorter paths (forced-use: must use maximum number of dice).
    return;
  }

  if (!reservoir.roll_is_double && depth == 1) {
    // Special forced-use rule for one-step non-double plays:
    // if higher die is playable, lower-die-only sequences are disallowed.
    if (current_steps[0].die == reservoir.higher_die) {
      reservoir.count_high += 1ULL;
      std::uniform_int_distribution<std::uint64_t> dist(1ULL, reservoir.count_high);
      if (dist(rng) == 1ULL) {
        reservoir.selected_high.n_steps = 1;
        reservoir.selected_high.steps[0] = current_steps[0];
      }
      return;
    }

    // Track lower-die options separately; they are used only if no high-die
    // one-step option exists.
    reservoir.count_low += 1ULL;
    std::uniform_int_distribution<std::uint64_t> dist(1ULL, reservoir.count_low);
    if (dist(rng) == 1ULL) {
      reservoir.selected_low.n_steps = 1;
      reservoir.selected_low.steps[0] = current_steps[0];
    }
    return;
  }

  reservoir.count_any += 1ULL;
  // Standard reservoir update for general case.
  std::uniform_int_distribution<std::uint64_t> dist(1ULL, reservoir.count_any);
  if (dist(rng) == 1ULL) {
    reservoir.selected_any.n_steps = depth;
    for (int i = 0; i < depth; ++i) {
      reservoir.selected_any.steps[i] = current_steps[i];
    }
  }
}

// DFS that streams legal sequences and keeps one uniform random sample
// without materializing the full legal sequence set.
void sample_random_sequence_one_pass_recursive(
    backgammonr::BoardState& board,
    const int player,
    const std::array<int, backgammonr::kDoubleExpandedDice>& dice_values,
    const int n_dice,
    const std::uint8_t used_mask,
    const int depth,
    std::array<backgammonr::MoveStep, backgammonr::kDoubleExpandedDice>& current_steps,
    RandomSelectionReservoir& reservoir,
    std::mt19937& rng) {
  bool extended = false;
  // Fixed-size temporary step buffer for current die.
  std::array<backgammonr::MoveStep, backgammonr::kNumPoints> legal_steps{};

  for (int die = backgammonr::kMinDieValue; die <= backgammonr::kMaxDieValue; ++die) {
    // Pick one unused die slot with this value (needed for doubles + masks).
    int die_index = -1;
    for (int idx = 0; idx < n_dice; ++idx) {
      if ((used_mask & static_cast<std::uint8_t>(1U << idx)) != 0U) {
        continue;
      }
      if (dice_values[idx] == die) {
        die_index = idx;
        break;
      }
    }
    if (die_index < 0) {
      continue;
    }

    // Generate legal one-step moves for this die value from current board.
    const int n_legal_steps =
        backgammonr::legal_steps_for_die_into_array(board, player, die, legal_steps);
    if (n_legal_steps <= 0) {
      continue;
    }

    extended = true;
    // Mark selected die slot as used for downstream recursion.
    const std::uint8_t next_used_mask =
        static_cast<std::uint8_t>(used_mask | static_cast<std::uint8_t>(1U << die_index));

    for (int step_index = 0; step_index < n_legal_steps; ++step_index) {
      const backgammonr::MoveStep& step = legal_steps[step_index];
      // Apply step, recurse, then undo step in place.
      current_steps[depth] = step;
      backgammonr::apply_move_step_unchecked_inplace(board, player, step);
      sample_random_sequence_one_pass_recursive(
          board,
          player,
          dice_values,
          n_dice,
          next_used_mask,
          depth + 1,
          current_steps,
          reservoir,
          rng);
      undo_move_step_unchecked_inplace(board, player, step);
    }
  }

  if (!extended && depth > 0) {
    // Leaf: no more legal continuations from this prefix.
    maybe_update_reservoir_choice(reservoir, current_steps, depth, rng);
  }
}

// DFS used by public legal move generation.
// This path still materializes all sequences for API output.
void generate_sequences_recursive(
    backgammonr::BoardState& board,
    const int player,
    const std::array<int, backgammonr::kDoubleExpandedDice>& dice_values,
    const int n_dice,
    const std::uint8_t used_mask,
    const backgammonr::DiceRoll& roll,
    std::vector<backgammonr::MoveStep>& current_steps,
    std::vector<backgammonr::MoveSequence>& out_sequences) {
  bool extended = false;
  // Fixed-size temporary buffer for per-die legal steps.
  std::array<backgammonr::MoveStep, backgammonr::kNumPoints> legal_steps{};

  for (int die = backgammonr::kMinDieValue; die <= backgammonr::kMaxDieValue; ++die) {
    int die_index = -1;
    for (int idx = 0; idx < n_dice; ++idx) {
      if ((used_mask & static_cast<std::uint8_t>(1U << idx)) != 0U) {
        continue;
      }
      if (dice_values[idx] == die) {
        die_index = idx;
        break;
      }
    }
    if (die_index < 0) {
      continue;
    }

    const int n_legal_steps =
        backgammonr::legal_steps_for_die_into_array(board, player, die, legal_steps);
    if (n_legal_steps <= 0) {
      continue;
    }

    extended = true;
    const std::uint8_t next_used_mask =
        static_cast<std::uint8_t>(used_mask | static_cast<std::uint8_t>(1U << die_index));

    for (int step_index = 0; step_index < n_legal_steps; ++step_index) {
      const backgammonr::MoveStep& step = legal_steps[step_index];
      // Record step in current path, recurse, then undo path extension.
      current_steps.push_back(step);
      backgammonr::apply_move_step_unchecked_inplace(board, player, step);
      generate_sequences_recursive(
          board,
          player,
          dice_values,
          n_dice,
          next_used_mask,
          roll,
          current_steps,
          out_sequences);
      undo_move_step_unchecked_inplace(board, player, step);
      current_steps.pop_back();
    }
  }

  if (!extended && !current_steps.empty()) {
    // Leaf with at least one step: emit a full sequence candidate.
    backgammonr::MoveSequence sequence;
    sequence.player = player;
    sequence.steps = current_steps;
    sequence.roll = roll;
    out_sequences.push_back(std::move(sequence));
  }
}

// Apply backgammon forced-use rules to raw DFS sequences:
// 1) keep only max-step sequences,
// 2) enforce higher-die rule in one-step non-double case,
// 3) deduplicate equivalent sequences.
std::vector<backgammonr::MoveSequence> filter_forced_use_rules(
    std::vector<backgammonr::MoveSequence> sequences,
    const backgammonr::DiceRoll& roll) {
  if (sequences.empty()) {
    return {};
  }

  std::size_t max_steps = 0;
  for (const backgammonr::MoveSequence& sequence : sequences) {
    max_steps = std::max(max_steps, sequence.steps.size());
  }

  if (max_steps == 0) {
    return {};
  }

  std::vector<backgammonr::MoveSequence> filtered;
  filtered.reserve(sequences.size());
  for (backgammonr::MoveSequence& sequence : sequences) {
    if (sequence.steps.size() == max_steps) {
      filtered.push_back(std::move(sequence));
    }
  }

  if (!roll.is_double() && max_steps == 1) {
    // If one-step only, higher die must be used if any such move exists.
    const int higher_die = std::max(roll.dice[0], roll.dice[1]);
    bool higher_die_is_playable = false;

    for (const backgammonr::MoveSequence& sequence : filtered) {
      if (!sequence.steps.empty() && sequence.steps[0].die == higher_die) {
        higher_die_is_playable = true;
        break;
      }
    }

    if (higher_die_is_playable) {
      std::vector<backgammonr::MoveSequence> higher_only;
      higher_only.reserve(filtered.size());
      for (backgammonr::MoveSequence& sequence : filtered) {
        if (!sequence.steps.empty() && sequence.steps[0].die == higher_die) {
          higher_only.push_back(std::move(sequence));
        }
      }
      filtered = higher_only;
    }
  }

  std::unordered_set<SequenceSignature, SequenceSignatureHash> seen_keys;
  std::vector<backgammonr::MoveSequence> out;
  out.reserve(filtered.size());

  for (backgammonr::MoveSequence& sequence : filtered) {
    if (seen_keys.insert(sequence_signature(sequence)).second) {
      out.push_back(std::move(sequence));
    }
  }

  return out;
}

}  // namespace

namespace backgammonr {

// Public legal move generator used by R API and deterministic selection paths.
std::vector<MoveSequence> generate_legal_move_sequences(
    const BoardState& board,
    const int player,
    const DiceRoll& roll) {
  validate_player(player);

  const Rcpp::IntegerVector expanded = expanded_roll(roll);
  const int n_dice = expanded.size();
  std::array<int, kDoubleExpandedDice> dice_values{};
  for (int i = 0; i < n_dice; ++i) {
    dice_values[i] = expanded[i];
  }

  std::vector<MoveStep> current_steps;
  std::vector<MoveSequence> raw_sequences;
  // Reserve avoids repeated growth in common positions.
  current_steps.reserve(n_dice);
  raw_sequences.reserve(64);
  // Work board mutated in recursion and restored by undo at each branch.
  BoardState board_work = board;

  generate_sequences_recursive(board_work, player, dice_values, n_dice, 0U, roll, current_steps, raw_sequences);
  return filter_forced_use_rules(std::move(raw_sequences), roll);
}

// Fast rollout-only random turn:
// uniform random legal sequence selection without full move list materialization.
bool play_random_turn_rollout_fast(
    BoardState& board,
    const DiceRoll& roll,
    std::mt19937& rng) {
  const int player = board.turn;
  validate_player(player);

  // Prepare expanded dice representation.
  std::array<int, kDoubleExpandedDice> dice_values{};
  int n_dice = 0;
  fill_dice_values_from_roll(roll, dice_values, n_dice);

  // Run one-pass DFS reservoir sampler over legal sequences.
  std::array<MoveStep, kDoubleExpandedDice> current_steps{};
  RandomSelectionReservoir reservoir;
  reservoir.roll_is_double = roll.is_double();
  reservoir.higher_die = std::max(roll.dice[0], roll.dice[1]);
  sample_random_sequence_one_pass_recursive(
      board,
      player,
      dice_values,
      n_dice,
      0U,
      0,
      current_steps,
      reservoir,
      rng);

  RandomSelectionResult selected;
  if (!reservoir.roll_is_double && reservoir.best_depth == 1 && reservoir.count_high > 0ULL) {
    // Forced one-step higher-die rule branch.
    selected = reservoir.selected_high;
  } else if (!reservoir.roll_is_double && reservoir.best_depth == 1 && reservoir.count_low > 0ULL) {
    // Fallback if no higher-die one-step candidate exists.
    selected = reservoir.selected_low;
  } else {
    // General case.
    selected = reservoir.selected_any;
  }

  if (reservoir.best_depth <= 0 || selected.n_steps <= 0) {
    // No legal move: pass turn.
    board.turn = -player;
    return false;
  }

  // Apply selected sequence in place.
  for (int i = 0; i < selected.n_steps; ++i) {
    apply_move_step_unchecked_inplace(board, player, selected.steps[i]);
  }
  // Hand off turn after move application.
  board.turn = -player;
  return true;
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::List bg_cpp_legal_moves(
    const Rcpp::List& board,
    const int player,
    const Rcpp::List& roll) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  // Generate legal move sequences in engine format.
  const std::vector<backgammonr::MoveSequence> sequences =
      backgammonr::generate_legal_move_sequences(parsed_board, player, parsed_roll);

  // Convert engine sequence objects back to R list representation.
  Rcpp::List out(sequences.size());
  for (int i = 0; i < static_cast<int>(sequences.size()); ++i) {
    out[i] = backgammonr::move_sequence_to_list(sequences[i]);
  }

  return out;
}
