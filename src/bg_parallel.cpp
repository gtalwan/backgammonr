// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <limits>
#include <random>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"
#include "bg_game.h"
#include "bg_move.h"
#include "bg_movegen.h"
#include "bg_rng.h"
#include "bg_rollout.h"
#include "bg_rules.h"

namespace {

struct CollapsedCandidate {
  backgammonr::BoardState board_after{};
  int representative_index{0};
  int acting_player{1};
  int n_equivalent{1};
};

struct ForcedRollSchedule {
  std::array<backgammonr::DiceRoll, 2> rolls{};
  int n_rolls{0};
};

struct RolloutBlockTask {
  int candidate_position{0};
  int candidate_index{0};
  int n_equivalent_sequences{1};
  int start_count{0};
  int n_rollouts{0};
};

struct RolloutBlockResult {
  int candidate_index{0};
  int n_equivalent_sequences{1};
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int single_loss{0};
  int gammon_loss{0};
  int backgammon_loss{0};
  int unresolved{0};
  int single_win{0};
  int gammon_win{0};
  int backgammon_win{0};
  double reward_sum{0.0};
  double reward_sum_sq{0.0};
};

backgammonr::BoardState apply_sequence_without_full_validation(
    const backgammonr::BoardState& board,
    const backgammonr::MoveSequence& sequence) {
  backgammonr::BoardState out = board;

  for (const backgammonr::MoveStep& step : sequence.steps) {
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  }

  out.turn = -sequence.player;
  return out;
}

void play_random_turn_lightweight(
    backgammonr::BoardState& board,
    const backgammonr::DiceRoll& roll,
    std::mt19937& rng) {
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
}

double outcome_reward(
    const backgammonr::TerminalScoreClass outcome,
    const backgammonr::RolloutConfig& config) {
  if (outcome == backgammonr::TerminalScoreClass::kSingleWin ||
      outcome == backgammonr::TerminalScoreClass::kGammonWin ||
      outcome == backgammonr::TerminalScoreClass::kBackgammonWin) {
    return 1.0;
  }
  if (outcome == backgammonr::TerminalScoreClass::kSingleLoss ||
      outcome == backgammonr::TerminalScoreClass::kGammonLoss ||
      outcome == backgammonr::TerminalScoreClass::kBackgammonLoss) {
    return 0.0;
  }
  return config.unresolved_value;
}

backgammonr::TerminalScoreClass outcome_from_turn_result(
    const backgammonr::TurnResult& turn_result,
    const int acting_player) {
  if (!turn_result.game_over) {
    return backgammonr::TerminalScoreClass::kUnresolved;
  }

  return backgammonr::terminal_score_class(turn_result.board_after, acting_player);
}

const std::vector<backgammonr::DiceRoll>& unique_unordered_rolls() {
  static const std::vector<backgammonr::DiceRoll> outcomes = []() {
    std::vector<backgammonr::DiceRoll> out;
    out.reserve(21);
    for (int die1 = backgammonr::kMinDieValue; die1 <= backgammonr::kMaxDieValue; ++die1) {
      for (int die2 = die1; die2 <= backgammonr::kMaxDieValue; ++die2) {
        out.push_back(backgammonr::make_roll(die1, die2));
      }
    }
    return out;
  }();
  return outcomes;
}

std::uint32_t stable_rollout_seed(
    const std::uint32_t base_seed,
    const int sample_index,
    const int salt) {
  std::uint32_t x = base_seed ^ static_cast<std::uint32_t>(sample_index * 0x9e3779b9U);
  x ^= static_cast<std::uint32_t>(salt * 0x7f4a7c15U);
  x ^= x >> 16;
  x *= 0x85ebca6bU;
  x ^= x >> 13;
  x *= 0xc2b2ae35U;
  x ^= x >> 16;
  return x;
}

ForcedRollSchedule scheduled_forced_rolls(
    const std::string& dice_mode,
    const int sample_index,
    const int offset) {
  ForcedRollSchedule schedule;

  if (dice_mode == "iid") {
    return schedule;
  }

  const std::vector<backgammonr::DiceRoll>& outcomes = unique_unordered_rolls();
  const int n = static_cast<int>(outcomes.size());

  if (dice_mode == "stratified_first_roll") {
    const int idx = (offset + sample_index - 1) % n;
    schedule.rolls[0] = outcomes[idx];
    schedule.n_rolls = 1;
    return schedule;
  }

  if (dice_mode == "stratified_first_two_rolls") {
    const int n2 = n * n;
    const int pair_idx = (offset + sample_index - 1) % n2;
    const int idx1 = pair_idx / n;
    const int idx2 = pair_idx % n;
    schedule.rolls[0] = outcomes[idx1];
    schedule.rolls[1] = outcomes[idx2];
    schedule.n_rolls = 2;
    return schedule;
  }

  throw std::range_error("Unsupported dice mode for rollout block simulation.");
}

backgammonr::TerminalScoreClass single_rollout_outcome(
    const backgammonr::BoardState& board_after,
    const int acting_player,
    const backgammonr::RolloutConfig& config,
    std::mt19937& rng,
    const ForcedRollSchedule& forced_rolls) {
  if (backgammonr::board_is_terminal(board_after)) {
    return backgammonr::terminal_score_class(board_after, acting_player);
  }

  if (config.max_turns <= 0) {
    return backgammonr::TerminalScoreClass::kUnresolved;
  }

  if (config.policy == "random") {
    backgammonr::BoardState current = board_after;
    int turns_remaining = config.max_turns;

    for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
      if (turns_remaining <= 0) {
        return backgammonr::TerminalScoreClass::kUnresolved;
      }

      play_random_turn_lightweight(current, forced_rolls.rolls[forced_idx], rng);
      if (backgammonr::board_is_terminal(current)) {
        return backgammonr::terminal_score_class(current, acting_player);
      }
      --turns_remaining;
    }

    for (int turn = 0; turn < turns_remaining; ++turn) {
      play_random_turn_lightweight(current, backgammonr::roll_dice(rng), rng);
      if (backgammonr::board_is_terminal(current)) {
        return backgammonr::terminal_score_class(current, acting_player);
      }
    }

    return backgammonr::TerminalScoreClass::kUnresolved;
  }

  backgammonr::BoardState current = board_after;
  int turns_remaining = config.max_turns;

  for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
    if (turns_remaining <= 0) {
      return backgammonr::TerminalScoreClass::kUnresolved;
    }

    const backgammonr::TurnResult turn_result = backgammonr::play_turn_with_roll(
        current,
        forced_rolls.rolls[forced_idx],
        config.policy,
        &rng,
        backgammonr::RolloutConfig());
    if (turn_result.game_over) {
      return outcome_from_turn_result(turn_result, acting_player);
    }

    current = turn_result.board_after;
    --turns_remaining;
  }

  const backgammonr::GameResult rollout_result = backgammonr::play_game_random(
      current,
      turns_remaining,
      rng,
      config.policy,
      backgammonr::RolloutConfig());

  if (!rollout_result.game_over) {
    return backgammonr::TerminalScoreClass::kUnresolved;
  }

  return backgammonr::terminal_score_class(rollout_result.final_board, acting_player);
}

struct BoardStateKey {
  std::array<int, backgammonr::kNumPoints> points{};
  std::array<int, backgammonr::kNumPlayers> bar{};
  std::array<int, backgammonr::kNumPlayers> off{};
  int turn{1};

  bool operator==(const BoardStateKey& other) const {
    return turn == other.turn &&
        points == other.points &&
        bar == other.bar &&
        off == other.off;
  }
};

struct BoardStateKeyHash {
  std::size_t operator()(const BoardStateKey& key) const {
    std::size_t h = static_cast<std::size_t>(key.turn * 1315423911U);

    for (const int value : key.points) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }
    for (const int value : key.bar) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }
    for (const int value : key.off) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }

    return h;
  }
};

BoardStateKey board_state_key(const backgammonr::BoardState& board) {
  BoardStateKey key;
  key.points = board.points;
  key.bar = board.bar;
  key.off = board.off;
  key.turn = board.turn;
  return key;
}

std::vector<CollapsedCandidate> collapse_equivalent_candidates(
    const backgammonr::BoardState& board,
    const std::vector<backgammonr::MoveSequence>& legal_moves) {
  std::vector<CollapsedCandidate> collapsed;
  collapsed.reserve(legal_moves.size());
  std::unordered_map<BoardStateKey, int, BoardStateKeyHash> key_to_collapsed_index;
  key_to_collapsed_index.reserve(legal_moves.size());

  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    const backgammonr::BoardState board_after =
        apply_sequence_without_full_validation(board, legal_moves[i]);
    const BoardStateKey key = board_state_key(board_after);
    const auto it = key_to_collapsed_index.find(key);

    if (it == key_to_collapsed_index.end()) {
      CollapsedCandidate row;
      row.board_after = board_after;
      row.representative_index = i;
      row.acting_player = legal_moves[i].player;
      row.n_equivalent = 1;
      key_to_collapsed_index.emplace(key, static_cast<int>(collapsed.size()));
      collapsed.push_back(row);
    } else {
      collapsed[it->second].n_equivalent += 1;
    }
  }

  return collapsed;
}

class RolloutBlockWorker : public RcppParallel::Worker {
 public:
  RolloutBlockWorker(
      const std::vector<CollapsedCandidate>& candidates,
      const std::vector<RolloutBlockTask>& tasks,
      const backgammonr::RolloutConfig& config,
      const std::uint32_t base_seed,
      std::vector<RolloutBlockResult>& results)
      : candidates_(candidates),
        tasks_(tasks),
        config_(config),
        base_seed_(base_seed),
        results_(results) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t task_index = begin; task_index < end; ++task_index) {
      const RolloutBlockTask& task = tasks_[task_index];
      const CollapsedCandidate& candidate = candidates_[task.candidate_position];

      RolloutBlockResult out;
      out.candidate_index = task.candidate_index;
      out.n_equivalent_sequences = task.n_equivalent_sequences;
      out.allocation_count = task.n_rollouts;

      if (task.n_rollouts <= 0) {
        results_[task_index] = out;
        continue;
      }

      const std::uint32_t task_seed = stable_rollout_seed(
          base_seed_,
          task.start_count + 1,
          task.candidate_index * 7919);
      std::mt19937 rng(task_seed);
      int stratification_offset = 0;

      if (config_.dice_mode != "iid" && !config_.crn) {
        const int n_outcomes = config_.dice_mode == "stratified_first_two_rolls" ? 441 : 21;
        std::uniform_int_distribution<int> offset_dist(0, n_outcomes - 1);
        stratification_offset = offset_dist(rng);
      }

      for (int local_idx = 0; local_idx < task.n_rollouts; ++local_idx) {
        const int sample_index = task.start_count + local_idx + 1;
        const int offset = config_.crn ? 0 : stratification_offset;
        const ForcedRollSchedule forced_rolls =
            scheduled_forced_rolls(config_.dice_mode, sample_index, offset);

        std::mt19937* rollout_rng = &rng;
        std::mt19937 crn_rng;
        if (config_.crn) {
          crn_rng.seed(stable_rollout_seed(base_seed_, sample_index, 0));
          rollout_rng = &crn_rng;
        }

        const backgammonr::TerminalScoreClass outcome = single_rollout_outcome(
            candidate.board_after,
            candidate.acting_player,
            config_,
            *rollout_rng,
            forced_rolls);

        if (outcome == backgammonr::TerminalScoreClass::kSingleWin) {
          out.single_win += 1;
          out.wins += 1;
        } else if (outcome == backgammonr::TerminalScoreClass::kGammonWin) {
          out.gammon_win += 1;
          out.wins += 1;
        } else if (outcome == backgammonr::TerminalScoreClass::kBackgammonWin) {
          out.backgammon_win += 1;
          out.wins += 1;
        } else if (outcome == backgammonr::TerminalScoreClass::kSingleLoss) {
          out.single_loss += 1;
          out.losses += 1;
        } else if (outcome == backgammonr::TerminalScoreClass::kGammonLoss) {
          out.gammon_loss += 1;
          out.losses += 1;
        } else if (outcome == backgammonr::TerminalScoreClass::kBackgammonLoss) {
          out.backgammon_loss += 1;
          out.losses += 1;
        } else {
          out.unresolved += 1;
        }

        const double reward = outcome_reward(outcome, config_);
        out.reward_sum += reward;
        out.reward_sum_sq += reward * reward;
      }

      results_[task_index] = out;
    }
  }

 private:
  const std::vector<CollapsedCandidate>& candidates_;
  const std::vector<RolloutBlockTask>& tasks_;
  const backgammonr::RolloutConfig config_;
  const std::uint32_t base_seed_;
  std::vector<RolloutBlockResult>& results_;
};

}  // namespace

// [[Rcpp::export]]
Rcpp::List bg_cpp_rollout_blocks(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const Rcpp::IntegerVector& candidate_index,
    const Rcpp::IntegerVector& block_rollouts,
    const Rcpp::IntegerVector& start_counts,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const double unresolved_value,
    const std::string& dice_mode,
    const bool crn,
    const int task_block_size,
    const int seed,
    const bool use_seed) {
  if (task_block_size < 1) {
    throw std::range_error("`task_block_size` must be at least 1.");
  }

  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const std::vector<CollapsedCandidate> collapsed =
      collapse_equivalent_candidates(parsed_board, parsed_moves);

  backgammonr::RolloutConfig config;
  config.budget = 1;
  config.policy = rollout_policy;
  config.max_turns = max_rollout_turns;
  config.unresolved_value = unresolved_value;
  config.dice_mode = dice_mode;
  config.crn = crn;
  backgammonr::validate_rollout_config(config);

  Rcpp::IntegerVector collapsed_index(collapsed.size());
  Rcpp::IntegerVector n_equivalent_sequences(collapsed.size());
  for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
    collapsed_index[i] = collapsed[i].representative_index + 1;
    n_equivalent_sequences[i] = collapsed[i].n_equivalent;
  }

  if (candidate_index.size() == 0) {
    return Rcpp::List::create(
        Rcpp::_["candidate_map"] = Rcpp::DataFrame::create(
            Rcpp::_["candidate_index"] = collapsed_index,
            Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
            Rcpp::_["stringsAsFactors"] = false),
        Rcpp::_["results"] = Rcpp::DataFrame::create(
            Rcpp::_["candidate_index"] = collapsed_index,
            Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
        Rcpp::_["added_allocation_count"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["wins"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["losses"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["single_loss"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["gammon_loss"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["backgammon_loss"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["unresolved"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["single_win"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["gammon_win"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["backgammon_win"] = Rcpp::IntegerVector(collapsed.size()),
        Rcpp::_["reward_sum"] = Rcpp::NumericVector(collapsed.size()),
        Rcpp::_["reward_sum_sq"] = Rcpp::NumericVector(collapsed.size()),
        Rcpp::_["stringsAsFactors"] = false));
  }

  if (candidate_index.size() != block_rollouts.size()) {
    throw std::range_error("`candidate_index` and `block_rollouts` must have the same length.");
  }

  Rcpp::IntegerVector normalized_start_counts = start_counts;
  if (normalized_start_counts.size() == 0) {
    normalized_start_counts = Rcpp::IntegerVector(candidate_index.size(), 0);
  }
  if (normalized_start_counts.size() != candidate_index.size()) {
    throw std::range_error("`start_counts` must be empty or match `candidate_index` length.");
  }

  std::unordered_map<int, int> representative_to_position;
  representative_to_position.reserve(collapsed.size());
  for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
    representative_to_position.emplace(collapsed[i].representative_index + 1, i);
  }

  std::vector<RolloutBlockTask> tasks;
  for (int i = 0; i < candidate_index.size(); ++i) {
    const int representative = candidate_index[i];
    const int n_rollouts = block_rollouts[i];
    const int start_count = normalized_start_counts[i];

    if (n_rollouts < 0) {
      throw std::range_error("`block_rollouts` values must be nonnegative.");
    }
    if (start_count < 0) {
      throw std::range_error("`start_counts` values must be nonnegative.");
    }

    const auto it = representative_to_position.find(representative);
    if (it == representative_to_position.end()) {
      throw std::range_error("Requested `candidate_index` was not found in the collapsed candidate set.");
    }

    int remaining = n_rollouts;
    int chunk_start = start_count;
    while (remaining > 0) {
      const int chunk = std::min(remaining, task_block_size);
      RolloutBlockTask task;
      task.candidate_position = it->second;
      task.candidate_index = representative;
      task.n_equivalent_sequences = collapsed[it->second].n_equivalent;
      task.start_count = chunk_start;
      task.n_rollouts = chunk;
      tasks.push_back(task);
      remaining -= chunk;
      chunk_start += chunk;
    }
  }

  std::mt19937 seed_rng = backgammonr::init_rng(seed, use_seed);
  const std::uint32_t base_seed = static_cast<std::uint32_t>(seed_rng());
  std::vector<RolloutBlockResult> task_results(tasks.size());

  if (!tasks.empty()) {
    RolloutBlockWorker worker(collapsed, tasks, config, base_seed, task_results);
    RcppParallel::parallelFor(
        static_cast<std::size_t>(0),
        static_cast<std::size_t>(tasks.size()),
        worker,
        static_cast<std::size_t>(1));
  }

  std::unordered_map<int, RolloutBlockResult> aggregated;
  aggregated.reserve(candidate_index.size());
  for (int i = 0; i < candidate_index.size(); ++i) {
    RolloutBlockResult row;
    row.candidate_index = candidate_index[i];
    row.n_equivalent_sequences = collapsed[representative_to_position[candidate_index[i]]].n_equivalent;
    aggregated.emplace(candidate_index[i], row);
  }

  for (const RolloutBlockResult& task_row : task_results) {
    RolloutBlockResult& agg = aggregated[task_row.candidate_index];
    agg.candidate_index = task_row.candidate_index;
    agg.n_equivalent_sequences = task_row.n_equivalent_sequences;
    agg.allocation_count += task_row.allocation_count;
    agg.wins += task_row.wins;
    agg.losses += task_row.losses;
    agg.single_loss += task_row.single_loss;
    agg.gammon_loss += task_row.gammon_loss;
    agg.backgammon_loss += task_row.backgammon_loss;
    agg.unresolved += task_row.unresolved;
    agg.single_win += task_row.single_win;
    agg.gammon_win += task_row.gammon_win;
    agg.backgammon_win += task_row.backgammon_win;
    agg.reward_sum += task_row.reward_sum;
    agg.reward_sum_sq += task_row.reward_sum_sq;
  }

  Rcpp::IntegerVector out_candidate(aggregated.size());
  Rcpp::IntegerVector out_equivalent(aggregated.size());
  Rcpp::IntegerVector out_alloc(aggregated.size());
  Rcpp::IntegerVector out_wins(aggregated.size());
  Rcpp::IntegerVector out_losses(aggregated.size());
  Rcpp::IntegerVector out_single_loss(aggregated.size());
  Rcpp::IntegerVector out_gammon_loss(aggregated.size());
  Rcpp::IntegerVector out_backgammon_loss(aggregated.size());
  Rcpp::IntegerVector out_unresolved(aggregated.size());
  Rcpp::IntegerVector out_single_win(aggregated.size());
  Rcpp::IntegerVector out_gammon_win(aggregated.size());
  Rcpp::IntegerVector out_backgammon_win(aggregated.size());
  Rcpp::NumericVector out_reward_sum(aggregated.size());
  Rcpp::NumericVector out_reward_sum_sq(aggregated.size());

  int row = 0;
  std::vector<int> ordered_candidates = Rcpp::as<std::vector<int>>(candidate_index);
  std::sort(ordered_candidates.begin(), ordered_candidates.end());
  ordered_candidates.erase(std::unique(ordered_candidates.begin(), ordered_candidates.end()), ordered_candidates.end());

  for (const int candidate_id : ordered_candidates) {
    const RolloutBlockResult& agg = aggregated[candidate_id];
    out_candidate[row] = agg.candidate_index;
    out_equivalent[row] = agg.n_equivalent_sequences;
    out_alloc[row] = agg.allocation_count;
    out_wins[row] = agg.wins;
    out_losses[row] = agg.losses;
    out_single_loss[row] = agg.single_loss;
    out_gammon_loss[row] = agg.gammon_loss;
    out_backgammon_loss[row] = agg.backgammon_loss;
    out_unresolved[row] = agg.unresolved;
    out_single_win[row] = agg.single_win;
    out_gammon_win[row] = agg.gammon_win;
    out_backgammon_win[row] = agg.backgammon_win;
    out_reward_sum[row] = agg.reward_sum;
    out_reward_sum_sq[row] = agg.reward_sum_sq;
    ++row;
  }

  return Rcpp::List::create(
      Rcpp::_["candidate_map"] = Rcpp::DataFrame::create(
          Rcpp::_["candidate_index"] = collapsed_index,
          Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
          Rcpp::_["stringsAsFactors"] = false),
      Rcpp::_["results"] = Rcpp::DataFrame::create(
          Rcpp::_["candidate_index"] = out_candidate,
          Rcpp::_["n_equivalent_sequences"] = out_equivalent,
          Rcpp::_["added_allocation_count"] = out_alloc,
          Rcpp::_["wins"] = out_wins,
          Rcpp::_["losses"] = out_losses,
          Rcpp::_["single_loss"] = out_single_loss,
          Rcpp::_["gammon_loss"] = out_gammon_loss,
          Rcpp::_["backgammon_loss"] = out_backgammon_loss,
          Rcpp::_["unresolved"] = out_unresolved,
          Rcpp::_["single_win"] = out_single_win,
          Rcpp::_["gammon_win"] = out_gammon_win,
          Rcpp::_["backgammon_win"] = out_backgammon_win,
          Rcpp::_["reward_sum"] = out_reward_sum,
          Rcpp::_["reward_sum_sq"] = out_reward_sum_sq,
          Rcpp::_["stringsAsFactors"] = false));
}
