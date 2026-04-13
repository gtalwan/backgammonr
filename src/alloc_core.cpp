// Shared fixed-budget allocation engine.
//
// Purpose:
// - own the one adaptive rollout loop used by the fast scalar allocation layer;
// - keep rollout execution, candidate collapsing, CRN handling, and summary
//   refreshes in one place; and
// - ensure policy files differ only in "which arm is chosen next", not in how
//   evidence is simulated or accumulated.
//
// Inputs:
// - one board state plus its legal moves;
// - one canonical allocation method label; and
// - one rollout configuration describing budget, rollout policy, unresolved
//   handling, and variance-control settings.
//
// Outputs:
// - one per-candidate summary table with empirical counts, posterior moments,
//   and optional diagnostic fields; and
// - optionally, an allocation trace sampled at checkpoint intervals.
//
// Statistical meaning:
// The engine treats each collapsed successor board as an "arm" in a fixed-
// budget pure-exploration problem. TS, TTTS, equal allocation, and legacy
// baselines all share this exact simulation/update path.

#include <RcppArmadillo.h>

#include "alloc_core.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <stdexcept>
#include <unordered_map>
#include <vector>

#include "alloc_trace.h"
#include "bg_game.h"
#include "bg_movegen.h"
#include "bg_rules.h"

namespace backgammonr {
namespace allocation {

BoardState apply_sequence_without_full_validation(
    const BoardState& board,
    const MoveSequence& sequence) {
  BoardState out = board;

  for (const MoveStep& step : sequence.steps) {
    apply_move_step_unchecked_inplace(out, sequence.player, step);
  }
  out.turn = -sequence.player;
  return out;
}

void play_random_turn_lightweight(
    BoardState& board,
    const DiceRoll& roll,
    std::mt19937& rng) {
  (void) play_random_turn_rollout_fast(board, roll, rng);
}

double outcome_reward(const RolloutOutcome outcome, const RolloutConfig& config) {
  // Convert the categorical rollout outcome into the scalar reward seen by the
  // fast Beta-style engine. Win/loss stay on {1, 0}; unresolved mass is mapped
  // through the caller's declared unresolved value.
  if (outcome == RolloutOutcome::kWin) {
    return 1.0;
  }

  if (outcome == RolloutOutcome::kLoss) {
    return 0.0;
  }

  return config.unresolved_value;
}

RolloutOutcome single_rollout_outcome(
    const BoardState& board_after,
    const int acting_player,
    const RolloutConfig& config,
    std::mt19937& rng,
    const ForcedRollSchedule& forced_rolls) {
  if (board_is_terminal(board_after)) {
    return board_winner(board_after) == acting_player
        ? RolloutOutcome::kWin
        : RolloutOutcome::kLoss;
  }

  if (config.max_turns <= 0) {
    return RolloutOutcome::kUnresolved;
  }

  if (config.policy == "random") {
    BoardState current = board_after;
    int turns_remaining = config.max_turns;

    for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
      if (turns_remaining <= 0) {
        return RolloutOutcome::kUnresolved;
      }

      play_random_turn_lightweight(current, forced_rolls.rolls[forced_idx], rng);
      if (board_is_terminal(current)) {
        return board_winner(current) == acting_player
            ? RolloutOutcome::kWin
            : RolloutOutcome::kLoss;
      }
      --turns_remaining;
    }

    for (int turn = 0; turn < turns_remaining; ++turn) {
      play_random_turn_lightweight(current, roll_dice(rng), rng);
      if (board_is_terminal(current)) {
        return board_winner(current) == acting_player
            ? RolloutOutcome::kWin
            : RolloutOutcome::kLoss;
      }
    }

    return RolloutOutcome::kUnresolved;
  }

  BoardState current = board_after;
  int turns_remaining = config.max_turns;
  for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
    if (turns_remaining <= 0) {
      return RolloutOutcome::kUnresolved;
    }

    const TurnResult turn_result = play_turn_with_roll(
        current,
        forced_rolls.rolls[forced_idx],
        config.policy,
        &rng,
        RolloutConfig());
    if (turn_result.game_over) {
      return outcome_from_turn_result(turn_result, acting_player);
    }

    current = turn_result.board_after;
    --turns_remaining;
  }

  const GameResult rollout_result = play_game_random(
      current,
      turns_remaining,
      rng,
      config.policy,
      RolloutConfig());

  if (!rollout_result.game_over) {
    return RolloutOutcome::kUnresolved;
  }
  return rollout_result.winner == acting_player
      ? RolloutOutcome::kWin
      : RolloutOutcome::kLoss;
}

void update_summary(
    ActionEvaluationSummary& summary,
    const RolloutOutcome outcome,
    const RolloutConfig& config) {
  // Every rollout increments one arm's sufficient statistics. The alpha/beta
  // updates intentionally track scalar reward mass rather than strict
  // Bernoulli win/loss counts so unresolved outcomes can contribute partial
  // pseudo-observations when requested.
  summary.allocation_count += 1;

  if (outcome == RolloutOutcome::kWin) {
    summary.wins += 1;
  } else if (outcome == RolloutOutcome::kLoss) {
    summary.losses += 1;
  } else {
    summary.unresolved += 1;
  }

  const double reward = outcome_reward(outcome, config);
  summary.alpha += reward;
  summary.beta += (1.0 - reward);
}

double sample_beta_distribution(const double alpha, const double beta, std::mt19937& rng) {
  // The fast TS/TTTS backend samples from a Beta posterior implied by the
  // scalar sufficient statistics. Gamma-normalization is numerically stable and
  // keeps the implementation consistent with standard Beta sampling.
  if (alpha <= 0.0 || beta <= 0.0) {
    throw std::range_error("Beta posterior parameters must be positive.");
  }

  std::gamma_distribution<double> gamma_alpha(alpha, 1.0);
  std::gamma_distribution<double> gamma_beta(beta, 1.0);
  const double x = gamma_alpha(rng);
  const double y = gamma_beta(rng);

  if (x <= 0.0 && y <= 0.0) {
    return 0.5;
  }

  return x / (x + y);
}

AllocationPolicy parse_allocation_policy(const std::string& canonical_method) {
  // R normalizes method aliases before calling into C++, so this parser only
  // needs to recognize the canonical internal labels.
  if (canonical_method == "equal") {
    return AllocationPolicy::kEqual;
  }
  if (canonical_method == "greedy") {
    return AllocationPolicy::kGreedy;
  }
  if (canonical_method == "ucb") {
    return AllocationPolicy::kUcb;
  }
  if (canonical_method == "thompson") {
    return AllocationPolicy::kThompson;
  }
  if (canonical_method == "ttts") {
    return AllocationPolicy::kTtts;
  }
  if (canonical_method == "ocba") {
    return AllocationPolicy::kOcba;
  }

  throw std::range_error("Unsupported allocation method.");
}

bool score_beats_incumbent(
    const double score,
    const int allocation_count,
    const double incumbent_score,
    const int incumbent_allocation_count) {
  // Tie-breaking is deterministic on lower allocation count so purely equal
  // scores still prefer less-sampled arms. That keeps greedy/UCB style scans
  // reproducible and slightly more exploratory at ties.
  if (score > incumbent_score + kTieTolerance) {
    return true;
  }

  if (std::fabs(score - incumbent_score) <= kTieTolerance &&
      allocation_count < incumbent_allocation_count) {
    return true;
  }

  return false;
}

std::uint32_t stable_rollout_seed(
    const std::uint32_t base_seed,
    const int sample_index,
    const int salt) {
  // Derive reproducible but decorrelated RNG streams from one shared base
  // seed. The sample index keeps CRN pairing aligned across arms, while the
  // salt distinguishes different candidate/task streams when CRN is off.
  std::uint32_t x = base_seed ^ static_cast<std::uint32_t>(sample_index * 0x9e3779b9U);
  x ^= static_cast<std::uint32_t>(salt * 0x7f4a7c15U);
  x ^= x >> 16;
  x *= 0x85ebca6bU;
  x ^= x >> 13;
  x *= 0xc2b2ae35U;
  x ^= x >> 16;
  return x;
}

const std::vector<DiceRoll>& unique_unordered_rolls() {
  // Stratified dice schedules cycle over the 21 unordered roll outcomes rather
  // than the 36 ordered outcomes because the root decision problem is already
  // conditioned on one realized roll.
  static const std::vector<DiceRoll> outcomes = []() {
    std::vector<DiceRoll> out;
    out.reserve(21);
    for (int die1 = kMinDieValue; die1 <= kMaxDieValue; ++die1) {
      for (int die2 = die1; die2 <= kMaxDieValue; ++die2) {
        out.push_back(make_roll(die1, die2));
      }
    }
    return out;
  }();
  return outcomes;
}

ForcedRollSchedule scheduled_forced_rolls(
    const std::string& dice_mode,
    const int sample_index,
    const int offset) {
  // Build the optional deterministic roll prefix used for stratified-first-roll
  // and stratified-first-two-roll designs. The returned schedule is empty under
  // ordinary IID continuation play.
  ForcedRollSchedule schedule;

  if (dice_mode == "iid") {
    return schedule;
  }

  const std::vector<DiceRoll>& outcomes = unique_unordered_rolls();
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

  throw std::range_error("Unsupported dice stratification mode.");
}

RolloutOutcome outcome_from_turn_result(
    const TurnResult& turn_result,
    const int acting_player) {
  if (!turn_result.game_over) {
    return RolloutOutcome::kUnresolved;
  }
  return turn_result.winner == acting_player ? RolloutOutcome::kWin : RolloutOutcome::kLoss;
}

BoardStateKey board_state_key(const BoardState& board) {
  // Collapse-equivalence is purely about successor-board identity, so the key
  // stores only the board snapshot and player to move.
  BoardStateKey key;
  key.points = board.points;
  key.bar = board.bar;
  key.off = board.off;
  key.turn = board.turn;
  return key;
}

std::vector<CollapsedCandidate> collapse_equivalent_candidates(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves) {
  // Multiple legal root sequences can land on the same successor board. The
  // allocation engine samples successor states, not syntax-level move aliases,
  // so collapse them once here and keep one representative index plus the
  // multiplicity for R-side reporting.
  std::vector<CollapsedCandidate> collapsed;
  collapsed.reserve(legal_moves.size());
  std::unordered_map<BoardStateKey, int, BoardStateKeyHash> key_to_collapsed_index;
  key_to_collapsed_index.reserve(legal_moves.size());

  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    const BoardState board_after =
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

void compute_posterior_diagnostics(
    std::vector<ActionEvaluationSummary>& summaries,
    std::mt19937& rng) {
  // These diagnostics are intentionally approximate and Beta-based because the
  // fast scalar engine only tracks alpha/beta-style sufficient statistics.
  // They are used for method diagnostics, not as proof-style posterior
  // guarantees.
  const int n = static_cast<int>(summaries.size());
  if (n == 0 || kPosteriorDiagnosticDraws <= 0) {
    return;
  }

  arma::vec draw(n);
  arma::vec best_count(n, arma::fill::zeros);
  arma::vec regret_sum(n, arma::fill::zeros);

  for (int draw_idx = 0; draw_idx < kPosteriorDiagnosticDraws; ++draw_idx) {
    int best_index = 0;
    double best_value = -std::numeric_limits<double>::infinity();

    for (int i = 0; i < n; ++i) {
      draw[i] = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
      if (draw[i] > best_value) {
        best_value = draw[i];
        best_index = i;
      }
    }

    best_count[best_index] += 1.0;
    regret_sum += (best_value - draw);
  }

  const double denom = static_cast<double>(kPosteriorDiagnosticDraws);
  for (int i = 0; i < n; ++i) {
    summaries[i].prob_best = best_count[i] / denom;
    summaries[i].posterior_expected_regret = regret_sum[i] / denom;
  }
}

void refresh_summary_fields(
    std::vector<ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const RolloutConfig& config,
    const int total_allocations) {
  // Keep all posterior-moment refresh logic in one place so every policy sees
  // the same estimate / interval / score semantics before choosing the next
  // arm or exporting a final table.
  const int n = static_cast<int>(summaries.size());
  if (n == 0) {
    return;
  }

  arma::vec counts(n);
  arma::vec wins(n);
  arma::vec unresolved(n);
  arma::vec alpha(n);
  arma::vec beta(n);

  for (int i = 0; i < n; ++i) {
    counts[i] = static_cast<double>(summaries[i].allocation_count);
    wins[i] = static_cast<double>(summaries[i].wins);
    unresolved[i] = static_cast<double>(summaries[i].unresolved);
    alpha[i] = summaries[i].alpha;
    beta[i] = summaries[i].beta;
  }

  const arma::vec posterior_mean = alpha / (alpha + beta);
  const arma::vec posterior_var =
      (alpha % beta) / (arma::square(alpha + beta) % (alpha + beta + 1.0));
  const arma::vec posterior_sd = arma::sqrt(arma::clamp(posterior_var, 0.0, 1.0));
  const arma::vec lower_95 = arma::clamp(posterior_mean - 1.96 * posterior_sd, 0.0, 1.0);
  const arma::vec upper_95 = arma::clamp(posterior_mean + 1.96 * posterior_sd, 0.0, 1.0);

  for (int i = 0; i < n; ++i) {
    if (summaries[i].allocation_count > 0) {
      summaries[i].empirical_value =
          (wins[i] + config.unresolved_value * unresolved[i]) /
          counts[i];
    } else {
      summaries[i].empirical_value = NA_REAL;
    }

    summaries[i].estimate = posterior_mean[i];
    summaries[i].posterior_sd = posterior_sd[i];
    summaries[i].lower_95 = lower_95[i];
    summaries[i].upper_95 = upper_95[i];
    summaries[i].selection_score = posterior_mean[i];
  }

  if (policy == AllocationPolicy::kUcb) {
    // UCB is the only policy whose selection score intentionally differs from
    // posterior mean. The rest use posterior mean as the exported score.
    const double total = static_cast<double>(std::max(total_allocations, 1));
    const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    const arma::vec bonus = config.ucb_exploration *
        arma::sqrt(std::log(total + 1.0) / denom);

    for (int i = 0; i < n; ++i) {
      summaries[i].selection_score = posterior_mean[i] + bonus[i];
    }
  }
}

void finalize_summaries(
    std::vector<ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const RolloutConfig& config,
    std::mt19937& rng) {
  int total_allocations = 0;
  for (const ActionEvaluationSummary& summary : summaries) {
    total_allocations += summary.allocation_count;
  }
  refresh_summary_fields(summaries, policy, config, total_allocations);

  if (!config.fast_diagnostics) {
    compute_posterior_diagnostics(summaries, rng);
    return;
  }

  for (ActionEvaluationSummary& summary : summaries) {
    summary.prob_best = NA_REAL;
    summary.posterior_expected_regret = NA_REAL;
  }
}

int choose_next_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const int step,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // One dispatcher keeps the single shared rollout loop below policy-agnostic.
  if (policy == AllocationPolicy::kEqual) {
    return choose_equal_candidate(static_cast<int>(summaries.size()), step);
  }
  if (policy == AllocationPolicy::kGreedy) {
    return choose_greedy_candidate(summaries);
  }
  if (policy == AllocationPolicy::kUcb) {
    return choose_ucb_candidate(summaries, step, config);
  }
  if (policy == AllocationPolicy::kThompson) {
    return choose_thompson_candidate(summaries, rng);
  }
  if (policy == AllocationPolicy::kTtts) {
    return choose_ttts_candidate(summaries, config, rng);
  }
  if (policy == AllocationPolicy::kOcba) {
    return choose_ocba_candidate(summaries, step);
  }

  throw std::range_error("Unsupported allocation policy.");
}

std::vector<ActionEvaluationSummary> evaluate_with_optional_trace(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng,
    const int trace_every,
    std::vector<AllocationTraceRow>* trace_rows) {
  validate_rollout_config(config);
  if (trace_rows != nullptr && trace_every < 1) {
    throw std::range_error("`trace_every` must be at least 1.");
  }

  const std::string canonical_method = canonicalize_allocation_method(method);
  const AllocationPolicy policy = parse_allocation_policy(canonical_method);

  if (legal_moves.empty()) {
    throw std::range_error("Cannot evaluate an empty legal-move set.");
  }

  const std::vector<CollapsedCandidate> collapsed =
      collapse_equivalent_candidates(board, legal_moves);
  // From this point onward the engine works at the collapsed-board level. That
  // avoids wasting simulation budget on syntactically different move sequences
  // that land on the same successor state.

  std::vector<ActionEvaluationSummary> summaries(collapsed.size());
  std::vector<BoardState> candidate_boards;
  candidate_boards.reserve(collapsed.size());
  std::vector<int> acting_players;
  acting_players.reserve(collapsed.size());
  std::vector<int> stratification_offsets(collapsed.size(), 0);

  for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
    // Initialize every candidate from the declared prior so adaptive policies
    // start from a symmetric posterior before data arrive.
    candidate_boards.push_back(collapsed[i].board_after);
    acting_players.push_back(collapsed[i].acting_player);
    summaries[i].candidate_index = collapsed[i].representative_index + 1;
    summaries[i].n_equivalent_sequences = collapsed[i].n_equivalent;
    summaries[i].alpha = config.prior_alpha;
    summaries[i].beta = config.prior_beta;
    summaries[i].estimate = config.prior_alpha / (config.prior_alpha + config.prior_beta);
    summaries[i].selection_score = summaries[i].estimate;
  }

  if (config.dice_mode != "iid" && !config.crn) {
    // Without CRN, each candidate gets its own random offset into the
    // stratified-roll schedule. This preserves stratification while avoiding
    // unintended coupling across candidates.
    const int n_outcomes = config.dice_mode == "stratified_first_two_rolls" ? 441 : 21;
    std::uniform_int_distribution<int> offset_dist(0, n_outcomes - 1);
    for (int i = 0; i < static_cast<int>(stratification_offsets.size()); ++i) {
      stratification_offsets[i] = offset_dist(rng);
    }
  }

  const std::uint32_t crn_base_seed = config.use_crn_seed
      ? static_cast<std::uint32_t>(config.crn_seed)
      : static_cast<std::uint32_t>(rng());
  // The CRN base seed is fixed once per run so sample index k means the same
  // synchronized continuation stream for every candidate when CRN is enabled.

  int step = 0;
  auto maybe_trace = [&](const int selected_candidate) {
    // Trace snapshots are appended after the latest update so checkpoint rows
    // reflect the state the next allocation decision will see.
    if (trace_rows == nullptr) {
      return;
    }
    if (step % trace_every == 0 || step == config.budget) {
      append_trace_snapshot(*trace_rows, summaries, policy, config, step, selected_candidate);
    }
  };

  if (policy != AllocationPolicy::kEqual && config.initial_allocations > 0) {
    // Adaptive policies get a small symmetric warm start so Thompson/UCB/OCBA
    // do not begin from completely unobserved arms.
    for (int round = 0; round < config.initial_allocations && step < config.budget; ++round) {
      for (int i = 0; i < static_cast<int>(candidate_boards.size()) && step < config.budget; ++i) {
        const int sample_index = summaries[i].allocation_count + 1;
        const int offset = config.crn ? 0 : stratification_offsets[i];
        const ForcedRollSchedule forced_rolls =
            scheduled_forced_rolls(config.dice_mode, sample_index, offset);
        std::mt19937* rollout_rng = &rng;
        std::mt19937 crn_rng;
        if (config.crn) {
          // Under CRN, the same sample index across candidates is replayed from
          // the same base seed so between-arm differences are paired.
          crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
          rollout_rng = &crn_rng;
        }
        const RolloutOutcome outcome = single_rollout_outcome(
            candidate_boards[i],
            acting_players[i],
            config,
            *rollout_rng,
            forced_rolls);
        // Warm-start samples update exactly the same sufficient statistics as
        // later adaptive samples; they only differ in how the arm was chosen.
        update_summary(summaries[i], outcome, config);
        step += 1;
        maybe_trace(summaries[i].candidate_index);
      }
    }
  }

  while (step < config.budget) {
    // The main fixed-budget loop is deliberately shared across all policies.
    // Policy files choose the next candidate; everything else here is common
    // rollout execution, variance-control handling, and summary updates.
    const int chosen_index =
        choose_next_candidate(summaries, policy, step, config, rng);
    const int sample_index = summaries[chosen_index].allocation_count + 1;
    const int offset = config.crn ? 0 : stratification_offsets[chosen_index];
    const ForcedRollSchedule forced_rolls =
        scheduled_forced_rolls(config.dice_mode, sample_index, offset);
    std::mt19937* rollout_rng = &rng;
    std::mt19937 crn_rng;
    if (config.crn) {
      crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
      rollout_rng = &crn_rng;
    }
    const RolloutOutcome outcome = single_rollout_outcome(
        candidate_boards[chosen_index],
        acting_players[chosen_index],
        config,
        *rollout_rng,
        forced_rolls);
    // One chosen rollout becomes one sufficient-stat update for the selected
    // collapsed candidate, after which the loop re-evaluates selection scores.
    update_summary(summaries[chosen_index], outcome, config);
    step += 1;
    maybe_trace(summaries[chosen_index].candidate_index);
  }

  finalize_summaries(summaries, policy, config, rng);
  // Finalization computes posterior moments/intervals once more after the
  // budget is exhausted so the exported action table reflects the full run.
  return summaries;
}

}  // namespace allocation
}  // namespace backgammonr
