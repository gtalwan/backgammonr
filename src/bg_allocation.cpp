// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
// Core statistical-allocation engine.
//
// This file implements the finite-budget sampling logic used across methods:
// - equal allocation
// - greedy posterior-mean allocation
// - UCB-style allocation
// - Thompson sampling
// - top-two Thompson sampling (TTTS)
// - OCBA-style approximate allocation
//
// The same engine is reused by multiple wrappers so method comparisons differ
// by policy choice rather than by duplicated simulation code paths.

#include "bg_allocation.h"

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <limits>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

#include "bg_game.h"
#include "bg_movegen.h"
#include "bg_rules.h"

namespace {

// Outcome encoding used for rollout reward accounting.
enum class RolloutOutcome {
  kWin,
  kLoss,
  kUnresolved
};

// Internal policy enum for budget-allocation strategy.
enum class AllocationPolicy {
  kEqual,
  kGreedy,
  kUcb,
  kThompson,
  kTtts,
  kOcba
};

inline constexpr double kTieTolerance = 1e-12;
// Posterior diagnostics are Monte Carlo approximations and remain deterministic
// under a fixed seed because they use the same local RNG stream.
inline constexpr int kPosteriorDiagnosticDraws = 512;

struct CollapsedCandidate {
  // Board after applying representative legal sequence.
  backgammonr::BoardState board_after{};
  // 0-based index of representative move in original legal move list.
  int representative_index{0};
  // Player who made the candidate move.
  int acting_player{1};
  // Number of equivalent sequences collapsed into this state.
  int n_equivalent{1};
};

struct AllocationTraceRow {
  // Snapshot metadata.
  int checkpoint{0};
  int selected_candidate{NA_INTEGER};
  int leader_index{NA_INTEGER};
  // Candidate-level stats at this checkpoint.
  int candidate_index{0};
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int unresolved{0};
  double empirical_value{NA_REAL};
  double alpha{1.0};
  double beta{1.0};
  double estimate{0.5};
  double posterior_sd{0.0};
  double lower_95{0.0};
  double upper_95{1.0};
  double selection_score{0.5};
};

struct ForcedRollSchedule {
  // Up to two pre-scheduled rolls for stratified dice modes.
  std::array<backgammonr::DiceRoll, 2> rolls{};
  int n_rolls{0};
};

RolloutOutcome outcome_from_turn_result(
    const backgammonr::TurnResult& turn_result,
    const int acting_player);

backgammonr::BoardState apply_sequence_without_full_validation(
    const backgammonr::BoardState& board,
    const backgammonr::MoveSequence& sequence) {
  // Copy once, then apply sequence steps without re-validating each step.
  backgammonr::BoardState out = board;

  for (const backgammonr::MoveStep& step : sequence.steps) {
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  }
  out.turn = -sequence.player;
  return out;
}

// Lightweight random turn helper used inside rollout loop.
void play_random_turn_lightweight(
    backgammonr::BoardState& board,
    const backgammonr::DiceRoll& roll,
    std::mt19937& rng) {
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
}

std::mt19937 init_rng(const int seed, const bool use_seed) {
  std::mt19937 rng;

  if (use_seed) {
    if (seed < 0) {
      throw std::range_error("`seed` must be nonnegative when supplied.");
    }
    rng.seed(static_cast<std::uint32_t>(seed));
  } else {
    std::random_device rd;
    rng.seed(rd());
  }

  return rng;
}

double outcome_reward(const RolloutOutcome outcome, const backgammonr::RolloutConfig& config) {
  // Map terminal outcome to Bernoulli-style reward with unresolved fallback.
  if (outcome == RolloutOutcome::kWin) {
    return 1.0;
  }

  if (outcome == RolloutOutcome::kLoss) {
    return 0.0;
  }

  return config.unresolved_value;
}

RolloutOutcome single_rollout_outcome(
    const backgammonr::BoardState& board_after,
    const int acting_player,
    const backgammonr::RolloutConfig& config,
    std::mt19937& rng,
    const ForcedRollSchedule& forced_rolls) {
  // Candidate already ends game.
  if (backgammonr::board_is_terminal(board_after)) {
    return backgammonr::board_winner(board_after) == acting_player
        ? RolloutOutcome::kWin
        : RolloutOutcome::kLoss;
  }

  if (config.max_turns <= 0) {
    return RolloutOutcome::kUnresolved;
  }

  // Fast path for random-policy rollout.
  if (config.policy == "random") {
    backgammonr::BoardState current = board_after;
    int turns_remaining = config.max_turns;

    // Consume any forced stratification rolls first.
    for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
      if (turns_remaining <= 0) {
        return RolloutOutcome::kUnresolved;
      }

      play_random_turn_lightweight(current, forced_rolls.rolls[forced_idx], rng);
      if (backgammonr::board_is_terminal(current)) {
        return backgammonr::board_winner(current) == acting_player
            ? RolloutOutcome::kWin
            : RolloutOutcome::kLoss;
      }
      --turns_remaining;
    }

    for (int turn = 0; turn < turns_remaining; ++turn) {
      // Then continue with IID sampled rolls.
      play_random_turn_lightweight(current, backgammonr::roll_dice(rng), rng);
      if (backgammonr::board_is_terminal(current)) {
        return backgammonr::board_winner(current) == acting_player
            ? RolloutOutcome::kWin
            : RolloutOutcome::kLoss;
      }
    }

    return RolloutOutcome::kUnresolved;
  }

  backgammonr::BoardState current = board_after;
  int turns_remaining = config.max_turns;
  // Non-random policy path still honors forced-roll prefix.
  for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
    if (turns_remaining <= 0) {
      return RolloutOutcome::kUnresolved;
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
    return RolloutOutcome::kUnresolved;
  }
  return rollout_result.winner == acting_player
      ? RolloutOutcome::kWin
      : RolloutOutcome::kLoss;
}

void update_summary(
    backgammonr::ActionEvaluationSummary& summary,
    const RolloutOutcome outcome,
    const backgammonr::RolloutConfig& config) {
  // Track raw outcome counts.
  summary.allocation_count += 1;

  if (outcome == RolloutOutcome::kWin) {
    summary.wins += 1;
  } else if (outcome == RolloutOutcome::kLoss) {
    summary.losses += 1;
  } else {
    summary.unresolved += 1;
  }

  const double reward = outcome_reward(outcome, config);
  // Conjugate Beta-Bernoulli posterior update.
  summary.alpha += reward;
  summary.beta += (1.0 - reward);
}

double sample_beta_distribution(const double alpha, const double beta, std::mt19937& rng) {
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
  // Map canonical method string to internal switch enum.
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
  // Primary criterion: larger score.
  if (score > incumbent_score + kTieTolerance) {
    return true;
  }

  if (std::fabs(score - incumbent_score) <= kTieTolerance &&
      allocation_count < incumbent_allocation_count) {
    // Tie-break toward less-sampled candidate (encourages balance).
    return true;
  }

  return false;
}

std::uint32_t stable_rollout_seed(
    const std::uint32_t base_seed,
    const int sample_index,
    const int salt) {
  // Mix sample index and salt into a stable 32-bit seed.
  std::uint32_t x = base_seed ^ static_cast<std::uint32_t>(sample_index * 0x9e3779b9U);
  x ^= static_cast<std::uint32_t>(salt * 0x7f4a7c15U);
  x ^= x >> 16;
  x *= 0x85ebca6bU;
  x ^= x >> 13;
  x *= 0xc2b2ae35U;
  x ^= x >> 16;
  return x;
}

const std::vector<backgammonr::DiceRoll>& unique_unordered_rolls() {
  // Lazily initialize 21 unordered roll outcomes (1-1, 1-2, ..., 6-6).
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

ForcedRollSchedule scheduled_forced_rolls(
    const std::string& dice_mode,
    const int sample_index,
    const int offset) {
  ForcedRollSchedule schedule;

  if (dice_mode == "iid") {
    // No stratification; rollout draws rolls normally.
    return schedule;
  }

  const std::vector<backgammonr::DiceRoll>& outcomes = unique_unordered_rolls();
  const int n = static_cast<int>(outcomes.size());

  if (dice_mode == "stratified_first_roll") {
    // Deterministically cycle first roll through 21 unordered outcomes.
    const int idx = (offset + sample_index - 1) % n;
    schedule.rolls[0] = outcomes[idx];
    schedule.n_rolls = 1;
    return schedule;
  }

  if (dice_mode == "stratified_first_two_rolls") {
    // Deterministically cycle first two rolls through 21 x 21 combinations.
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
    const backgammonr::TurnResult& turn_result,
    const int acting_player) {
  if (!turn_result.game_over) {
    return RolloutOutcome::kUnresolved;
  }
  return turn_result.winner == acting_player ? RolloutOutcome::kWin : RolloutOutcome::kLoss;
}

struct BoardStateKey {
  // Hashable canonical board representation for candidate deduplication.
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
  // Copy board fields into fixed key struct for unordered_map lookup.
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
  // **WHAT IT'S DOING (DETAILED):** We canonicalize the action set by merging
  // different legal move sequences that end in the same resulting board state.
  // This avoids spending duplicate rollout budget on strategically identical
  // outcomes.
  // them as one option so simulation time is not wasted repeating equivalent work.
  // Collapse moves that lead to identical board states.
  std::vector<CollapsedCandidate> collapsed;
  collapsed.reserve(legal_moves.size());
  std::unordered_map<BoardStateKey, int, BoardStateKeyHash> key_to_collapsed_index;
  key_to_collapsed_index.reserve(legal_moves.size());

  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    // Apply move once and hash resulting board state.
    const backgammonr::BoardState board_after =
        apply_sequence_without_full_validation(board, legal_moves[i]);
    const BoardStateKey key = board_state_key(board_after);
    const auto it = key_to_collapsed_index.find(key);

    if (it == key_to_collapsed_index.end()) {
      // First time this board-after appears.
      CollapsedCandidate row;
      row.board_after = board_after;
      row.representative_index = i;
      row.acting_player = legal_moves[i].player;
      row.n_equivalent = 1;
      key_to_collapsed_index.emplace(key, static_cast<int>(collapsed.size()));
      collapsed.push_back(row);
    } else {
      // Equivalent board state: increase multiplicity only.
      collapsed[it->second].n_equivalent += 1;
    }
  }

  return collapsed;
}

void compute_posterior_diagnostics(
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Approximates two Bayesian diagnostics from
  // posterior draws:
  // 1) `prob_best`: how often each candidate wins a posterior draw tournament.
  // 2) `posterior_expected_regret`: average gap to the sampled best value.
  // current uncertainty, then count how often each move looks best and how much
  // value is lost if we picked a non-best move in those hypothetical worlds.
  // Monte Carlo posterior diagnostics (probability best + expected regret).
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
      // Draw one posterior sample per candidate.
      draw[i] = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
      if (draw[i] > best_value) {
        best_value = draw[i];
        best_index = i;
      }
    }

    best_count[best_index] += 1.0;
    // The vector subtraction is element-wise:
    // `best_value - draw[i]` is the regret of candidate i in this posterior world.
    // Averaging this across draws gives a Bayes-style simple regret diagnostic.
    // Bayes simple regret for each candidate under this posterior draw.
    regret_sum += (best_value - draw);
  }

  const double denom = static_cast<double>(kPosteriorDiagnosticDraws);
  for (int i = 0; i < n; ++i) {
    summaries[i].prob_best = best_count[i] / denom;
    summaries[i].posterior_expected_regret = regret_sum[i] / denom;
  }
}

void finalize_summaries(
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const backgammonr::RolloutConfig& config,
    std::mt19937& rng) {
  // Convert integer counts and Beta parameters into vectorized Armadillo arrays.
  const int n = static_cast<int>(summaries.size());
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
    // Empirical value uses unresolved_value for unresolved outcomes.
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
    // Selection score for UCB includes exploration bonus.
    const double total_allocations = arma::accu(counts);
    const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    const arma::vec bonus = config.ucb_exploration *
        arma::sqrt(std::log(total_allocations + 1.0) / denom);

    for (int i = 0; i < n; ++i) {
      summaries[i].selection_score = posterior_mean[i] + bonus[i];
    }
  }

  if (!config.fast_diagnostics) {
    // Full output mode: compute posterior diagnostics.
    compute_posterior_diagnostics(summaries, rng);
    return;
  }

  for (int i = 0; i < n; ++i) {
    // Fast mode: skip expensive diagnostics.
    summaries[i].prob_best = NA_REAL;
    summaries[i].posterior_expected_regret = NA_REAL;
  }
}

void update_interim_summary_fields(
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const backgammonr::RolloutConfig& config,
    const int total_allocations) {
  // Cheaper variant used for trace snapshots during allocation loop.
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
    const double total = static_cast<double>(std::max(total_allocations, 1));
    const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    const arma::vec bonus = config.ucb_exploration *
        arma::sqrt(std::log(total + 1.0) / denom);

    for (int i = 0; i < n; ++i) {
      summaries[i].selection_score = posterior_mean[i] + bonus[i];
    }
  }
}

int current_leader_index(const std::vector<backgammonr::ActionEvaluationSummary>& summaries) {
  // Pick current leader by estimate, tie-break by higher sample count.
  if (summaries.empty()) {
    return NA_INTEGER;
  }

  int leader = 0;
  for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
    if (summaries[i].estimate > summaries[leader].estimate + kTieTolerance) {
      leader = i;
      continue;
    }

    if (std::fabs(summaries[i].estimate - summaries[leader].estimate) <= kTieTolerance &&
        summaries[i].allocation_count > summaries[leader].allocation_count) {
      leader = i;
    }
  }

  return leader;
}

void append_trace_snapshot(
    std::vector<AllocationTraceRow>& trace_rows,
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const backgammonr::RolloutConfig& config,
    const int checkpoint,
    const int selected_candidate) {
  // Refresh per-candidate fields as of this checkpoint.
  update_interim_summary_fields(summaries, policy, config, checkpoint);
  const int leader_pos = current_leader_index(summaries);
  const int leader_index = leader_pos == NA_INTEGER
      ? NA_INTEGER
      : summaries[leader_pos].candidate_index;

  for (const backgammonr::ActionEvaluationSummary& summary : summaries) {
    // Emit one trace row per candidate at this checkpoint.
    AllocationTraceRow row;
    row.checkpoint = checkpoint;
    row.selected_candidate = selected_candidate;
    row.leader_index = leader_index;
    row.candidate_index = summary.candidate_index;
    row.allocation_count = summary.allocation_count;
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    row.empirical_value = summary.empirical_value;
    row.alpha = summary.alpha;
    row.beta = summary.beta;
    row.estimate = summary.estimate;
    row.posterior_sd = summary.posterior_sd;
    row.lower_95 = summary.lower_95;
    row.upper_95 = summary.upper_95;
    row.selection_score = summary.selection_score;
    trace_rows.push_back(row);
  }
}

arma::vec ocba_target_allocations(
    const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const int next_total_allocations) {
  // **WHAT IT'S DOING (DETAILED):** Computes a continuous OCBA-inspired target
  // allocation profile from posterior means and posterior standard deviations.
  // Arms with smaller mean gaps and/or larger uncertainty get larger target mass.
  // and still uncertain, because those are the ones that can still change the
  // final recommendation.
  // OCBA target-allocation approximation from posterior means/variances.
  const int n = static_cast<int>(summaries.size());
  arma::vec target(n, arma::fill::zeros);
  if (n == 0) {
    return target;
  }
  if (n == 1) {
    target[0] = static_cast<double>(next_total_allocations);
    return target;
  }

  arma::vec mu(n);
  arma::vec sigma(n);
  for (int i = 0; i < n; ++i) {
    const double alpha = summaries[i].alpha;
    const double beta = summaries[i].beta;
    mu[i] = alpha / (alpha + beta);
    const double var = (alpha * beta) /
        ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0));
    sigma[i] = std::sqrt(std::max(var, 1e-12));
  }

  int best = 0;
  for (int i = 1; i < n; ++i) {
    if (mu[i] > mu[best]) {
      best = i;
    }
  }

  arma::vec ratio(n, arma::fill::zeros);
  const double mu_best = mu[best];
  for (int i = 0; i < n; ++i) {
    if (i == best) {
      continue;
    }
    const double gap = std::max(mu_best - mu[i], 1e-6);
    ratio[i] = (sigma[i] * sigma[i]) / (gap * gap);
  }

  double sum_term = 0.0;
  for (int i = 0; i < n; ++i) {
    if (i == best) {
      continue;
    }
    sum_term += (ratio[i] * ratio[i]) / std::max(sigma[i] * sigma[i], 1e-12);
  }
  ratio[best] = std::max(sigma[best] * std::sqrt(std::max(sum_term, 1e-12)), 1e-12);

  for (int i = 0; i < n; ++i) {
    ratio[i] = std::max(ratio[i], 1e-12);
  }

  const double ratio_sum = arma::accu(ratio);
  if (ratio_sum <= 0.0 || !std::isfinite(ratio_sum)) {
    target.fill(static_cast<double>(next_total_allocations) / static_cast<double>(n));
    return target;
  }

  target = ratio / ratio_sum * static_cast<double>(next_total_allocations);
  return target;
}

int choose_next_candidate(
    const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const int step,
    const backgammonr::RolloutConfig& config,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Central policy switch for one-step budget
  // allocation. Given the current posterior state, this chooses exactly one
  // candidate to receive the next rollout sample.
  // Different methods answer that question differently (equal, UCB, Thompson,
  // TTTS, OCBA), but they all pass through this function.
  // Policy-specific next-arm selection in a fixed-budget simulation problem.
  const int n = static_cast<int>(summaries.size());

  if (policy == AllocationPolicy::kEqual) {
    // Deterministic round-robin.
    return n == 0 ? -1 : (step % n);
  }

  if (n == 0) {
    throw std::range_error("Cannot choose from an empty candidate set.");
  }

  int best_index = 0;
  double best_score = -std::numeric_limits<double>::infinity();
  int best_allocations = std::numeric_limits<int>::max();
  const double ucb_log_term = std::log(static_cast<double>(step) + 2.0);

  if (policy == AllocationPolicy::kOcba) {
    // Allocate toward largest deficit from OCBA target allocations.
    const arma::vec target = ocba_target_allocations(summaries, step + 1);
    int chosen = 0;
    double best_deficit = target[0] - static_cast<double>(summaries[0].allocation_count);
    for (int i = 1; i < n; ++i) {
      const double deficit = target[i] - static_cast<double>(summaries[i].allocation_count);
      if (deficit > best_deficit + kTieTolerance) {
        chosen = i;
        best_deficit = deficit;
        continue;
      }
      if (std::fabs(deficit - best_deficit) <= kTieTolerance &&
          summaries[i].allocation_count < summaries[chosen].allocation_count) {
        chosen = i;
        best_deficit = deficit;
      }
    }
    return chosen;
  }

  if (policy == AllocationPolicy::kTtts) {
    // **WHAT IT'S DOING (DETAILED):** Top-Two Thompson Sampling (TTTS):
    // sample a first winner I, then with probability beta play I, otherwise
    // sample until we get a distinct winner J and play J.
    // under posterior uncertainty so we do not over-commit too early.
    // Top-Two Thompson Sampling:
    // 1) draw posterior sample and pick top action I,
    // 2) with probability beta play I,
    // 3) otherwise draw again until top action J != I and play J.
    auto draw_thompson_winner = [&](void) -> int {
      int winner = 0;
      double best = -std::numeric_limits<double>::infinity();
      for (int i = 0; i < n; ++i) {
        const double draw = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
        if (draw > best) {
          best = draw;
          winner = i;
        }
      }
      return winner;
    };

    const int top1 = draw_thompson_winner();
    if (n == 1) {
      return top1;
    }

    double beta = config.ucb_exploration;
    if (!(beta > 0.0 && beta <= 1.0) || !std::isfinite(beta)) {
      beta = 0.5;
    }

    std::uniform_real_distribution<double> coin(0.0, 1.0);
    if (coin(rng) <= beta) {
      return top1;
    }

    for (int attempt = 0; attempt < 64; ++attempt) {
      const int top2 = draw_thompson_winner();
      if (top2 != top1) {
        return top2;
      }
    }

    // Rare fallback if repeated posterior draws tie to the same winner.
    int fallback = -1;
    double best_mean = -std::numeric_limits<double>::infinity();
    for (int i = 0; i < n; ++i) {
      if (i == top1) {
        continue;
      }
      const double mean = summaries[i].alpha / (summaries[i].alpha + summaries[i].beta);
      if (mean > best_mean) {
        best_mean = mean;
        fallback = i;
      }
    }
    return fallback >= 0 ? fallback : top1;
  }

  for (int i = 0; i < n; ++i) {
    const backgammonr::ActionEvaluationSummary& summary = summaries[i];
    const double posterior_mean = summary.alpha / (summary.alpha + summary.beta);
    double score = posterior_mean;

    if (policy == AllocationPolicy::kUcb) {
      // UCB score = posterior mean + exploration bonus.
      const double denom = static_cast<double>(std::max(summary.allocation_count, 1));
      score += config.ucb_exploration * std::sqrt(ucb_log_term / denom);
    } else if (policy == AllocationPolicy::kThompson) {
      // Thompson score = one posterior sample draw.
      score = sample_beta_distribution(summary.alpha, summary.beta, rng);
    }

    if (score_beats_incumbent(score, summary.allocation_count, best_score, best_allocations)) {
      best_index = i;
      best_score = score;
      best_allocations = summary.allocation_count;
    }
  }

  return best_index;
}

std::vector<backgammonr::ActionEvaluationSummary> evaluate_with_optional_trace(
    const backgammonr::BoardState& board,
    const std::vector<backgammonr::MoveSequence>& legal_moves,
    const std::string& method,
    const backgammonr::RolloutConfig& config,
    std::mt19937& rng,
    const int trace_every,
    std::vector<AllocationTraceRow>* trace_rows) {
  // **WHAT IT'S DOING (DETAILED):** End-to-end fixed-budget evaluator with
  // optional trace logging.
  // Phase A: validate config and canonicalize method.
  // Phase B: collapse equivalent actions and initialize posterior summaries.
  // Phase C: optional warm-start allocations for adaptive methods.
  // Phase D: main adaptive allocation loop until budget is exhausted.
  // Phase E: finalize posterior summaries/diagnostics and return.
  // limited simulation budget and records how estimates and uncertainty evolve.
  // Core allocation engine used by all public wrappers.
  backgammonr::validate_rollout_config(config);
  if (trace_rows != nullptr && trace_every < 1) {
    throw std::range_error("`trace_every` must be at least 1.");
  }

  const std::string canonical_method = backgammonr::canonicalize_allocation_method(method);
  const AllocationPolicy policy = parse_allocation_policy(canonical_method);

  if (legal_moves.empty()) {
    throw std::range_error("Cannot evaluate an empty legal-move set.");
  }

  const std::vector<CollapsedCandidate> collapsed =
      collapse_equivalent_candidates(board, legal_moves);

  std::vector<backgammonr::ActionEvaluationSummary> summaries(collapsed.size());
  std::vector<backgammonr::BoardState> candidate_boards;
  candidate_boards.reserve(collapsed.size());
  std::vector<int> acting_players;
  acting_players.reserve(collapsed.size());
  std::vector<int> stratification_offsets(collapsed.size(), 0);

  for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
    // Initialize one summary row per collapsed candidate state.
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
    // Randomize candidate-specific stratification offsets when not using CRN.
    const int n_outcomes = config.dice_mode == "stratified_first_two_rolls" ? 441 : 21;
    std::uniform_int_distribution<int> offset_dist(0, n_outcomes - 1);
    for (int i = 0; i < static_cast<int>(stratification_offsets.size()); ++i) {
      stratification_offsets[i] = offset_dist(rng);
    }
  }

  const std::uint32_t crn_base_seed = config.use_crn_seed
      ? static_cast<std::uint32_t>(config.crn_seed)
      : static_cast<std::uint32_t>(rng());

  int step = 0;
  // Snapshot helper: emit checkpoints only when requested.
  auto maybe_trace = [&](const int selected_candidate) {
    if (trace_rows == nullptr) {
      return;
    }
    if (step % trace_every == 0 || step == config.budget) {
      append_trace_snapshot(*trace_rows, summaries, policy, config, step, selected_candidate);
    }
  };

  if (policy != AllocationPolicy::kEqual && config.initial_allocations > 0) {
    // Warm-start adaptive methods with round-robin initial allocations.
    for (int round = 0; round < config.initial_allocations && step < config.budget; ++round) {
      for (int i = 0; i < static_cast<int>(candidate_boards.size()) && step < config.budget; ++i) {
        const int sample_index = summaries[i].allocation_count + 1;
        const int offset = config.crn ? 0 : stratification_offsets[i];
        const ForcedRollSchedule forced_rolls =
            scheduled_forced_rolls(config.dice_mode, sample_index, offset);
        std::mt19937* rollout_rng = &rng;
        std::mt19937 crn_rng;
        if (config.crn) {
          // Re-seed rollout RNG deterministically per sample index.
          crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
          rollout_rng = &crn_rng;
        }
        const RolloutOutcome outcome = single_rollout_outcome(
            candidate_boards[i],
            acting_players[i],
            config,
            *rollout_rng,
            forced_rolls);
        update_summary(summaries[i], outcome, config);
        step += 1;
        maybe_trace(summaries[i].candidate_index);
      }
    }
  }

  while (step < config.budget) {
    // Each iteration consumes exactly one rollout from the remaining budget.
    // The selected index comes from the chosen allocation policy and current
    // posterior state.
    // Policy decides which candidate gets next rollout.
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
    update_summary(summaries[chosen_index], outcome, config);
    step += 1;
    maybe_trace(summaries[chosen_index].candidate_index);
  }

  finalize_summaries(summaries, policy, config, rng);
  return summaries;
}

}  // namespace

namespace backgammonr {

// Supported public method identifiers (including compatibility aliases).
bool is_supported_allocation_method(const std::string& method) {
  return method == "equal" ||
      method == "greedy" ||
      method == "ucb" ||
      method == "ocba" ||
      method == "thompson" ||
      method == "ttts" ||
      method == "rollout" ||
      method == "equal_rollout" ||
      method == "greedy_rollout" ||
      method == "ucb_rollout" ||
      method == "ocba_rollout" ||
      method == "thompson_rollout" ||
      method == "ttts_rollout";
}

void validate_allocation_method(const std::string& method) {
  if (!is_supported_allocation_method(method)) {
    throw std::range_error(
        "`method` must be one of \"equal\", \"greedy\", \"ucb\", \"ocba\", \"thompson\", \"ttts\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"ocba_rollout\", \"thompson_rollout\", or \"ttts_rollout\".");
  }
}

std::string canonicalize_allocation_method(const std::string& method) {
  // Canonicalize aliases so downstream switches only handle one spelling.
  validate_allocation_method(method);

  if (method == "equal" || method == "rollout" || method == "equal_rollout") {
    return "equal";
  }

  if (method == "greedy" || method == "greedy_rollout") {
    return "greedy";
  }

  if (method == "ucb" || method == "ucb_rollout") {
    return "ucb";
  }
  if (method == "ocba" || method == "ocba_rollout") {
    return "ocba";
  }
  if (method == "ttts" || method == "ttts_rollout") {
    return "ttts";
  }

  return "thompson";
}

std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Public entry: evaluate without trace.
  return evaluate_with_optional_trace(
      board,
      legal_moves,
      method,
      config,
      rng,
      1,
      nullptr);
}

int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries) {
  // Pick best by posterior estimate, with deterministic tie-breaks.
  if (summaries.empty()) {
    throw std::range_error("Cannot determine a best candidate from an empty summary set.");
  }

  int best_index = 0;
  for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
    if (summaries[i].estimate > summaries[best_index].estimate + 1e-12) {
      best_index = i;
      continue;
    }

    if (std::fabs(summaries[i].estimate - summaries[best_index].estimate) <= 1e-12) {
      const double current_empirical = Rcpp::NumericVector::is_na(summaries[i].empirical_value)
          ? -std::numeric_limits<double>::infinity()
          : summaries[i].empirical_value;
      const double best_empirical = Rcpp::NumericVector::is_na(summaries[best_index].empirical_value)
          ? -std::numeric_limits<double>::infinity()
          : summaries[best_index].empirical_value;

      if (current_empirical > best_empirical + 1e-12) {
        best_index = i;
        continue;
      }

      if (std::fabs(current_empirical - best_empirical) <= 1e-12 &&
          summaries[i].allocation_count > summaries[best_index].allocation_count) {
        best_index = i;
      }
    }
  }

  return best_index;
}

MoveSequence choose_move_sequence_with_allocation(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng) {
  if (legal_moves.empty()) {
    throw std::range_error("Cannot choose from an empty legal-move set.");
  }

  if (legal_moves.size() == 1U) {
    return legal_moves.front();
  }

  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, method, config, rng);

  const int best_summary_index = best_candidate_index(summaries);
  const int representative_move_index = summaries[best_summary_index].candidate_index - 1;
  if (representative_move_index < 0 ||
      representative_move_index >= static_cast<int>(legal_moves.size())) {
    throw std::range_error("Internal error: recommended move index is out of range.");
  }

  return legal_moves[representative_move_index];
}

Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
    const std::vector<ActionEvaluationSummary>& summaries) {
  // Columnar conversion for R-side data-frame consumption.
  const int n = static_cast<int>(summaries.size());
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector n_equivalent_sequences(n);
  Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector empirical_value(n);
  Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector beta(n);
  Rcpp::NumericVector estimate(n);
  Rcpp::NumericVector posterior_sd(n);
  Rcpp::NumericVector lower_95(n);
  Rcpp::NumericVector upper_95(n);
  Rcpp::NumericVector prob_best(n);
  Rcpp::NumericVector posterior_expected_regret(n);
  Rcpp::NumericVector selection_score(n);

  for (int i = 0; i < n; ++i) {
    candidate_index[i] = summaries[i].candidate_index;
    n_equivalent_sequences[i] = summaries[i].n_equivalent_sequences;
    allocation_count[i] = summaries[i].allocation_count;
    wins[i] = summaries[i].wins;
    losses[i] = summaries[i].losses;
    unresolved[i] = summaries[i].unresolved;
    empirical_value[i] = summaries[i].empirical_value;
    alpha[i] = summaries[i].alpha;
    beta[i] = summaries[i].beta;
    estimate[i] = summaries[i].estimate;
    posterior_sd[i] = summaries[i].posterior_sd;
    lower_95[i] = summaries[i].lower_95;
    upper_95[i] = summaries[i].upper_95;
    prob_best[i] = summaries[i].prob_best;
    posterior_expected_regret[i] = summaries[i].posterior_expected_regret;
    selection_score[i] = summaries[i].selection_score;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
      Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["empirical_value"] = empirical_value,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta,
      Rcpp::_["estimate"] = estimate,
      Rcpp::_["posterior_sd"] = posterior_sd,
      Rcpp::_["lower_95"] = lower_95,
      Rcpp::_["upper_95"] = upper_95,
      Rcpp::_["prob_best"] = prob_best,
      Rcpp::_["posterior_expected_regret"] = posterior_expected_regret,
      Rcpp::_["selection_score"] = selection_score,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace backgammonr

Rcpp::DataFrame allocation_trace_rows_to_data_frame(
    const std::vector<AllocationTraceRow>& trace_rows) {
  // Columnar conversion for optional trace output.
  const int n = static_cast<int>(trace_rows.size());
  Rcpp::IntegerVector checkpoint(n);
  Rcpp::IntegerVector selected_candidate(n);
  Rcpp::IntegerVector leader_index(n);
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector empirical_value(n);
  Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector beta(n);
  Rcpp::NumericVector estimate(n);
  Rcpp::NumericVector posterior_sd(n);
  Rcpp::NumericVector lower_95(n);
  Rcpp::NumericVector upper_95(n);
  Rcpp::NumericVector selection_score(n);

  for (int i = 0; i < n; ++i) {
    checkpoint[i] = trace_rows[i].checkpoint;
    selected_candidate[i] = trace_rows[i].selected_candidate;
    leader_index[i] = trace_rows[i].leader_index;
    candidate_index[i] = trace_rows[i].candidate_index;
    allocation_count[i] = trace_rows[i].allocation_count;
    wins[i] = trace_rows[i].wins;
    losses[i] = trace_rows[i].losses;
    unresolved[i] = trace_rows[i].unresolved;
    empirical_value[i] = trace_rows[i].empirical_value;
    alpha[i] = trace_rows[i].alpha;
    beta[i] = trace_rows[i].beta;
    estimate[i] = trace_rows[i].estimate;
    posterior_sd[i] = trace_rows[i].posterior_sd;
    lower_95[i] = trace_rows[i].lower_95;
    upper_95[i] = trace_rows[i].upper_95;
    selection_score[i] = trace_rows[i].selection_score;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["checkpoint"] = checkpoint,
      Rcpp::_["selected_candidate"] = selected_candidate,
      Rcpp::_["leader_index"] = leader_index,
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["empirical_value"] = empirical_value,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta,
      Rcpp::_["estimate"] = estimate,
      Rcpp::_["posterior_sd"] = posterior_sd,
      Rcpp::_["lower_95"] = lower_95,
      Rcpp::_["upper_95"] = upper_95,
      Rcpp::_["selection_score"] = selection_score,
      Rcpp::_["stringsAsFactors"] = false);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_allocation_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& method,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const double unresolved_value,
    const int initial_allocations,
    const double ucb_exploration,
    const double prior_alpha,
    const double prior_beta,
    const std::string& dice_mode,
    const bool crn,
    const bool fast_diagnostics,
    const int seed,
    const bool use_seed) {
  // Parse R inputs.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{
      total_budget,
      rollout_policy,
      max_rollout_turns,
      ucb_exploration,
      prior_alpha,
      prior_beta,
      initial_allocations,
      unresolved_value,
      dice_mode,
      crn,
      seed,
      use_seed,
      fast_diagnostics};
  // Local RNG stream for this evaluation call.
  std::mt19937 rng = init_rng(seed, use_seed);

  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          method,
          config,
          rng);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_index = summaries[best_summary_index].candidate_index;

  return Rcpp::List::create(
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          backgammonr::canonicalize_allocation_method(method)),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_allocation_evaluate_trace(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& method,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const double unresolved_value,
    const int initial_allocations,
    const double ucb_exploration,
    const double prior_alpha,
    const double prior_beta,
    const std::string& dice_mode,
    const bool crn,
    const bool fast_diagnostics,
    const int trace_every,
    const int seed,
    const bool use_seed) {
  // Same as evaluate, but also capture checkpoint trace rows.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{
      total_budget,
      rollout_policy,
      max_rollout_turns,
      ucb_exploration,
      prior_alpha,
      prior_beta,
      initial_allocations,
      unresolved_value,
      dice_mode,
      crn,
      seed,
      use_seed,
      fast_diagnostics};
  std::mt19937 rng = init_rng(seed, use_seed);
  std::vector<AllocationTraceRow> trace_rows;
  // Reserve worst-case order-of-magnitude to reduce trace vector growth.
  trace_rows.reserve(static_cast<std::size_t>(std::max(total_budget, 0)) *
      static_cast<std::size_t>(std::max(static_cast<int>(parsed_moves.size()), 1)));

  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      evaluate_with_optional_trace(
          parsed_board,
          parsed_moves,
          method,
          config,
          rng,
          trace_every,
          &trace_rows);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_index = summaries[best_summary_index].candidate_index;

  return Rcpp::List::create(
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["trace"] = allocation_trace_rows_to_data_frame(trace_rows),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          backgammonr::canonicalize_allocation_method(method)),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_profile_rollout_runtime(
    const Rcpp::List& board,
    const Rcpp::List& roll,
    const int legal_reps,
    const int apply_reps,
    const int one_rollout_reps,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Validate repetition counts and budget for runtime profiling.
  if (legal_reps < 1) {
    throw std::range_error("`legal_reps` must be at least 1.");
  }
  if (apply_reps < 1) {
    throw std::range_error("`apply_reps` must be at least 1.");
  }
  if (one_rollout_reps < 1) {
    throw std::range_error("`one_rollout_reps` must be at least 1.");
  }
  if (total_budget < 1) {
    throw std::range_error("`total_budget` must be at least 1.");
  }

  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  std::mt19937 rng = init_rng(seed, use_seed);

  // Time legal move generation.
  auto tic = std::chrono::steady_clock::now();
  std::vector<backgammonr::MoveSequence> legal_moves;
  for (int i = 0; i < legal_reps; ++i) {
    legal_moves = backgammonr::generate_legal_move_sequences(
        parsed_board, parsed_board.turn, parsed_roll);
  }
  auto toc = std::chrono::steady_clock::now();
  const double legal_seconds = std::chrono::duration<double>(toc - tic).count();

  if (legal_moves.empty()) {
    // No legal moves: remaining timings are not defined.
    return Rcpp::List::create(
        Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(0),
        Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
        Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL));
  }

  const backgammonr::MoveSequence first_move = legal_moves.front();

  // Time move application hot path.
  tic = std::chrono::steady_clock::now();
  for (int i = 0; i < apply_reps; ++i) {
    (void) apply_sequence_without_full_validation(parsed_board, first_move);
  }
  toc = std::chrono::steady_clock::now();
  const double apply_seconds = std::chrono::duration<double>(toc - tic).count();

  const std::vector<backgammonr::MoveSequence> singleton_moves{first_move};
  const backgammonr::RolloutConfig single_rollout_config{
      1,
      rollout_policy,
      max_rollout_turns};

  // Time one-candidate one-rollout evaluations repeatedly.
  tic = std::chrono::steady_clock::now();
  for (int i = 0; i < one_rollout_reps; ++i) {
    (void) backgammonr::evaluate_move_sequences_with_allocation(
        parsed_board,
        singleton_moves,
        "equal",
        single_rollout_config,
        rng);
  }
  toc = std::chrono::steady_clock::now();
  const double one_rollout_seconds = std::chrono::duration<double>(toc - tic).count();

  const backgammonr::RolloutConfig batch_config{
      total_budget,
      rollout_policy,
      max_rollout_turns};
  // Time full batched evaluation for the provided budget.
  tic = std::chrono::steady_clock::now();
  (void) backgammonr::evaluate_move_sequences_with_allocation(
      parsed_board,
      legal_moves,
      "equal",
      batch_config,
      rng);
  toc = std::chrono::steady_clock::now();
  const double batched_seconds = std::chrono::duration<double>(toc - tic).count();

  return Rcpp::List::create(
      Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(static_cast<int>(legal_moves.size())),
      Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
      Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(apply_seconds),
      Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(one_rollout_seconds),
      Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(batched_seconds),
      Rcpp::_["legal_reps"] = Rcpp::IntegerVector::create(legal_reps),
      Rcpp::_["apply_reps"] = Rcpp::IntegerVector::create(apply_reps),
      Rcpp::_["one_rollout_reps"] = Rcpp::IntegerVector::create(one_rollout_reps),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget),
      Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(rollout_policy),
      Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(max_rollout_turns));
}
