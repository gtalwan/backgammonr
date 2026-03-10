// LINE NOTE: // [[Rcpp::depends(RcppArmadillo)]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::depends(RcppArmadillo)]]
// LINE NOTE: #include <RcppArmadillo.h>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <RcppArmadillo.h>
// LINE NOTE: // Core statistical-allocation engine.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Core statistical-allocation engine.
// LINE NOTE: //
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//
// LINE NOTE: // This file implements the finite-budget sampling logic used across methods:
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// This file implements the finite-budget sampling logic used across methods:
// LINE NOTE: // - equal allocation
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - equal allocation
// LINE NOTE: // - greedy posterior-mean allocation
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - greedy posterior-mean allocation
// LINE NOTE: // - UCB-style allocation
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - UCB-style allocation
// LINE NOTE: // - Thompson sampling
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - Thompson sampling
// LINE NOTE: // - top-two Thompson sampling (TTTS)
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - top-two Thompson sampling (TTTS)
// LINE NOTE: // - OCBA-style approximate allocation
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - OCBA-style approximate allocation
// LINE NOTE: //
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//
// LINE NOTE: // The same engine is reused by multiple wrappers so method comparisons differ
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// The same engine is reused by multiple wrappers so method comparisons differ
// LINE NOTE: // by policy choice rather than by duplicated simulation code paths.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// by policy choice rather than by duplicated simulation code paths.

// LINE NOTE: #include "bg_allocation.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_allocation.h"

// LINE NOTE: #include <algorithm>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <algorithm>
// LINE NOTE: #include <array>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <array>
// LINE NOTE: #include <chrono>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <chrono>
// LINE NOTE: #include <cmath>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <cmath>
// LINE NOTE: #include <cstdint>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <cstdint>
// LINE NOTE: #include <limits>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <limits>
// LINE NOTE: #include <optional>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <optional>
// LINE NOTE: #include <random>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <random>
// LINE NOTE: #include <sstream>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <sstream>
// LINE NOTE: #include <stdexcept>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <stdexcept>
// LINE NOTE: #include <string>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <string>
// LINE NOTE: #include <unordered_map>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <unordered_map>
// LINE NOTE: #include <vector>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <vector>

// LINE NOTE: #include "bg_game.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_game.h"
// LINE NOTE: #include "bg_movegen.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_movegen.h"
// LINE NOTE: #include "bg_rules.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_rules.h"

// LINE NOTE: namespace {
// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace {

// LINE NOTE: // Outcome encoding used for rollout reward accounting.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Outcome encoding used for rollout reward accounting.
// LINE NOTE: enum class RolloutOutcome {
// **WHAT IT'S DOING:** Defines a constrained set of named categories that the allocation engine can switch on safely.
// **IN PLAIN ENGLISH:** This is a controlled vocabulary so the algorithm uses clear, valid options instead of ambiguous integers.
enum class RolloutOutcome {
  // LINE NOTE: kWin,
  kWin,
  // LINE NOTE: kLoss,
  kLoss,
  // LINE NOTE: kUnresolved
  kUnresolved
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: // Internal policy enum for budget-allocation strategy.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Internal policy enum for budget-allocation strategy.
// LINE NOTE: enum class AllocationPolicy {
// **WHAT IT'S DOING:** Defines a constrained set of named categories that the allocation engine can switch on safely.
// **IN PLAIN ENGLISH:** This is a controlled vocabulary so the algorithm uses clear, valid options instead of ambiguous integers.
enum class AllocationPolicy {
  // LINE NOTE: kEqual,
  kEqual,
  // LINE NOTE: kGreedy,
  kGreedy,
  // LINE NOTE: kUcb,
  kUcb,
  // LINE NOTE: kThompson,
  kThompson,
  // LINE NOTE: kTtts,
  kTtts,
  // LINE NOTE: kOcba
  kOcba
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: inline constexpr double kTieTolerance = 1e-12;
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
inline constexpr double kTieTolerance = 1e-12;
// LINE NOTE: // Posterior diagnostics are Monte Carlo approximations and remain deterministic
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Posterior diagnostics are Monte Carlo approximations and remain deterministic
// LINE NOTE: // under a fixed seed because they use the same local RNG stream.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// under a fixed seed because they use the same local RNG stream.
// LINE NOTE: inline constexpr int kPosteriorDiagnosticDraws = 512;
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
inline constexpr int kPosteriorDiagnosticDraws = 512;

// LINE NOTE: struct CollapsedCandidate {
// **WHAT IT'S DOING:** Defines a structured record used to carry related simulation or posterior fields together.
// **IN PLAIN ENGLISH:** This is a named container that keeps related numbers grouped so downstream summaries stay coherent.
struct CollapsedCandidate {
  // LINE NOTE: // Board after applying representative legal sequence.
  // Board after applying representative legal sequence.
  // LINE NOTE: backgammonr::BoardState board_after{};
  backgammonr::BoardState board_after{};
  // LINE NOTE: // 0-based index of representative move in original legal move list.
  // 0-based index of representative move in original legal move list.
  // LINE NOTE: int representative_index{0};
  int representative_index{0};
  // LINE NOTE: // Player who made the candidate move.
  // Player who made the candidate move.
  // LINE NOTE: int acting_player{1};
  int acting_player{1};
  // LINE NOTE: // Number of equivalent sequences collapsed into this state.
  // Number of equivalent sequences collapsed into this state.
  // LINE NOTE: int n_equivalent{1};
  int n_equivalent{1};
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: struct AllocationTraceRow {
// **WHAT IT'S DOING:** Defines a structured record used to carry related simulation or posterior fields together.
// **IN PLAIN ENGLISH:** This is a named container that keeps related numbers grouped so downstream summaries stay coherent.
struct AllocationTraceRow {
  // LINE NOTE: // Snapshot metadata.
  // Snapshot metadata.
  // LINE NOTE: int checkpoint{0};
  int checkpoint{0};
  // LINE NOTE: int selected_candidate{NA_INTEGER};
  int selected_candidate{NA_INTEGER};
  // LINE NOTE: int leader_index{NA_INTEGER};
  int leader_index{NA_INTEGER};
  // LINE NOTE: // Candidate-level stats at this checkpoint.
  // Candidate-level stats at this checkpoint.
  // LINE NOTE: int candidate_index{0};
  int candidate_index{0};
  // LINE NOTE: int allocation_count{0};
  int allocation_count{0};
  // LINE NOTE: int wins{0};
  int wins{0};
  // LINE NOTE: int losses{0};
  int losses{0};
  // LINE NOTE: int unresolved{0};
  int unresolved{0};
  // LINE NOTE: double empirical_value{NA_REAL};
  double empirical_value{NA_REAL};
  // LINE NOTE: double alpha{1.0};
  double alpha{1.0};
  // LINE NOTE: double beta{1.0};
  double beta{1.0};
  // LINE NOTE: double estimate{0.5};
  double estimate{0.5};
  // LINE NOTE: double posterior_sd{0.0};
  double posterior_sd{0.0};
  // LINE NOTE: double lower_95{0.0};
  double lower_95{0.0};
  // LINE NOTE: double upper_95{1.0};
  double upper_95{1.0};
  // LINE NOTE: double selection_score{0.5};
  double selection_score{0.5};
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: struct ForcedRollSchedule {
// **WHAT IT'S DOING:** Defines a structured record used to carry related simulation or posterior fields together.
// **IN PLAIN ENGLISH:** This is a named container that keeps related numbers grouped so downstream summaries stay coherent.
struct ForcedRollSchedule {
  // LINE NOTE: // Up to two pre-scheduled rolls for stratified dice modes.
  // Up to two pre-scheduled rolls for stratified dice modes.
  // LINE NOTE: std::array<backgammonr::DiceRoll, 2> rolls{};
  std::array<backgammonr::DiceRoll, 2> rolls{};
  // LINE NOTE: int n_rolls{0};
  int n_rolls{0};
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: RolloutOutcome outcome_from_turn_result(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
RolloutOutcome outcome_from_turn_result(
    // LINE NOTE: const backgammonr::TurnResult& turn_result,
    const backgammonr::TurnResult& turn_result,
    // LINE NOTE: const int acting_player);
    const int acting_player);

// LINE NOTE: backgammonr::BoardState apply_sequence_without_full_validation(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
backgammonr::BoardState apply_sequence_without_full_validation(
    // LINE NOTE: const backgammonr::BoardState& board,
    const backgammonr::BoardState& board,
    // LINE NOTE: const backgammonr::MoveSequence& sequence) {
    const backgammonr::MoveSequence& sequence) {
  // LINE NOTE: // Copy once, then apply sequence steps without re-validating each step.
  // Copy once, then apply sequence steps without re-validating each step.
  // LINE NOTE: backgammonr::BoardState out = board;
  backgammonr::BoardState out = board;

  // LINE NOTE: for (const backgammonr::MoveStep& step : sequence.steps) {
  for (const backgammonr::MoveStep& step : sequence.steps) {
    // LINE NOTE: backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  // LINE NOTE: }
  }
  // LINE NOTE: out.turn = -sequence.player;
  out.turn = -sequence.player;
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Lightweight random turn helper used inside rollout loop.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Lightweight random turn helper used inside rollout loop.
// LINE NOTE: void play_random_turn_lightweight(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void play_random_turn_lightweight(
    // LINE NOTE: backgammonr::BoardState& board,
    backgammonr::BoardState& board,
    // LINE NOTE: const backgammonr::DiceRoll& roll,
    const backgammonr::DiceRoll& roll,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::mt19937 init_rng(const int seed, const bool use_seed) {
// **WHAT IT'S DOING:** Creates a Mersenne Twister random-number generator used for reproducible stochastic simulation.
// **IN PLAIN ENGLISH:** This is the randomness engine that drives rollouts and posterior sampling.
std::mt19937 init_rng(const int seed, const bool use_seed) {
  // LINE NOTE: std::mt19937 rng;
  std::mt19937 rng;

  // LINE NOTE: if (use_seed) {
  if (use_seed) {
    // LINE NOTE: if (seed < 0) {
    if (seed < 0) {
      // LINE NOTE: throw std::range_error("`seed` must be nonnegative when supplied.");
      throw std::range_error("`seed` must be nonnegative when supplied.");
    // LINE NOTE: }
    }
    // LINE NOTE: rng.seed(static_cast<std::uint32_t>(seed));
    rng.seed(static_cast<std::uint32_t>(seed));
  // LINE NOTE: } else {
  } else {
    // LINE NOTE: std::random_device rd;
    std::random_device rd;
    // LINE NOTE: rng.seed(rd());
    rng.seed(rd());
  // LINE NOTE: }
  }

  // LINE NOTE: return rng;
  return rng;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: double outcome_reward(const RolloutOutcome outcome, const backgammonr::RolloutConfig& config) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
double outcome_reward(const RolloutOutcome outcome, const backgammonr::RolloutConfig& config) {
  // LINE NOTE: // Map terminal outcome to Bernoulli-style reward with unresolved fallback.
  // Map terminal outcome to Bernoulli-style reward with unresolved fallback.
  // LINE NOTE: if (outcome == RolloutOutcome::kWin) {
  if (outcome == RolloutOutcome::kWin) {
    // LINE NOTE: return 1.0;
    return 1.0;
  // LINE NOTE: }
  }

  // LINE NOTE: if (outcome == RolloutOutcome::kLoss) {
  if (outcome == RolloutOutcome::kLoss) {
    // LINE NOTE: return 0.0;
    return 0.0;
  // LINE NOTE: }
  }

  // LINE NOTE: return config.unresolved_value;
  return config.unresolved_value;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: RolloutOutcome single_rollout_outcome(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
RolloutOutcome single_rollout_outcome(
    // LINE NOTE: const backgammonr::BoardState& board_after,
    const backgammonr::BoardState& board_after,
    // LINE NOTE: const int acting_player,
    const int acting_player,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng,
    std::mt19937& rng,
    // LINE NOTE: const ForcedRollSchedule& forced_rolls) {
    const ForcedRollSchedule& forced_rolls) {
  // LINE NOTE: // Candidate already ends game.
  // Candidate already ends game.
  // LINE NOTE: if (backgammonr::board_is_terminal(board_after)) {
  if (backgammonr::board_is_terminal(board_after)) {
    // LINE NOTE: return backgammonr::board_winner(board_after) == acting_player
    return backgammonr::board_winner(board_after) == acting_player
        // LINE NOTE: ? RolloutOutcome::kWin
        ? RolloutOutcome::kWin
        // LINE NOTE: : RolloutOutcome::kLoss;
        : RolloutOutcome::kLoss;
  // LINE NOTE: }
  }

  // LINE NOTE: if (config.max_turns <= 0) {
  if (config.max_turns <= 0) {
    // LINE NOTE: return RolloutOutcome::kUnresolved;
    return RolloutOutcome::kUnresolved;
  // LINE NOTE: }
  }

  // LINE NOTE: // Fast path for random-policy rollout.
  // Fast path for random-policy rollout.
  // LINE NOTE: if (config.policy == "random") {
  if (config.policy == "random") {
    // LINE NOTE: backgammonr::BoardState current = board_after;
    backgammonr::BoardState current = board_after;
    // LINE NOTE: int turns_remaining = config.max_turns;
    int turns_remaining = config.max_turns;

    // LINE NOTE: // Consume any forced stratification rolls first.
    // Consume any forced stratification rolls first.
    // LINE NOTE: for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
    for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
      // LINE NOTE: if (turns_remaining <= 0) {
      if (turns_remaining <= 0) {
        // LINE NOTE: return RolloutOutcome::kUnresolved;
        return RolloutOutcome::kUnresolved;
      // LINE NOTE: }
      }

      // LINE NOTE: play_random_turn_lightweight(current, forced_rolls.rolls[forced_idx], rng);
      play_random_turn_lightweight(current, forced_rolls.rolls[forced_idx], rng);
      // LINE NOTE: if (backgammonr::board_is_terminal(current)) {
      if (backgammonr::board_is_terminal(current)) {
        // LINE NOTE: return backgammonr::board_winner(current) == acting_player
        return backgammonr::board_winner(current) == acting_player
            // LINE NOTE: ? RolloutOutcome::kWin
            ? RolloutOutcome::kWin
            // LINE NOTE: : RolloutOutcome::kLoss;
            : RolloutOutcome::kLoss;
      // LINE NOTE: }
      }
      // LINE NOTE: --turns_remaining;
      --turns_remaining;
    // LINE NOTE: }
    }

    // LINE NOTE: for (int turn = 0; turn < turns_remaining; ++turn) {
    for (int turn = 0; turn < turns_remaining; ++turn) {
      // LINE NOTE: // Then continue with IID sampled rolls.
      // Then continue with IID sampled rolls.
      // LINE NOTE: play_random_turn_lightweight(current, backgammonr::roll_dice(rng), rng);
      play_random_turn_lightweight(current, backgammonr::roll_dice(rng), rng);
      // LINE NOTE: if (backgammonr::board_is_terminal(current)) {
      if (backgammonr::board_is_terminal(current)) {
        // LINE NOTE: return backgammonr::board_winner(current) == acting_player
        return backgammonr::board_winner(current) == acting_player
            // LINE NOTE: ? RolloutOutcome::kWin
            ? RolloutOutcome::kWin
            // LINE NOTE: : RolloutOutcome::kLoss;
            : RolloutOutcome::kLoss;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: return RolloutOutcome::kUnresolved;
    return RolloutOutcome::kUnresolved;
  // LINE NOTE: }
  }

  // LINE NOTE: backgammonr::BoardState current = board_after;
  backgammonr::BoardState current = board_after;
  // LINE NOTE: int turns_remaining = config.max_turns;
  int turns_remaining = config.max_turns;
  // LINE NOTE: // Non-random policy path still honors forced-roll prefix.
  // Non-random policy path still honors forced-roll prefix.
  // LINE NOTE: for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
  for (int forced_idx = 0; forced_idx < forced_rolls.n_rolls; ++forced_idx) {
    // LINE NOTE: if (turns_remaining <= 0) {
    if (turns_remaining <= 0) {
      // LINE NOTE: return RolloutOutcome::kUnresolved;
      return RolloutOutcome::kUnresolved;
    // LINE NOTE: }
    }

    // LINE NOTE: const backgammonr::TurnResult turn_result = backgammonr::play_turn_with_roll(
    const backgammonr::TurnResult turn_result = backgammonr::play_turn_with_roll(
        // LINE NOTE: current,
        current,
        // LINE NOTE: forced_rolls.rolls[forced_idx],
        forced_rolls.rolls[forced_idx],
        // LINE NOTE: config.policy,
        config.policy,
        // LINE NOTE: &rng,
        &rng,
        // LINE NOTE: backgammonr::RolloutConfig());
        backgammonr::RolloutConfig());
    // LINE NOTE: if (turn_result.game_over) {
    if (turn_result.game_over) {
      // LINE NOTE: return outcome_from_turn_result(turn_result, acting_player);
      return outcome_from_turn_result(turn_result, acting_player);
    // LINE NOTE: }
    }

    // LINE NOTE: current = turn_result.board_after;
    current = turn_result.board_after;
    // LINE NOTE: --turns_remaining;
    --turns_remaining;
  // LINE NOTE: }
  }

  // LINE NOTE: const backgammonr::GameResult rollout_result = backgammonr::play_game_random(
  const backgammonr::GameResult rollout_result = backgammonr::play_game_random(
      // LINE NOTE: current,
      current,
      // LINE NOTE: turns_remaining,
      turns_remaining,
      // LINE NOTE: rng,
      rng,
      // LINE NOTE: config.policy,
      config.policy,
      // LINE NOTE: backgammonr::RolloutConfig());
      backgammonr::RolloutConfig());

  // LINE NOTE: if (!rollout_result.game_over) {
  if (!rollout_result.game_over) {
    // LINE NOTE: return RolloutOutcome::kUnresolved;
    return RolloutOutcome::kUnresolved;
  // LINE NOTE: }
  }
  // LINE NOTE: return rollout_result.winner == acting_player
  return rollout_result.winner == acting_player
      // LINE NOTE: ? RolloutOutcome::kWin
      ? RolloutOutcome::kWin
      // LINE NOTE: : RolloutOutcome::kLoss;
      : RolloutOutcome::kLoss;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void update_summary(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void update_summary(
    // LINE NOTE: backgammonr::ActionEvaluationSummary& summary,
    backgammonr::ActionEvaluationSummary& summary,
    // LINE NOTE: const RolloutOutcome outcome,
    const RolloutOutcome outcome,
    // LINE NOTE: const backgammonr::RolloutConfig& config) {
    const backgammonr::RolloutConfig& config) {
  // LINE NOTE: // Track raw outcome counts.
  // Track raw outcome counts.
  // LINE NOTE: summary.allocation_count += 1;
  summary.allocation_count += 1;

  // LINE NOTE: if (outcome == RolloutOutcome::kWin) {
  if (outcome == RolloutOutcome::kWin) {
    // LINE NOTE: summary.wins += 1;
    summary.wins += 1;
  // LINE NOTE: } else if (outcome == RolloutOutcome::kLoss) {
  } else if (outcome == RolloutOutcome::kLoss) {
    // LINE NOTE: summary.losses += 1;
    summary.losses += 1;
  // LINE NOTE: } else {
  } else {
    // LINE NOTE: summary.unresolved += 1;
    summary.unresolved += 1;
  // LINE NOTE: }
  }

  // LINE NOTE: const double reward = outcome_reward(outcome, config);
  const double reward = outcome_reward(outcome, config);
  // LINE NOTE: // Conjugate Beta-Bernoulli posterior update.
  // Conjugate Beta-Bernoulli posterior update.
  // LINE NOTE: summary.alpha += reward;
  summary.alpha += reward;
  // LINE NOTE: summary.beta += (1.0 - reward);
  summary.beta += (1.0 - reward);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: double sample_beta_distribution(const double alpha, const double beta, std::mt19937& rng) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
double sample_beta_distribution(const double alpha, const double beta, std::mt19937& rng) {
  // LINE NOTE: if (alpha <= 0.0 || beta <= 0.0) {
  if (alpha <= 0.0 || beta <= 0.0) {
    // LINE NOTE: throw std::range_error("Beta posterior parameters must be positive.");
    throw std::range_error("Beta posterior parameters must be positive.");
  // LINE NOTE: }
  }

  // LINE NOTE: std::gamma_distribution<double> gamma_alpha(alpha, 1.0);
  std::gamma_distribution<double> gamma_alpha(alpha, 1.0);
  // LINE NOTE: std::gamma_distribution<double> gamma_beta(beta, 1.0);
  std::gamma_distribution<double> gamma_beta(beta, 1.0);
  // LINE NOTE: const double x = gamma_alpha(rng);
  const double x = gamma_alpha(rng);
  // LINE NOTE: const double y = gamma_beta(rng);
  const double y = gamma_beta(rng);

  // LINE NOTE: if (x <= 0.0 && y <= 0.0) {
  if (x <= 0.0 && y <= 0.0) {
    // LINE NOTE: return 0.5;
    return 0.5;
  // LINE NOTE: }
  }

  // LINE NOTE: return x / (x + y);
  return x / (x + y);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: AllocationPolicy parse_allocation_policy(const std::string& canonical_method) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
AllocationPolicy parse_allocation_policy(const std::string& canonical_method) {
  // LINE NOTE: // Map canonical method string to internal switch enum.
  // Map canonical method string to internal switch enum.
  // LINE NOTE: if (canonical_method == "equal") {
  if (canonical_method == "equal") {
    // LINE NOTE: return AllocationPolicy::kEqual;
    return AllocationPolicy::kEqual;
  // LINE NOTE: }
  }
  // LINE NOTE: if (canonical_method == "greedy") {
  if (canonical_method == "greedy") {
    // LINE NOTE: return AllocationPolicy::kGreedy;
    return AllocationPolicy::kGreedy;
  // LINE NOTE: }
  }
  // LINE NOTE: if (canonical_method == "ucb") {
  if (canonical_method == "ucb") {
    // LINE NOTE: return AllocationPolicy::kUcb;
    return AllocationPolicy::kUcb;
  // LINE NOTE: }
  }
  // LINE NOTE: if (canonical_method == "thompson") {
  if (canonical_method == "thompson") {
    // LINE NOTE: return AllocationPolicy::kThompson;
    return AllocationPolicy::kThompson;
  // LINE NOTE: }
  }
  // LINE NOTE: if (canonical_method == "ttts") {
  if (canonical_method == "ttts") {
    // LINE NOTE: return AllocationPolicy::kTtts;
    return AllocationPolicy::kTtts;
  // LINE NOTE: }
  }
  // LINE NOTE: if (canonical_method == "ocba") {
  if (canonical_method == "ocba") {
    // LINE NOTE: return AllocationPolicy::kOcba;
    return AllocationPolicy::kOcba;
  // LINE NOTE: }
  }

  // LINE NOTE: throw std::range_error("Unsupported allocation method.");
  throw std::range_error("Unsupported allocation method.");
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: bool score_beats_incumbent(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool score_beats_incumbent(
    // LINE NOTE: const double score,
    const double score,
    // LINE NOTE: const int allocation_count,
    const int allocation_count,
    // LINE NOTE: const double incumbent_score,
    const double incumbent_score,
    // LINE NOTE: const int incumbent_allocation_count) {
    const int incumbent_allocation_count) {
  // LINE NOTE: // Primary criterion: larger score.
  // Primary criterion: larger score.
  // LINE NOTE: if (score > incumbent_score + kTieTolerance) {
  if (score > incumbent_score + kTieTolerance) {
    // LINE NOTE: return true;
    return true;
  // LINE NOTE: }
  }

  // LINE NOTE: if (std::fabs(score - incumbent_score) <= kTieTolerance &&
  if (std::fabs(score - incumbent_score) <= kTieTolerance &&
      // LINE NOTE: allocation_count < incumbent_allocation_count) {
      allocation_count < incumbent_allocation_count) {
    // LINE NOTE: // Tie-break toward less-sampled candidate (encourages balance).
    // Tie-break toward less-sampled candidate (encourages balance).
    // LINE NOTE: return true;
    return true;
  // LINE NOTE: }
  }

  // LINE NOTE: return false;
  return false;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::uint32_t stable_rollout_seed(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::uint32_t stable_rollout_seed(
    // LINE NOTE: const std::uint32_t base_seed,
    const std::uint32_t base_seed,
    // LINE NOTE: const int sample_index,
    const int sample_index,
    // LINE NOTE: const int salt) {
    const int salt) {
  // LINE NOTE: // Mix sample index and salt into a stable 32-bit seed.
  // Mix sample index and salt into a stable 32-bit seed.
  // LINE NOTE: std::uint32_t x = base_seed ^ static_cast<std::uint32_t>(sample_index * 0x9e3779b9U);
  std::uint32_t x = base_seed ^ static_cast<std::uint32_t>(sample_index * 0x9e3779b9U);
  // LINE NOTE: x ^= static_cast<std::uint32_t>(salt * 0x7f4a7c15U);
  x ^= static_cast<std::uint32_t>(salt * 0x7f4a7c15U);
  // LINE NOTE: x ^= x >> 16;
  x ^= x >> 16;
  // LINE NOTE: x *= 0x85ebca6bU;
  x *= 0x85ebca6bU;
  // LINE NOTE: x ^= x >> 13;
  x ^= x >> 13;
  // LINE NOTE: x *= 0xc2b2ae35U;
  x *= 0xc2b2ae35U;
  // LINE NOTE: x ^= x >> 16;
  x ^= x >> 16;
  // LINE NOTE: return x;
  return x;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: const std::vector<backgammonr::DiceRoll>& unique_unordered_rolls() {
// **WHAT IT'S DOING:** Declares an immutable value to make intent explicit and prevent accidental mutation.
// **IN PLAIN ENGLISH:** This locks a value so it cannot be changed later by mistake.
const std::vector<backgammonr::DiceRoll>& unique_unordered_rolls() {
  // LINE NOTE: // Lazily initialize 21 unordered roll outcomes (1-1, 1-2, ..., 6-6).
  // Lazily initialize 21 unordered roll outcomes (1-1, 1-2, ..., 6-6).
  // LINE NOTE: static const std::vector<backgammonr::DiceRoll> outcomes = []() {
  static const std::vector<backgammonr::DiceRoll> outcomes = []() {
    // LINE NOTE: std::vector<backgammonr::DiceRoll> out;
    std::vector<backgammonr::DiceRoll> out;
    // LINE NOTE: out.reserve(21);
    out.reserve(21);
    // LINE NOTE: for (int die1 = backgammonr::kMinDieValue; die1 <= backgammonr::kMaxDieValue; ++die1) {
    for (int die1 = backgammonr::kMinDieValue; die1 <= backgammonr::kMaxDieValue; ++die1) {
      // LINE NOTE: for (int die2 = die1; die2 <= backgammonr::kMaxDieValue; ++die2) {
      for (int die2 = die1; die2 <= backgammonr::kMaxDieValue; ++die2) {
        // LINE NOTE: out.push_back(backgammonr::make_roll(die1, die2));
        out.push_back(backgammonr::make_roll(die1, die2));
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
    // LINE NOTE: return out;
    return out;
  // LINE NOTE: }();
  }();
  // LINE NOTE: return outcomes;
  return outcomes;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: ForcedRollSchedule scheduled_forced_rolls(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
ForcedRollSchedule scheduled_forced_rolls(
    // LINE NOTE: const std::string& dice_mode,
    const std::string& dice_mode,
    // LINE NOTE: const int sample_index,
    const int sample_index,
    // LINE NOTE: const int offset) {
    const int offset) {
  // LINE NOTE: ForcedRollSchedule schedule;
  ForcedRollSchedule schedule;

  // LINE NOTE: if (dice_mode == "iid") {
  if (dice_mode == "iid") {
    // LINE NOTE: // No stratification; rollout draws rolls normally.
    // No stratification; rollout draws rolls normally.
    // LINE NOTE: return schedule;
    return schedule;
  // LINE NOTE: }
  }

  // LINE NOTE: const std::vector<backgammonr::DiceRoll>& outcomes = unique_unordered_rolls();
  const std::vector<backgammonr::DiceRoll>& outcomes = unique_unordered_rolls();
  // LINE NOTE: const int n = static_cast<int>(outcomes.size());
  const int n = static_cast<int>(outcomes.size());

  // LINE NOTE: if (dice_mode == "stratified_first_roll") {
  if (dice_mode == "stratified_first_roll") {
    // LINE NOTE: // Deterministically cycle first roll through 21 unordered outcomes.
    // Deterministically cycle first roll through 21 unordered outcomes.
    // LINE NOTE: const int idx = (offset + sample_index - 1) % n;
    const int idx = (offset + sample_index - 1) % n;
    // LINE NOTE: schedule.rolls[0] = outcomes[idx];
    schedule.rolls[0] = outcomes[idx];
    // LINE NOTE: schedule.n_rolls = 1;
    schedule.n_rolls = 1;
    // LINE NOTE: return schedule;
    return schedule;
  // LINE NOTE: }
  }

  // LINE NOTE: if (dice_mode == "stratified_first_two_rolls") {
  if (dice_mode == "stratified_first_two_rolls") {
    // LINE NOTE: // Deterministically cycle first two rolls through 21 x 21 combinations.
    // Deterministically cycle first two rolls through 21 x 21 combinations.
    // LINE NOTE: const int n2 = n * n;
    const int n2 = n * n;
    // LINE NOTE: const int pair_idx = (offset + sample_index - 1) % n2;
    const int pair_idx = (offset + sample_index - 1) % n2;
    // LINE NOTE: const int idx1 = pair_idx / n;
    const int idx1 = pair_idx / n;
    // LINE NOTE: const int idx2 = pair_idx % n;
    const int idx2 = pair_idx % n;
    // LINE NOTE: schedule.rolls[0] = outcomes[idx1];
    schedule.rolls[0] = outcomes[idx1];
    // LINE NOTE: schedule.rolls[1] = outcomes[idx2];
    schedule.rolls[1] = outcomes[idx2];
    // LINE NOTE: schedule.n_rolls = 2;
    schedule.n_rolls = 2;
    // LINE NOTE: return schedule;
    return schedule;
  // LINE NOTE: }
  }

  // LINE NOTE: throw std::range_error("Unsupported dice stratification mode.");
  throw std::range_error("Unsupported dice stratification mode.");
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: RolloutOutcome outcome_from_turn_result(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
RolloutOutcome outcome_from_turn_result(
    // LINE NOTE: const backgammonr::TurnResult& turn_result,
    const backgammonr::TurnResult& turn_result,
    // LINE NOTE: const int acting_player) {
    const int acting_player) {
  // LINE NOTE: if (!turn_result.game_over) {
  if (!turn_result.game_over) {
    // LINE NOTE: return RolloutOutcome::kUnresolved;
    return RolloutOutcome::kUnresolved;
  // LINE NOTE: }
  }
  // LINE NOTE: return turn_result.winner == acting_player ? RolloutOutcome::kWin : RolloutOutcome::kLoss;
  return turn_result.winner == acting_player ? RolloutOutcome::kWin : RolloutOutcome::kLoss;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: struct BoardStateKey {
// **WHAT IT'S DOING:** Defines a structured record used to carry related simulation or posterior fields together.
// **IN PLAIN ENGLISH:** This is a named container that keeps related numbers grouped so downstream summaries stay coherent.
struct BoardStateKey {
  // LINE NOTE: // Hashable canonical board representation for candidate deduplication.
  // Hashable canonical board representation for candidate deduplication.
  // LINE NOTE: std::array<int, backgammonr::kNumPoints> points{};
  std::array<int, backgammonr::kNumPoints> points{};
  // LINE NOTE: std::array<int, backgammonr::kNumPlayers> bar{};
  std::array<int, backgammonr::kNumPlayers> bar{};
  // LINE NOTE: std::array<int, backgammonr::kNumPlayers> off{};
  std::array<int, backgammonr::kNumPlayers> off{};
  // LINE NOTE: int turn{1};
  int turn{1};

  // LINE NOTE: bool operator==(const BoardStateKey& other) const {
  bool operator==(const BoardStateKey& other) const {
    // LINE NOTE: return turn == other.turn &&
    return turn == other.turn &&
        // LINE NOTE: points == other.points &&
        points == other.points &&
        // LINE NOTE: bar == other.bar &&
        bar == other.bar &&
        // LINE NOTE: off == other.off;
        off == other.off;
  // LINE NOTE: }
  }
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: struct BoardStateKeyHash {
// **WHAT IT'S DOING:** Defines a structured record used to carry related simulation or posterior fields together.
// **IN PLAIN ENGLISH:** This is a named container that keeps related numbers grouped so downstream summaries stay coherent.
struct BoardStateKeyHash {
  // LINE NOTE: std::size_t operator()(const BoardStateKey& key) const {
  std::size_t operator()(const BoardStateKey& key) const {
    // LINE NOTE: std::size_t h = static_cast<std::size_t>(key.turn * 1315423911U);
    std::size_t h = static_cast<std::size_t>(key.turn * 1315423911U);

    // LINE NOTE: for (const int value : key.points) {
    for (const int value : key.points) {
      // LINE NOTE: h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    // LINE NOTE: }
    }
    // LINE NOTE: for (const int value : key.bar) {
    for (const int value : key.bar) {
      // LINE NOTE: h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    // LINE NOTE: }
    }
    // LINE NOTE: for (const int value : key.off) {
    for (const int value : key.off) {
      // LINE NOTE: h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    // LINE NOTE: }
    }

    // LINE NOTE: return h;
    return h;
  // LINE NOTE: }
  }
// LINE NOTE: };
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
};

// LINE NOTE: BoardStateKey board_state_key(const backgammonr::BoardState& board) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
BoardStateKey board_state_key(const backgammonr::BoardState& board) {
  // LINE NOTE: // Copy board fields into fixed key struct for unordered_map lookup.
  // Copy board fields into fixed key struct for unordered_map lookup.
  // LINE NOTE: BoardStateKey key;
  BoardStateKey key;
  // LINE NOTE: key.points = board.points;
  key.points = board.points;
  // LINE NOTE: key.bar = board.bar;
  key.bar = board.bar;
  // LINE NOTE: key.off = board.off;
  key.off = board.off;
  // LINE NOTE: key.turn = board.turn;
  key.turn = board.turn;
  // LINE NOTE: return key;
  return key;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::vector<CollapsedCandidate> collapse_equivalent_candidates(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::vector<CollapsedCandidate> collapse_equivalent_candidates(
    // LINE NOTE: const backgammonr::BoardState& board,
    const backgammonr::BoardState& board,
    // LINE NOTE: const std::vector<backgammonr::MoveSequence>& legal_moves) {
    const std::vector<backgammonr::MoveSequence>& legal_moves) {
  // **WHAT IT'S DOING (DETAILED):** We canonicalize the action set by merging
  // different legal move sequences that end in the same resulting board state.
  // This avoids spending duplicate rollout budget on strategically identical
  // outcomes.
  // **IN PLAIN ENGLISH:** If two move strings land on the same board, we treat
  // them as one option so simulation time is not wasted repeating equivalent work.
  // LINE NOTE: // Collapse moves that lead to identical board states.
  // Collapse moves that lead to identical board states.
  // LINE NOTE: std::vector<CollapsedCandidate> collapsed;
  std::vector<CollapsedCandidate> collapsed;
  // LINE NOTE: collapsed.reserve(legal_moves.size());
  collapsed.reserve(legal_moves.size());
  // LINE NOTE: std::unordered_map<BoardStateKey, int, BoardStateKeyHash> key_to_collapsed_index;
  std::unordered_map<BoardStateKey, int, BoardStateKeyHash> key_to_collapsed_index;
  // LINE NOTE: key_to_collapsed_index.reserve(legal_moves.size());
  key_to_collapsed_index.reserve(legal_moves.size());

  // LINE NOTE: for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    // LINE NOTE: // Apply move once and hash resulting board state.
    // Apply move once and hash resulting board state.
    // LINE NOTE: const backgammonr::BoardState board_after =
    const backgammonr::BoardState board_after =
        // LINE NOTE: apply_sequence_without_full_validation(board, legal_moves[i]);
        apply_sequence_without_full_validation(board, legal_moves[i]);
    // LINE NOTE: const BoardStateKey key = board_state_key(board_after);
    const BoardStateKey key = board_state_key(board_after);
    // LINE NOTE: const auto it = key_to_collapsed_index.find(key);
    const auto it = key_to_collapsed_index.find(key);

    // LINE NOTE: if (it == key_to_collapsed_index.end()) {
    if (it == key_to_collapsed_index.end()) {
      // LINE NOTE: // First time this board-after appears.
      // First time this board-after appears.
      // LINE NOTE: CollapsedCandidate row;
      CollapsedCandidate row;
      // LINE NOTE: row.board_after = board_after;
      row.board_after = board_after;
      // LINE NOTE: row.representative_index = i;
      row.representative_index = i;
      // LINE NOTE: row.acting_player = legal_moves[i].player;
      row.acting_player = legal_moves[i].player;
      // LINE NOTE: row.n_equivalent = 1;
      row.n_equivalent = 1;
      // LINE NOTE: key_to_collapsed_index.emplace(key, static_cast<int>(collapsed.size()));
      key_to_collapsed_index.emplace(key, static_cast<int>(collapsed.size()));
      // LINE NOTE: collapsed.push_back(row);
      collapsed.push_back(row);
    // LINE NOTE: } else {
    } else {
      // LINE NOTE: // Equivalent board state: increase multiplicity only.
      // Equivalent board state: increase multiplicity only.
      // LINE NOTE: collapsed[it->second].n_equivalent += 1;
      collapsed[it->second].n_equivalent += 1;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: return collapsed;
  return collapsed;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void compute_posterior_diagnostics(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void compute_posterior_diagnostics(
    // LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Approximates two Bayesian diagnostics from
  // posterior draws:
  // 1) `prob_best`: how often each candidate wins a posterior draw tournament.
  // 2) `posterior_expected_regret`: average gap to the sampled best value.
  // **IN PLAIN ENGLISH:** We repeatedly "simulate what might be true" under
  // current uncertainty, then count how often each move looks best and how much
  // value is lost if we picked a non-best move in those hypothetical worlds.
  // LINE NOTE: // Monte Carlo posterior diagnostics (probability best + expected regret).
  // Monte Carlo posterior diagnostics (probability best + expected regret).
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: if (n == 0 || kPosteriorDiagnosticDraws <= 0) {
  if (n == 0 || kPosteriorDiagnosticDraws <= 0) {
    // LINE NOTE: return;
    return;
  // LINE NOTE: }
  }

  // LINE NOTE: arma::vec draw(n);
  arma::vec draw(n);
  // LINE NOTE: arma::vec best_count(n, arma::fill::zeros);
  arma::vec best_count(n, arma::fill::zeros);
  // LINE NOTE: arma::vec regret_sum(n, arma::fill::zeros);
  arma::vec regret_sum(n, arma::fill::zeros);

  // LINE NOTE: for (int draw_idx = 0; draw_idx < kPosteriorDiagnosticDraws; ++draw_idx) {
  for (int draw_idx = 0; draw_idx < kPosteriorDiagnosticDraws; ++draw_idx) {
    // LINE NOTE: int best_index = 0;
    int best_index = 0;
    // LINE NOTE: double best_value = -std::numeric_limits<double>::infinity();
    double best_value = -std::numeric_limits<double>::infinity();

    // LINE NOTE: for (int i = 0; i < n; ++i) {
    for (int i = 0; i < n; ++i) {
      // LINE NOTE: // Draw one posterior sample per candidate.
      // Draw one posterior sample per candidate.
      // LINE NOTE: draw[i] = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
      draw[i] = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
      // LINE NOTE: if (draw[i] > best_value) {
      if (draw[i] > best_value) {
        // LINE NOTE: best_value = draw[i];
        best_value = draw[i];
        // LINE NOTE: best_index = i;
        best_index = i;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: best_count[best_index] += 1.0;
    best_count[best_index] += 1.0;
    // The vector subtraction is element-wise:
    // `best_value - draw[i]` is the regret of candidate i in this posterior world.
    // Averaging this across draws gives a Bayes-style simple regret diagnostic.
    // LINE NOTE: // Bayes simple regret for each candidate under this posterior draw.
    // Bayes simple regret for each candidate under this posterior draw.
    // LINE NOTE: regret_sum += (best_value - draw);
    regret_sum += (best_value - draw);
  // LINE NOTE: }
  }

  // LINE NOTE: const double denom = static_cast<double>(kPosteriorDiagnosticDraws);
  const double denom = static_cast<double>(kPosteriorDiagnosticDraws);
  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: summaries[i].prob_best = best_count[i] / denom;
    summaries[i].prob_best = best_count[i] / denom;
    // LINE NOTE: summaries[i].posterior_expected_regret = regret_sum[i] / denom;
    summaries[i].posterior_expected_regret = regret_sum[i] / denom;
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void finalize_summaries(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void finalize_summaries(
    // LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: const AllocationPolicy policy,
    const AllocationPolicy policy,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: // Convert integer counts and Beta parameters into vectorized Armadillo arrays.
  // Convert integer counts and Beta parameters into vectorized Armadillo arrays.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: arma::vec counts(n);
  arma::vec counts(n);
  // LINE NOTE: arma::vec wins(n);
  arma::vec wins(n);
  // LINE NOTE: arma::vec unresolved(n);
  arma::vec unresolved(n);
  // LINE NOTE: arma::vec alpha(n);
  arma::vec alpha(n);
  // LINE NOTE: arma::vec beta(n);
  arma::vec beta(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: counts[i] = static_cast<double>(summaries[i].allocation_count);
    counts[i] = static_cast<double>(summaries[i].allocation_count);
    // LINE NOTE: wins[i] = static_cast<double>(summaries[i].wins);
    wins[i] = static_cast<double>(summaries[i].wins);
    // LINE NOTE: unresolved[i] = static_cast<double>(summaries[i].unresolved);
    unresolved[i] = static_cast<double>(summaries[i].unresolved);
    // LINE NOTE: alpha[i] = summaries[i].alpha;
    alpha[i] = summaries[i].alpha;
    // LINE NOTE: beta[i] = summaries[i].beta;
    beta[i] = summaries[i].beta;
  // LINE NOTE: }
  }

  // LINE NOTE: const arma::vec posterior_mean = alpha / (alpha + beta);
  const arma::vec posterior_mean = alpha / (alpha + beta);
  // LINE NOTE: const arma::vec posterior_var =
  const arma::vec posterior_var =
      // LINE NOTE: (alpha % beta) / (arma::square(alpha + beta) % (alpha + beta + 1.0));
      (alpha % beta) / (arma::square(alpha + beta) % (alpha + beta + 1.0));
  // LINE NOTE: const arma::vec posterior_sd = arma::sqrt(arma::clamp(posterior_var, 0.0, 1.0));
  const arma::vec posterior_sd = arma::sqrt(arma::clamp(posterior_var, 0.0, 1.0));
  // LINE NOTE: const arma::vec lower_95 = arma::clamp(posterior_mean - 1.96 * posterior_sd, 0.0, 1.0);
  const arma::vec lower_95 = arma::clamp(posterior_mean - 1.96 * posterior_sd, 0.0, 1.0);
  // LINE NOTE: const arma::vec upper_95 = arma::clamp(posterior_mean + 1.96 * posterior_sd, 0.0, 1.0);
  const arma::vec upper_95 = arma::clamp(posterior_mean + 1.96 * posterior_sd, 0.0, 1.0);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: // Empirical value uses unresolved_value for unresolved outcomes.
    // Empirical value uses unresolved_value for unresolved outcomes.
    // LINE NOTE: if (summaries[i].allocation_count > 0) {
    if (summaries[i].allocation_count > 0) {
      // LINE NOTE: summaries[i].empirical_value =
      summaries[i].empirical_value =
          // LINE NOTE: (wins[i] + config.unresolved_value * unresolved[i]) /
          (wins[i] + config.unresolved_value * unresolved[i]) /
          // LINE NOTE: counts[i];
          counts[i];
    // LINE NOTE: } else {
    } else {
      // LINE NOTE: summaries[i].empirical_value = NA_REAL;
      summaries[i].empirical_value = NA_REAL;
    // LINE NOTE: }
    }

    // LINE NOTE: summaries[i].estimate = posterior_mean[i];
    summaries[i].estimate = posterior_mean[i];
    // LINE NOTE: summaries[i].posterior_sd = posterior_sd[i];
    summaries[i].posterior_sd = posterior_sd[i];
    // LINE NOTE: summaries[i].lower_95 = lower_95[i];
    summaries[i].lower_95 = lower_95[i];
    // LINE NOTE: summaries[i].upper_95 = upper_95[i];
    summaries[i].upper_95 = upper_95[i];
    // LINE NOTE: summaries[i].selection_score = posterior_mean[i];
    summaries[i].selection_score = posterior_mean[i];
  // LINE NOTE: }
  }

  // LINE NOTE: if (policy == AllocationPolicy::kUcb) {
  if (policy == AllocationPolicy::kUcb) {
    // LINE NOTE: // Selection score for UCB includes exploration bonus.
    // Selection score for UCB includes exploration bonus.
    // LINE NOTE: const double total_allocations = arma::accu(counts);
    const double total_allocations = arma::accu(counts);
    // LINE NOTE: const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    // LINE NOTE: const arma::vec bonus = config.ucb_exploration *
    const arma::vec bonus = config.ucb_exploration *
        // LINE NOTE: arma::sqrt(std::log(total_allocations + 1.0) / denom);
        arma::sqrt(std::log(total_allocations + 1.0) / denom);

    // LINE NOTE: for (int i = 0; i < n; ++i) {
    for (int i = 0; i < n; ++i) {
      // LINE NOTE: summaries[i].selection_score = posterior_mean[i] + bonus[i];
      summaries[i].selection_score = posterior_mean[i] + bonus[i];
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: if (!config.fast_diagnostics) {
  if (!config.fast_diagnostics) {
    // LINE NOTE: // Full output mode: compute posterior diagnostics.
    // Full output mode: compute posterior diagnostics.
    // LINE NOTE: compute_posterior_diagnostics(summaries, rng);
    compute_posterior_diagnostics(summaries, rng);
    // LINE NOTE: return;
    return;
  // LINE NOTE: }
  }

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: // Fast mode: skip expensive diagnostics.
    // Fast mode: skip expensive diagnostics.
    // LINE NOTE: summaries[i].prob_best = NA_REAL;
    summaries[i].prob_best = NA_REAL;
    // LINE NOTE: summaries[i].posterior_expected_regret = NA_REAL;
    summaries[i].posterior_expected_regret = NA_REAL;
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void update_interim_summary_fields(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void update_interim_summary_fields(
    // LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: const AllocationPolicy policy,
    const AllocationPolicy policy,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: const int total_allocations) {
    const int total_allocations) {
  // LINE NOTE: // Cheaper variant used for trace snapshots during allocation loop.
  // Cheaper variant used for trace snapshots during allocation loop.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: if (n == 0) {
  if (n == 0) {
    // LINE NOTE: return;
    return;
  // LINE NOTE: }
  }

  // LINE NOTE: arma::vec counts(n);
  arma::vec counts(n);
  // LINE NOTE: arma::vec wins(n);
  arma::vec wins(n);
  // LINE NOTE: arma::vec unresolved(n);
  arma::vec unresolved(n);
  // LINE NOTE: arma::vec alpha(n);
  arma::vec alpha(n);
  // LINE NOTE: arma::vec beta(n);
  arma::vec beta(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: counts[i] = static_cast<double>(summaries[i].allocation_count);
    counts[i] = static_cast<double>(summaries[i].allocation_count);
    // LINE NOTE: wins[i] = static_cast<double>(summaries[i].wins);
    wins[i] = static_cast<double>(summaries[i].wins);
    // LINE NOTE: unresolved[i] = static_cast<double>(summaries[i].unresolved);
    unresolved[i] = static_cast<double>(summaries[i].unresolved);
    // LINE NOTE: alpha[i] = summaries[i].alpha;
    alpha[i] = summaries[i].alpha;
    // LINE NOTE: beta[i] = summaries[i].beta;
    beta[i] = summaries[i].beta;
  // LINE NOTE: }
  }

  // LINE NOTE: const arma::vec posterior_mean = alpha / (alpha + beta);
  const arma::vec posterior_mean = alpha / (alpha + beta);
  // LINE NOTE: const arma::vec posterior_var =
  const arma::vec posterior_var =
      // LINE NOTE: (alpha % beta) / (arma::square(alpha + beta) % (alpha + beta + 1.0));
      (alpha % beta) / (arma::square(alpha + beta) % (alpha + beta + 1.0));
  // LINE NOTE: const arma::vec posterior_sd = arma::sqrt(arma::clamp(posterior_var, 0.0, 1.0));
  const arma::vec posterior_sd = arma::sqrt(arma::clamp(posterior_var, 0.0, 1.0));
  // LINE NOTE: const arma::vec lower_95 = arma::clamp(posterior_mean - 1.96 * posterior_sd, 0.0, 1.0);
  const arma::vec lower_95 = arma::clamp(posterior_mean - 1.96 * posterior_sd, 0.0, 1.0);
  // LINE NOTE: const arma::vec upper_95 = arma::clamp(posterior_mean + 1.96 * posterior_sd, 0.0, 1.0);
  const arma::vec upper_95 = arma::clamp(posterior_mean + 1.96 * posterior_sd, 0.0, 1.0);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: if (summaries[i].allocation_count > 0) {
    if (summaries[i].allocation_count > 0) {
      // LINE NOTE: summaries[i].empirical_value =
      summaries[i].empirical_value =
          // LINE NOTE: (wins[i] + config.unresolved_value * unresolved[i]) /
          (wins[i] + config.unresolved_value * unresolved[i]) /
          // LINE NOTE: counts[i];
          counts[i];
    // LINE NOTE: } else {
    } else {
      // LINE NOTE: summaries[i].empirical_value = NA_REAL;
      summaries[i].empirical_value = NA_REAL;
    // LINE NOTE: }
    }

    // LINE NOTE: summaries[i].estimate = posterior_mean[i];
    summaries[i].estimate = posterior_mean[i];
    // LINE NOTE: summaries[i].posterior_sd = posterior_sd[i];
    summaries[i].posterior_sd = posterior_sd[i];
    // LINE NOTE: summaries[i].lower_95 = lower_95[i];
    summaries[i].lower_95 = lower_95[i];
    // LINE NOTE: summaries[i].upper_95 = upper_95[i];
    summaries[i].upper_95 = upper_95[i];
    // LINE NOTE: summaries[i].selection_score = posterior_mean[i];
    summaries[i].selection_score = posterior_mean[i];
  // LINE NOTE: }
  }

  // LINE NOTE: if (policy == AllocationPolicy::kUcb) {
  if (policy == AllocationPolicy::kUcb) {
    // LINE NOTE: const double total = static_cast<double>(std::max(total_allocations, 1));
    const double total = static_cast<double>(std::max(total_allocations, 1));
    // LINE NOTE: const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    const arma::vec denom = arma::max(counts, arma::ones<arma::vec>(n));
    // LINE NOTE: const arma::vec bonus = config.ucb_exploration *
    const arma::vec bonus = config.ucb_exploration *
        // LINE NOTE: arma::sqrt(std::log(total + 1.0) / denom);
        arma::sqrt(std::log(total + 1.0) / denom);

    // LINE NOTE: for (int i = 0; i < n; ++i) {
    for (int i = 0; i < n; ++i) {
      // LINE NOTE: summaries[i].selection_score = posterior_mean[i] + bonus[i];
      summaries[i].selection_score = posterior_mean[i] + bonus[i];
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: int current_leader_index(const std::vector<backgammonr::ActionEvaluationSummary>& summaries) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
int current_leader_index(const std::vector<backgammonr::ActionEvaluationSummary>& summaries) {
  // LINE NOTE: // Pick current leader by estimate, tie-break by higher sample count.
  // Pick current leader by estimate, tie-break by higher sample count.
  // LINE NOTE: if (summaries.empty()) {
  if (summaries.empty()) {
    // LINE NOTE: return NA_INTEGER;
    return NA_INTEGER;
  // LINE NOTE: }
  }

  // LINE NOTE: int leader = 0;
  int leader = 0;
  // LINE NOTE: for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
  for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
    // LINE NOTE: if (summaries[i].estimate > summaries[leader].estimate + kTieTolerance) {
    if (summaries[i].estimate > summaries[leader].estimate + kTieTolerance) {
      // LINE NOTE: leader = i;
      leader = i;
      // LINE NOTE: continue;
      continue;
    // LINE NOTE: }
    }

    // LINE NOTE: if (std::fabs(summaries[i].estimate - summaries[leader].estimate) <= kTieTolerance &&
    if (std::fabs(summaries[i].estimate - summaries[leader].estimate) <= kTieTolerance &&
        // LINE NOTE: summaries[i].allocation_count > summaries[leader].allocation_count) {
        summaries[i].allocation_count > summaries[leader].allocation_count) {
      // LINE NOTE: leader = i;
      leader = i;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: return leader;
  return leader;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void append_trace_snapshot(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void append_trace_snapshot(
    // LINE NOTE: std::vector<AllocationTraceRow>& trace_rows,
    std::vector<AllocationTraceRow>& trace_rows,
    // LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: const AllocationPolicy policy,
    const AllocationPolicy policy,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: const int checkpoint,
    const int checkpoint,
    // LINE NOTE: const int selected_candidate) {
    const int selected_candidate) {
  // LINE NOTE: // Refresh per-candidate fields as of this checkpoint.
  // Refresh per-candidate fields as of this checkpoint.
  // LINE NOTE: update_interim_summary_fields(summaries, policy, config, checkpoint);
  update_interim_summary_fields(summaries, policy, config, checkpoint);
  // LINE NOTE: const int leader_pos = current_leader_index(summaries);
  const int leader_pos = current_leader_index(summaries);
  // LINE NOTE: const int leader_index = leader_pos == NA_INTEGER
  const int leader_index = leader_pos == NA_INTEGER
      // LINE NOTE: ? NA_INTEGER
      ? NA_INTEGER
      // LINE NOTE: : summaries[leader_pos].candidate_index;
      : summaries[leader_pos].candidate_index;

  // LINE NOTE: for (const backgammonr::ActionEvaluationSummary& summary : summaries) {
  for (const backgammonr::ActionEvaluationSummary& summary : summaries) {
    // LINE NOTE: // Emit one trace row per candidate at this checkpoint.
    // Emit one trace row per candidate at this checkpoint.
    // LINE NOTE: AllocationTraceRow row;
    AllocationTraceRow row;
    // LINE NOTE: row.checkpoint = checkpoint;
    row.checkpoint = checkpoint;
    // LINE NOTE: row.selected_candidate = selected_candidate;
    row.selected_candidate = selected_candidate;
    // LINE NOTE: row.leader_index = leader_index;
    row.leader_index = leader_index;
    // LINE NOTE: row.candidate_index = summary.candidate_index;
    row.candidate_index = summary.candidate_index;
    // LINE NOTE: row.allocation_count = summary.allocation_count;
    row.allocation_count = summary.allocation_count;
    // LINE NOTE: row.wins = summary.wins;
    row.wins = summary.wins;
    // LINE NOTE: row.losses = summary.losses;
    row.losses = summary.losses;
    // LINE NOTE: row.unresolved = summary.unresolved;
    row.unresolved = summary.unresolved;
    // LINE NOTE: row.empirical_value = summary.empirical_value;
    row.empirical_value = summary.empirical_value;
    // LINE NOTE: row.alpha = summary.alpha;
    row.alpha = summary.alpha;
    // LINE NOTE: row.beta = summary.beta;
    row.beta = summary.beta;
    // LINE NOTE: row.estimate = summary.estimate;
    row.estimate = summary.estimate;
    // LINE NOTE: row.posterior_sd = summary.posterior_sd;
    row.posterior_sd = summary.posterior_sd;
    // LINE NOTE: row.lower_95 = summary.lower_95;
    row.lower_95 = summary.lower_95;
    // LINE NOTE: row.upper_95 = summary.upper_95;
    row.upper_95 = summary.upper_95;
    // LINE NOTE: row.selection_score = summary.selection_score;
    row.selection_score = summary.selection_score;
    // LINE NOTE: trace_rows.push_back(row);
    trace_rows.push_back(row);
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: arma::vec ocba_target_allocations(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
arma::vec ocba_target_allocations(
    // LINE NOTE: const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: const int next_total_allocations) {
    const int next_total_allocations) {
  // **WHAT IT'S DOING (DETAILED):** Computes a continuous OCBA-inspired target
  // allocation profile from posterior means and posterior standard deviations.
  // Arms with smaller mean gaps and/or larger uncertainty get larger target mass.
  // **IN PLAIN ENGLISH:** Give extra simulations to moves that are both promising
  // and still uncertain, because those are the ones that can still change the
  // final recommendation.
  // LINE NOTE: // OCBA target-allocation approximation from posterior means/variances.
  // OCBA target-allocation approximation from posterior means/variances.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: arma::vec target(n, arma::fill::zeros);
  arma::vec target(n, arma::fill::zeros);
  // LINE NOTE: if (n == 0) {
  if (n == 0) {
    // LINE NOTE: return target;
    return target;
  // LINE NOTE: }
  }
  // LINE NOTE: if (n == 1) {
  if (n == 1) {
    // LINE NOTE: target[0] = static_cast<double>(next_total_allocations);
    target[0] = static_cast<double>(next_total_allocations);
    // LINE NOTE: return target;
    return target;
  // LINE NOTE: }
  }

  // LINE NOTE: arma::vec mu(n);
  arma::vec mu(n);
  // LINE NOTE: arma::vec sigma(n);
  arma::vec sigma(n);
  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: const double alpha = summaries[i].alpha;
    const double alpha = summaries[i].alpha;
    // LINE NOTE: const double beta = summaries[i].beta;
    const double beta = summaries[i].beta;
    // LINE NOTE: mu[i] = alpha / (alpha + beta);
    mu[i] = alpha / (alpha + beta);
    // LINE NOTE: const double var = (alpha * beta) /
    const double var = (alpha * beta) /
        // LINE NOTE: ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0));
        ((alpha + beta) * (alpha + beta) * (alpha + beta + 1.0));
    // LINE NOTE: sigma[i] = std::sqrt(std::max(var, 1e-12));
    sigma[i] = std::sqrt(std::max(var, 1e-12));
  // LINE NOTE: }
  }

  // LINE NOTE: int best = 0;
  int best = 0;
  // LINE NOTE: for (int i = 1; i < n; ++i) {
  for (int i = 1; i < n; ++i) {
    // LINE NOTE: if (mu[i] > mu[best]) {
    if (mu[i] > mu[best]) {
      // LINE NOTE: best = i;
      best = i;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: arma::vec ratio(n, arma::fill::zeros);
  arma::vec ratio(n, arma::fill::zeros);
  // LINE NOTE: const double mu_best = mu[best];
  const double mu_best = mu[best];
  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: if (i == best) {
    if (i == best) {
      // LINE NOTE: continue;
      continue;
    // LINE NOTE: }
    }
    // LINE NOTE: const double gap = std::max(mu_best - mu[i], 1e-6);
    const double gap = std::max(mu_best - mu[i], 1e-6);
    // LINE NOTE: ratio[i] = (sigma[i] * sigma[i]) / (gap * gap);
    ratio[i] = (sigma[i] * sigma[i]) / (gap * gap);
  // LINE NOTE: }
  }

  // LINE NOTE: double sum_term = 0.0;
  double sum_term = 0.0;
  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: if (i == best) {
    if (i == best) {
      // LINE NOTE: continue;
      continue;
    // LINE NOTE: }
    }
    // LINE NOTE: sum_term += (ratio[i] * ratio[i]) / std::max(sigma[i] * sigma[i], 1e-12);
    sum_term += (ratio[i] * ratio[i]) / std::max(sigma[i] * sigma[i], 1e-12);
  // LINE NOTE: }
  }
  // LINE NOTE: ratio[best] = std::max(sigma[best] * std::sqrt(std::max(sum_term, 1e-12)), 1e-12);
  ratio[best] = std::max(sigma[best] * std::sqrt(std::max(sum_term, 1e-12)), 1e-12);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: ratio[i] = std::max(ratio[i], 1e-12);
    ratio[i] = std::max(ratio[i], 1e-12);
  // LINE NOTE: }
  }

  // LINE NOTE: const double ratio_sum = arma::accu(ratio);
  const double ratio_sum = arma::accu(ratio);
  // LINE NOTE: if (ratio_sum <= 0.0 || !std::isfinite(ratio_sum)) {
  if (ratio_sum <= 0.0 || !std::isfinite(ratio_sum)) {
    // LINE NOTE: target.fill(static_cast<double>(next_total_allocations) / static_cast<double>(n));
    target.fill(static_cast<double>(next_total_allocations) / static_cast<double>(n));
    // LINE NOTE: return target;
    return target;
  // LINE NOTE: }
  }

  // LINE NOTE: target = ratio / ratio_sum * static_cast<double>(next_total_allocations);
  target = ratio / ratio_sum * static_cast<double>(next_total_allocations);
  // LINE NOTE: return target;
  return target;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: int choose_next_candidate(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
int choose_next_candidate(
    // LINE NOTE: const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    const std::vector<backgammonr::ActionEvaluationSummary>& summaries,
    // LINE NOTE: const AllocationPolicy policy,
    const AllocationPolicy policy,
    // LINE NOTE: const int step,
    const int step,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Central policy switch for one-step budget
  // allocation. Given the current posterior state, this chooses exactly one
  // candidate to receive the next rollout sample.
  // **IN PLAIN ENGLISH:** This is the "who gets the next simulation?" decision.
  // Different methods answer that question differently (equal, UCB, Thompson,
  // TTTS, OCBA), but they all pass through this function.
  // LINE NOTE: // Policy-specific next-arm selection in a fixed-budget simulation problem.
  // Policy-specific next-arm selection in a fixed-budget simulation problem.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());

  // LINE NOTE: if (policy == AllocationPolicy::kEqual) {
  if (policy == AllocationPolicy::kEqual) {
    // LINE NOTE: // Deterministic round-robin.
    // Deterministic round-robin.
    // LINE NOTE: return n == 0 ? -1 : (step % n);
    return n == 0 ? -1 : (step % n);
  // LINE NOTE: }
  }

  // LINE NOTE: if (n == 0) {
  if (n == 0) {
    // LINE NOTE: throw std::range_error("Cannot choose from an empty candidate set.");
    throw std::range_error("Cannot choose from an empty candidate set.");
  // LINE NOTE: }
  }

  // LINE NOTE: int best_index = 0;
  int best_index = 0;
  // LINE NOTE: double best_score = -std::numeric_limits<double>::infinity();
  double best_score = -std::numeric_limits<double>::infinity();
  // LINE NOTE: int best_allocations = std::numeric_limits<int>::max();
  int best_allocations = std::numeric_limits<int>::max();
  // LINE NOTE: const double ucb_log_term = std::log(static_cast<double>(step) + 2.0);
  const double ucb_log_term = std::log(static_cast<double>(step) + 2.0);

  // LINE NOTE: if (policy == AllocationPolicy::kOcba) {
  if (policy == AllocationPolicy::kOcba) {
    // LINE NOTE: // Allocate toward largest deficit from OCBA target allocations.
    // Allocate toward largest deficit from OCBA target allocations.
    // LINE NOTE: const arma::vec target = ocba_target_allocations(summaries, step + 1);
    const arma::vec target = ocba_target_allocations(summaries, step + 1);
    // LINE NOTE: int chosen = 0;
    int chosen = 0;
    // LINE NOTE: double best_deficit = target[0] - static_cast<double>(summaries[0].allocation_count);
    double best_deficit = target[0] - static_cast<double>(summaries[0].allocation_count);
    // LINE NOTE: for (int i = 1; i < n; ++i) {
    for (int i = 1; i < n; ++i) {
      // LINE NOTE: const double deficit = target[i] - static_cast<double>(summaries[i].allocation_count);
      const double deficit = target[i] - static_cast<double>(summaries[i].allocation_count);
      // LINE NOTE: if (deficit > best_deficit + kTieTolerance) {
      if (deficit > best_deficit + kTieTolerance) {
        // LINE NOTE: chosen = i;
        chosen = i;
        // LINE NOTE: best_deficit = deficit;
        best_deficit = deficit;
        // LINE NOTE: continue;
        continue;
      // LINE NOTE: }
      }
      // LINE NOTE: if (std::fabs(deficit - best_deficit) <= kTieTolerance &&
      if (std::fabs(deficit - best_deficit) <= kTieTolerance &&
          // LINE NOTE: summaries[i].allocation_count < summaries[chosen].allocation_count) {
          summaries[i].allocation_count < summaries[chosen].allocation_count) {
        // LINE NOTE: chosen = i;
        chosen = i;
        // LINE NOTE: best_deficit = deficit;
        best_deficit = deficit;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
    // LINE NOTE: return chosen;
    return chosen;
  // LINE NOTE: }
  }

  // LINE NOTE: if (policy == AllocationPolicy::kTtts) {
  if (policy == AllocationPolicy::kTtts) {
    // **WHAT IT'S DOING (DETAILED):** Top-Two Thompson Sampling (TTTS):
    // sample a first winner I, then with probability beta play I, otherwise
    // sample until we get a distinct winner J and play J.
    // **IN PLAIN ENGLISH:** TTTS deliberately gives some budget to the runner-up
    // under posterior uncertainty so we do not over-commit too early.
    // LINE NOTE: // Top-Two Thompson Sampling:
    // Top-Two Thompson Sampling:
    // LINE NOTE: // 1) draw posterior sample and pick top action I,
    // 1) draw posterior sample and pick top action I,
    // LINE NOTE: // 2) with probability beta play I,
    // 2) with probability beta play I,
    // LINE NOTE: // 3) otherwise draw again until top action J != I and play J.
    // 3) otherwise draw again until top action J != I and play J.
    // LINE NOTE: auto draw_thompson_winner = [&](void) -> int {
    auto draw_thompson_winner = [&](void) -> int {
      // LINE NOTE: int winner = 0;
      int winner = 0;
      // LINE NOTE: double best = -std::numeric_limits<double>::infinity();
      double best = -std::numeric_limits<double>::infinity();
      // LINE NOTE: for (int i = 0; i < n; ++i) {
      for (int i = 0; i < n; ++i) {
        // LINE NOTE: const double draw = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
        const double draw = sample_beta_distribution(summaries[i].alpha, summaries[i].beta, rng);
        // LINE NOTE: if (draw > best) {
        if (draw > best) {
          // LINE NOTE: best = draw;
          best = draw;
          // LINE NOTE: winner = i;
          winner = i;
        // LINE NOTE: }
        }
      // LINE NOTE: }
      }
      // LINE NOTE: return winner;
      return winner;
    // LINE NOTE: };
    };

    // LINE NOTE: const int top1 = draw_thompson_winner();
    const int top1 = draw_thompson_winner();
    // LINE NOTE: if (n == 1) {
    if (n == 1) {
      // LINE NOTE: return top1;
      return top1;
    // LINE NOTE: }
    }

    // LINE NOTE: double beta = config.ucb_exploration;
    double beta = config.ucb_exploration;
    // LINE NOTE: if (!(beta > 0.0 && beta <= 1.0) || !std::isfinite(beta)) {
    if (!(beta > 0.0 && beta <= 1.0) || !std::isfinite(beta)) {
      // LINE NOTE: beta = 0.5;
      beta = 0.5;
    // LINE NOTE: }
    }

    // LINE NOTE: std::uniform_real_distribution<double> coin(0.0, 1.0);
    std::uniform_real_distribution<double> coin(0.0, 1.0);
    // LINE NOTE: if (coin(rng) <= beta) {
    if (coin(rng) <= beta) {
      // LINE NOTE: return top1;
      return top1;
    // LINE NOTE: }
    }

    // LINE NOTE: for (int attempt = 0; attempt < 64; ++attempt) {
    for (int attempt = 0; attempt < 64; ++attempt) {
      // LINE NOTE: const int top2 = draw_thompson_winner();
      const int top2 = draw_thompson_winner();
      // LINE NOTE: if (top2 != top1) {
      if (top2 != top1) {
        // LINE NOTE: return top2;
        return top2;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: // Rare fallback if repeated posterior draws tie to the same winner.
    // Rare fallback if repeated posterior draws tie to the same winner.
    // LINE NOTE: int fallback = -1;
    int fallback = -1;
    // LINE NOTE: double best_mean = -std::numeric_limits<double>::infinity();
    double best_mean = -std::numeric_limits<double>::infinity();
    // LINE NOTE: for (int i = 0; i < n; ++i) {
    for (int i = 0; i < n; ++i) {
      // LINE NOTE: if (i == top1) {
      if (i == top1) {
        // LINE NOTE: continue;
        continue;
      // LINE NOTE: }
      }
      // LINE NOTE: const double mean = summaries[i].alpha / (summaries[i].alpha + summaries[i].beta);
      const double mean = summaries[i].alpha / (summaries[i].alpha + summaries[i].beta);
      // LINE NOTE: if (mean > best_mean) {
      if (mean > best_mean) {
        // LINE NOTE: best_mean = mean;
        best_mean = mean;
        // LINE NOTE: fallback = i;
        fallback = i;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
    // LINE NOTE: return fallback >= 0 ? fallback : top1;
    return fallback >= 0 ? fallback : top1;
  // LINE NOTE: }
  }

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: const backgammonr::ActionEvaluationSummary& summary = summaries[i];
    const backgammonr::ActionEvaluationSummary& summary = summaries[i];
    // LINE NOTE: const double posterior_mean = summary.alpha / (summary.alpha + summary.beta);
    const double posterior_mean = summary.alpha / (summary.alpha + summary.beta);
    // LINE NOTE: double score = posterior_mean;
    double score = posterior_mean;

    // LINE NOTE: if (policy == AllocationPolicy::kUcb) {
    if (policy == AllocationPolicy::kUcb) {
      // LINE NOTE: // UCB score = posterior mean + exploration bonus.
      // UCB score = posterior mean + exploration bonus.
      // LINE NOTE: const double denom = static_cast<double>(std::max(summary.allocation_count, 1));
      const double denom = static_cast<double>(std::max(summary.allocation_count, 1));
      // LINE NOTE: score += config.ucb_exploration * std::sqrt(ucb_log_term / denom);
      score += config.ucb_exploration * std::sqrt(ucb_log_term / denom);
    // LINE NOTE: } else if (policy == AllocationPolicy::kThompson) {
    } else if (policy == AllocationPolicy::kThompson) {
      // LINE NOTE: // Thompson score = one posterior sample draw.
      // Thompson score = one posterior sample draw.
      // LINE NOTE: score = sample_beta_distribution(summary.alpha, summary.beta, rng);
      score = sample_beta_distribution(summary.alpha, summary.beta, rng);
    // LINE NOTE: }
    }

    // LINE NOTE: if (score_beats_incumbent(score, summary.allocation_count, best_score, best_allocations)) {
    if (score_beats_incumbent(score, summary.allocation_count, best_score, best_allocations)) {
      // LINE NOTE: best_index = i;
      best_index = i;
      // LINE NOTE: best_score = score;
      best_score = score;
      // LINE NOTE: best_allocations = summary.allocation_count;
      best_allocations = summary.allocation_count;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: return best_index;
  return best_index;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary> evaluate_with_optional_trace(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::vector<backgammonr::ActionEvaluationSummary> evaluate_with_optional_trace(
    // LINE NOTE: const backgammonr::BoardState& board,
    const backgammonr::BoardState& board,
    // LINE NOTE: const std::vector<backgammonr::MoveSequence>& legal_moves,
    const std::vector<backgammonr::MoveSequence>& legal_moves,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const backgammonr::RolloutConfig& config,
    const backgammonr::RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng,
    std::mt19937& rng,
    // LINE NOTE: const int trace_every,
    const int trace_every,
    // LINE NOTE: std::vector<AllocationTraceRow>* trace_rows) {
    std::vector<AllocationTraceRow>* trace_rows) {
  // **WHAT IT'S DOING (DETAILED):** End-to-end fixed-budget evaluator with
  // optional trace logging.
  // Phase A: validate config and canonicalize method.
  // Phase B: collapse equivalent actions and initialize posterior summaries.
  // Phase C: optional warm-start allocations for adaptive methods.
  // Phase D: main adaptive allocation loop until budget is exhausted.
  // Phase E: finalize posterior summaries/diagnostics and return.
  // **IN PLAIN ENGLISH:** This is the main experiment engine that spends a
  // limited simulation budget and records how estimates and uncertainty evolve.
  // LINE NOTE: // Core allocation engine used by all public wrappers.
  // Core allocation engine used by all public wrappers.
  // LINE NOTE: backgammonr::validate_rollout_config(config);
  backgammonr::validate_rollout_config(config);
  // LINE NOTE: if (trace_rows != nullptr && trace_every < 1) {
  if (trace_rows != nullptr && trace_every < 1) {
    // LINE NOTE: throw std::range_error("`trace_every` must be at least 1.");
    throw std::range_error("`trace_every` must be at least 1.");
  // LINE NOTE: }
  }

  // LINE NOTE: const std::string canonical_method = backgammonr::canonicalize_allocation_method(method);
  const std::string canonical_method = backgammonr::canonicalize_allocation_method(method);
  // LINE NOTE: const AllocationPolicy policy = parse_allocation_policy(canonical_method);
  const AllocationPolicy policy = parse_allocation_policy(canonical_method);

  // LINE NOTE: if (legal_moves.empty()) {
  if (legal_moves.empty()) {
    // LINE NOTE: throw std::range_error("Cannot evaluate an empty legal-move set.");
    throw std::range_error("Cannot evaluate an empty legal-move set.");
  // LINE NOTE: }
  }

  // LINE NOTE: const std::vector<CollapsedCandidate> collapsed =
  const std::vector<CollapsedCandidate> collapsed =
      // LINE NOTE: collapse_equivalent_candidates(board, legal_moves);
      collapse_equivalent_candidates(board, legal_moves);

  // LINE NOTE: std::vector<backgammonr::ActionEvaluationSummary> summaries(collapsed.size());
  std::vector<backgammonr::ActionEvaluationSummary> summaries(collapsed.size());
  // LINE NOTE: std::vector<backgammonr::BoardState> candidate_boards;
  std::vector<backgammonr::BoardState> candidate_boards;
  // LINE NOTE: candidate_boards.reserve(collapsed.size());
  candidate_boards.reserve(collapsed.size());
  // LINE NOTE: std::vector<int> acting_players;
  std::vector<int> acting_players;
  // LINE NOTE: acting_players.reserve(collapsed.size());
  acting_players.reserve(collapsed.size());
  // LINE NOTE: std::vector<int> stratification_offsets(collapsed.size(), 0);
  std::vector<int> stratification_offsets(collapsed.size(), 0);

  // LINE NOTE: for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
  for (int i = 0; i < static_cast<int>(collapsed.size()); ++i) {
    // LINE NOTE: // Initialize one summary row per collapsed candidate state.
    // Initialize one summary row per collapsed candidate state.
    // LINE NOTE: candidate_boards.push_back(collapsed[i].board_after);
    candidate_boards.push_back(collapsed[i].board_after);
    // LINE NOTE: acting_players.push_back(collapsed[i].acting_player);
    acting_players.push_back(collapsed[i].acting_player);
    // LINE NOTE: summaries[i].candidate_index = collapsed[i].representative_index + 1;
    summaries[i].candidate_index = collapsed[i].representative_index + 1;
    // LINE NOTE: summaries[i].n_equivalent_sequences = collapsed[i].n_equivalent;
    summaries[i].n_equivalent_sequences = collapsed[i].n_equivalent;
    // LINE NOTE: summaries[i].alpha = config.prior_alpha;
    summaries[i].alpha = config.prior_alpha;
    // LINE NOTE: summaries[i].beta = config.prior_beta;
    summaries[i].beta = config.prior_beta;
    // LINE NOTE: summaries[i].estimate = config.prior_alpha / (config.prior_alpha + config.prior_beta);
    summaries[i].estimate = config.prior_alpha / (config.prior_alpha + config.prior_beta);
    // LINE NOTE: summaries[i].selection_score = summaries[i].estimate;
    summaries[i].selection_score = summaries[i].estimate;
  // LINE NOTE: }
  }

  // LINE NOTE: if (config.dice_mode != "iid" && !config.crn) {
  if (config.dice_mode != "iid" && !config.crn) {
    // LINE NOTE: // Randomize candidate-specific stratification offsets when not using CRN.
    // Randomize candidate-specific stratification offsets when not using CRN.
    // LINE NOTE: const int n_outcomes = config.dice_mode == "stratified_first_two_rolls" ? 441 : 21;
    const int n_outcomes = config.dice_mode == "stratified_first_two_rolls" ? 441 : 21;
    // LINE NOTE: std::uniform_int_distribution<int> offset_dist(0, n_outcomes - 1);
    std::uniform_int_distribution<int> offset_dist(0, n_outcomes - 1);
    // LINE NOTE: for (int i = 0; i < static_cast<int>(stratification_offsets.size()); ++i) {
    for (int i = 0; i < static_cast<int>(stratification_offsets.size()); ++i) {
      // LINE NOTE: stratification_offsets[i] = offset_dist(rng);
      stratification_offsets[i] = offset_dist(rng);
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: const std::uint32_t crn_base_seed = config.use_crn_seed
  const std::uint32_t crn_base_seed = config.use_crn_seed
      // LINE NOTE: ? static_cast<std::uint32_t>(config.crn_seed)
      ? static_cast<std::uint32_t>(config.crn_seed)
      // LINE NOTE: : static_cast<std::uint32_t>(rng());
      : static_cast<std::uint32_t>(rng());

  // LINE NOTE: int step = 0;
  int step = 0;
  // LINE NOTE: // Snapshot helper: emit checkpoints only when requested.
  // Snapshot helper: emit checkpoints only when requested.
  // LINE NOTE: auto maybe_trace = [&](const int selected_candidate) {
  auto maybe_trace = [&](const int selected_candidate) {
    // LINE NOTE: if (trace_rows == nullptr) {
    if (trace_rows == nullptr) {
      // LINE NOTE: return;
      return;
    // LINE NOTE: }
    }
    // LINE NOTE: if (step % trace_every == 0 || step == config.budget) {
    if (step % trace_every == 0 || step == config.budget) {
      // LINE NOTE: append_trace_snapshot(*trace_rows, summaries, policy, config, step, selected_candidate);
      append_trace_snapshot(*trace_rows, summaries, policy, config, step, selected_candidate);
    // LINE NOTE: }
    }
  // LINE NOTE: };
  };

  // LINE NOTE: if (policy != AllocationPolicy::kEqual && config.initial_allocations > 0) {
  if (policy != AllocationPolicy::kEqual && config.initial_allocations > 0) {
    // LINE NOTE: // Warm-start adaptive methods with round-robin initial allocations.
    // Warm-start adaptive methods with round-robin initial allocations.
    // LINE NOTE: for (int round = 0; round < config.initial_allocations && step < config.budget; ++round) {
    for (int round = 0; round < config.initial_allocations && step < config.budget; ++round) {
      // LINE NOTE: for (int i = 0; i < static_cast<int>(candidate_boards.size()) && step < config.budget; ++i) {
      for (int i = 0; i < static_cast<int>(candidate_boards.size()) && step < config.budget; ++i) {
        // LINE NOTE: const int sample_index = summaries[i].allocation_count + 1;
        const int sample_index = summaries[i].allocation_count + 1;
        // LINE NOTE: const int offset = config.crn ? 0 : stratification_offsets[i];
        const int offset = config.crn ? 0 : stratification_offsets[i];
        // LINE NOTE: const ForcedRollSchedule forced_rolls =
        const ForcedRollSchedule forced_rolls =
            // LINE NOTE: scheduled_forced_rolls(config.dice_mode, sample_index, offset);
            scheduled_forced_rolls(config.dice_mode, sample_index, offset);
        // LINE NOTE: std::mt19937* rollout_rng = &rng;
        std::mt19937* rollout_rng = &rng;
        // LINE NOTE: std::mt19937 crn_rng;
        std::mt19937 crn_rng;
        // LINE NOTE: if (config.crn) {
        if (config.crn) {
          // LINE NOTE: // Re-seed rollout RNG deterministically per sample index.
          // Re-seed rollout RNG deterministically per sample index.
          // LINE NOTE: crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
          crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
          // LINE NOTE: rollout_rng = &crn_rng;
          rollout_rng = &crn_rng;
        // LINE NOTE: }
        }
        // LINE NOTE: const RolloutOutcome outcome = single_rollout_outcome(
        const RolloutOutcome outcome = single_rollout_outcome(
            // LINE NOTE: candidate_boards[i],
            candidate_boards[i],
            // LINE NOTE: acting_players[i],
            acting_players[i],
            // LINE NOTE: config,
            config,
            // LINE NOTE: *rollout_rng,
            *rollout_rng,
            // LINE NOTE: forced_rolls);
            forced_rolls);
        // LINE NOTE: update_summary(summaries[i], outcome, config);
        update_summary(summaries[i], outcome, config);
        // LINE NOTE: step += 1;
        step += 1;
        // LINE NOTE: maybe_trace(summaries[i].candidate_index);
        maybe_trace(summaries[i].candidate_index);
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: while (step < config.budget) {
  while (step < config.budget) {
    // Each iteration consumes exactly one rollout from the remaining budget.
    // The selected index comes from the chosen allocation policy and current
    // posterior state.
    // LINE NOTE: // Policy decides which candidate gets next rollout.
    // Policy decides which candidate gets next rollout.
    // LINE NOTE: const int chosen_index =
    const int chosen_index =
        // LINE NOTE: choose_next_candidate(summaries, policy, step, config, rng);
        choose_next_candidate(summaries, policy, step, config, rng);
    // LINE NOTE: const int sample_index = summaries[chosen_index].allocation_count + 1;
    const int sample_index = summaries[chosen_index].allocation_count + 1;
    // LINE NOTE: const int offset = config.crn ? 0 : stratification_offsets[chosen_index];
    const int offset = config.crn ? 0 : stratification_offsets[chosen_index];
    // LINE NOTE: const ForcedRollSchedule forced_rolls =
    const ForcedRollSchedule forced_rolls =
        // LINE NOTE: scheduled_forced_rolls(config.dice_mode, sample_index, offset);
        scheduled_forced_rolls(config.dice_mode, sample_index, offset);
    // LINE NOTE: std::mt19937* rollout_rng = &rng;
    std::mt19937* rollout_rng = &rng;
    // LINE NOTE: std::mt19937 crn_rng;
    std::mt19937 crn_rng;
    // LINE NOTE: if (config.crn) {
    if (config.crn) {
      // LINE NOTE: crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
      crn_rng.seed(stable_rollout_seed(crn_base_seed, sample_index, 0));
      // LINE NOTE: rollout_rng = &crn_rng;
      rollout_rng = &crn_rng;
    // LINE NOTE: }
    }
    // LINE NOTE: const RolloutOutcome outcome = single_rollout_outcome(
    const RolloutOutcome outcome = single_rollout_outcome(
        // LINE NOTE: candidate_boards[chosen_index],
        candidate_boards[chosen_index],
        // LINE NOTE: acting_players[chosen_index],
        acting_players[chosen_index],
        // LINE NOTE: config,
        config,
        // LINE NOTE: *rollout_rng,
        *rollout_rng,
        // LINE NOTE: forced_rolls);
        forced_rolls);
    // LINE NOTE: update_summary(summaries[chosen_index], outcome, config);
    update_summary(summaries[chosen_index], outcome, config);
    // LINE NOTE: step += 1;
    step += 1;
    // LINE NOTE: maybe_trace(summaries[chosen_index].candidate_index);
    maybe_trace(summaries[chosen_index].candidate_index);
  // LINE NOTE: }
  }

  // LINE NOTE: finalize_summaries(summaries, policy, config, rng);
  finalize_summaries(summaries, policy, config, rng);
  // LINE NOTE: return summaries;
  return summaries;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: }  // namespace
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
}  // namespace

// LINE NOTE: namespace backgammonr {
// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace backgammonr {

// LINE NOTE: // Supported public method identifiers (including compatibility aliases).
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Supported public method identifiers (including compatibility aliases).
// LINE NOTE: bool is_supported_allocation_method(const std::string& method) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool is_supported_allocation_method(const std::string& method) {
  // LINE NOTE: return method == "equal" ||
  return method == "equal" ||
      // LINE NOTE: method == "greedy" ||
      method == "greedy" ||
      // LINE NOTE: method == "ucb" ||
      method == "ucb" ||
      // LINE NOTE: method == "ocba" ||
      method == "ocba" ||
      // LINE NOTE: method == "thompson" ||
      method == "thompson" ||
      // LINE NOTE: method == "ttts" ||
      method == "ttts" ||
      // LINE NOTE: method == "rollout" ||
      method == "rollout" ||
      // LINE NOTE: method == "equal_rollout" ||
      method == "equal_rollout" ||
      // LINE NOTE: method == "greedy_rollout" ||
      method == "greedy_rollout" ||
      // LINE NOTE: method == "ucb_rollout" ||
      method == "ucb_rollout" ||
      // LINE NOTE: method == "ocba_rollout" ||
      method == "ocba_rollout" ||
      // LINE NOTE: method == "thompson_rollout" ||
      method == "thompson_rollout" ||
      // LINE NOTE: method == "ttts_rollout";
      method == "ttts_rollout";
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: void validate_allocation_method(const std::string& method) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void validate_allocation_method(const std::string& method) {
  // LINE NOTE: if (!is_supported_allocation_method(method)) {
  if (!is_supported_allocation_method(method)) {
    // LINE NOTE: throw std::range_error(
    throw std::range_error(
        // LINE NOTE: "`method` must be one of \"equal\", \"greedy\", \"ucb\", \"ocba\", \"thompson\", \"ttts\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"ocba_rollout\", \"thompson_rollout\", or \"ttts_rollout\".");
        "`method` must be one of \"equal\", \"greedy\", \"ucb\", \"ocba\", \"thompson\", \"ttts\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"ocba_rollout\", \"thompson_rollout\", or \"ttts_rollout\".");
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::string canonicalize_allocation_method(const std::string& method) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::string canonicalize_allocation_method(const std::string& method) {
  // LINE NOTE: // Canonicalize aliases so downstream switches only handle one spelling.
  // Canonicalize aliases so downstream switches only handle one spelling.
  // LINE NOTE: validate_allocation_method(method);
  validate_allocation_method(method);

  // LINE NOTE: if (method == "equal" || method == "rollout" || method == "equal_rollout") {
  if (method == "equal" || method == "rollout" || method == "equal_rollout") {
    // LINE NOTE: return "equal";
    return "equal";
  // LINE NOTE: }
  }

  // LINE NOTE: if (method == "greedy" || method == "greedy_rollout") {
  if (method == "greedy" || method == "greedy_rollout") {
    // LINE NOTE: return "greedy";
    return "greedy";
  // LINE NOTE: }
  }

  // LINE NOTE: if (method == "ucb" || method == "ucb_rollout") {
  if (method == "ucb" || method == "ucb_rollout") {
    // LINE NOTE: return "ucb";
    return "ucb";
  // LINE NOTE: }
  }
  // LINE NOTE: if (method == "ocba" || method == "ocba_rollout") {
  if (method == "ocba" || method == "ocba_rollout") {
    // LINE NOTE: return "ocba";
    return "ocba";
  // LINE NOTE: }
  }
  // LINE NOTE: if (method == "ttts" || method == "ttts_rollout") {
  if (method == "ttts" || method == "ttts_rollout") {
    // LINE NOTE: return "ttts";
    return "ttts";
  // LINE NOTE: }
  }

  // LINE NOTE: return "thompson";
  return "thompson";
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: // Public entry: evaluate without trace.
  // Public entry: evaluate without trace.
  // LINE NOTE: return evaluate_with_optional_trace(
  return evaluate_with_optional_trace(
      // LINE NOTE: board,
      board,
      // LINE NOTE: legal_moves,
      legal_moves,
      // LINE NOTE: method,
      method,
      // LINE NOTE: config,
      config,
      // LINE NOTE: rng,
      rng,
      // LINE NOTE: 1,
      1,
      // LINE NOTE: nullptr);
      nullptr);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries) {
  // LINE NOTE: // Pick best by posterior estimate, with deterministic tie-breaks.
  // Pick best by posterior estimate, with deterministic tie-breaks.
  // LINE NOTE: if (summaries.empty()) {
  if (summaries.empty()) {
    // LINE NOTE: throw std::range_error("Cannot determine a best candidate from an empty summary set.");
    throw std::range_error("Cannot determine a best candidate from an empty summary set.");
  // LINE NOTE: }
  }

  // LINE NOTE: int best_index = 0;
  int best_index = 0;
  // LINE NOTE: for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
  for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
    // LINE NOTE: if (summaries[i].estimate > summaries[best_index].estimate + 1e-12) {
    if (summaries[i].estimate > summaries[best_index].estimate + 1e-12) {
      // LINE NOTE: best_index = i;
      best_index = i;
      // LINE NOTE: continue;
      continue;
    // LINE NOTE: }
    }

    // LINE NOTE: if (std::fabs(summaries[i].estimate - summaries[best_index].estimate) <= 1e-12) {
    if (std::fabs(summaries[i].estimate - summaries[best_index].estimate) <= 1e-12) {
      // LINE NOTE: const double current_empirical = Rcpp::NumericVector::is_na(summaries[i].empirical_value)
      const double current_empirical = Rcpp::NumericVector::is_na(summaries[i].empirical_value)
          // LINE NOTE: ? -std::numeric_limits<double>::infinity()
          ? -std::numeric_limits<double>::infinity()
          // LINE NOTE: : summaries[i].empirical_value;
          : summaries[i].empirical_value;
      // LINE NOTE: const double best_empirical = Rcpp::NumericVector::is_na(summaries[best_index].empirical_value)
      const double best_empirical = Rcpp::NumericVector::is_na(summaries[best_index].empirical_value)
          // LINE NOTE: ? -std::numeric_limits<double>::infinity()
          ? -std::numeric_limits<double>::infinity()
          // LINE NOTE: : summaries[best_index].empirical_value;
          : summaries[best_index].empirical_value;

      // LINE NOTE: if (current_empirical > best_empirical + 1e-12) {
      if (current_empirical > best_empirical + 1e-12) {
        // LINE NOTE: best_index = i;
        best_index = i;
        // LINE NOTE: continue;
        continue;
      // LINE NOTE: }
      }

      // LINE NOTE: if (std::fabs(current_empirical - best_empirical) <= 1e-12 &&
      if (std::fabs(current_empirical - best_empirical) <= 1e-12 &&
          // LINE NOTE: summaries[i].allocation_count > summaries[best_index].allocation_count) {
          summaries[i].allocation_count > summaries[best_index].allocation_count) {
        // LINE NOTE: best_index = i;
        best_index = i;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: return best_index;
  return best_index;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: MoveSequence choose_move_sequence_with_allocation(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
MoveSequence choose_move_sequence_with_allocation(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: if (legal_moves.empty()) {
  if (legal_moves.empty()) {
    // LINE NOTE: throw std::range_error("Cannot choose from an empty legal-move set.");
    throw std::range_error("Cannot choose from an empty legal-move set.");
  // LINE NOTE: }
  }

  // LINE NOTE: if (legal_moves.size() == 1U) {
  if (legal_moves.size() == 1U) {
    // LINE NOTE: return legal_moves.front();
    return legal_moves.front();
  // LINE NOTE: }
  }

  // LINE NOTE: const std::vector<ActionEvaluationSummary> summaries =
  const std::vector<ActionEvaluationSummary> summaries =
      // LINE NOTE: evaluate_move_sequences_with_allocation(board, legal_moves, method, config, rng);
      evaluate_move_sequences_with_allocation(board, legal_moves, method, config, rng);

  // LINE NOTE: const int best_summary_index = best_candidate_index(summaries);
  const int best_summary_index = best_candidate_index(summaries);
  // LINE NOTE: const int representative_move_index = summaries[best_summary_index].candidate_index - 1;
  const int representative_move_index = summaries[best_summary_index].candidate_index - 1;
  // LINE NOTE: if (representative_move_index < 0 ||
  if (representative_move_index < 0 ||
      // LINE NOTE: representative_move_index >= static_cast<int>(legal_moves.size())) {
      representative_move_index >= static_cast<int>(legal_moves.size())) {
    // LINE NOTE: throw std::range_error("Internal error: recommended move index is out of range.");
    throw std::range_error("Internal error: recommended move index is out of range.");
  // LINE NOTE: }
  }

  // LINE NOTE: return legal_moves[representative_move_index];
  return legal_moves[representative_move_index];
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
    // LINE NOTE: const std::vector<ActionEvaluationSummary>& summaries) {
    const std::vector<ActionEvaluationSummary>& summaries) {
  // LINE NOTE: // Columnar conversion for R-side data-frame consumption.
  // Columnar conversion for R-side data-frame consumption.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector candidate_index(n);
  // LINE NOTE: Rcpp::IntegerVector n_equivalent_sequences(n);
  Rcpp::IntegerVector n_equivalent_sequences(n);
  // LINE NOTE: Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector allocation_count(n);
  // LINE NOTE: Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector wins(n);
  // LINE NOTE: Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector losses(n);
  // LINE NOTE: Rcpp::IntegerVector unresolved(n);
  Rcpp::IntegerVector unresolved(n);
  // LINE NOTE: Rcpp::NumericVector empirical_value(n);
  Rcpp::NumericVector empirical_value(n);
  // LINE NOTE: Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector alpha(n);
  // LINE NOTE: Rcpp::NumericVector beta(n);
  Rcpp::NumericVector beta(n);
  // LINE NOTE: Rcpp::NumericVector estimate(n);
  Rcpp::NumericVector estimate(n);
  // LINE NOTE: Rcpp::NumericVector posterior_sd(n);
  Rcpp::NumericVector posterior_sd(n);
  // LINE NOTE: Rcpp::NumericVector lower_95(n);
  Rcpp::NumericVector lower_95(n);
  // LINE NOTE: Rcpp::NumericVector upper_95(n);
  Rcpp::NumericVector upper_95(n);
  // LINE NOTE: Rcpp::NumericVector prob_best(n);
  Rcpp::NumericVector prob_best(n);
  // LINE NOTE: Rcpp::NumericVector posterior_expected_regret(n);
  Rcpp::NumericVector posterior_expected_regret(n);
  // LINE NOTE: Rcpp::NumericVector selection_score(n);
  Rcpp::NumericVector selection_score(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: candidate_index[i] = summaries[i].candidate_index;
    candidate_index[i] = summaries[i].candidate_index;
    // LINE NOTE: n_equivalent_sequences[i] = summaries[i].n_equivalent_sequences;
    n_equivalent_sequences[i] = summaries[i].n_equivalent_sequences;
    // LINE NOTE: allocation_count[i] = summaries[i].allocation_count;
    allocation_count[i] = summaries[i].allocation_count;
    // LINE NOTE: wins[i] = summaries[i].wins;
    wins[i] = summaries[i].wins;
    // LINE NOTE: losses[i] = summaries[i].losses;
    losses[i] = summaries[i].losses;
    // LINE NOTE: unresolved[i] = summaries[i].unresolved;
    unresolved[i] = summaries[i].unresolved;
    // LINE NOTE: empirical_value[i] = summaries[i].empirical_value;
    empirical_value[i] = summaries[i].empirical_value;
    // LINE NOTE: alpha[i] = summaries[i].alpha;
    alpha[i] = summaries[i].alpha;
    // LINE NOTE: beta[i] = summaries[i].beta;
    beta[i] = summaries[i].beta;
    // LINE NOTE: estimate[i] = summaries[i].estimate;
    estimate[i] = summaries[i].estimate;
    // LINE NOTE: posterior_sd[i] = summaries[i].posterior_sd;
    posterior_sd[i] = summaries[i].posterior_sd;
    // LINE NOTE: lower_95[i] = summaries[i].lower_95;
    lower_95[i] = summaries[i].lower_95;
    // LINE NOTE: upper_95[i] = summaries[i].upper_95;
    upper_95[i] = summaries[i].upper_95;
    // LINE NOTE: prob_best[i] = summaries[i].prob_best;
    prob_best[i] = summaries[i].prob_best;
    // LINE NOTE: posterior_expected_regret[i] = summaries[i].posterior_expected_regret;
    posterior_expected_regret[i] = summaries[i].posterior_expected_regret;
    // LINE NOTE: selection_score[i] = summaries[i].selection_score;
    selection_score[i] = summaries[i].selection_score;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["candidate_index"] = candidate_index,
      // LINE NOTE: Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
      Rcpp::_["n_equivalent_sequences"] = n_equivalent_sequences,
      // LINE NOTE: Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["allocation_count"] = allocation_count,
      // LINE NOTE: Rcpp::_["wins"] = wins,
      Rcpp::_["wins"] = wins,
      // LINE NOTE: Rcpp::_["losses"] = losses,
      Rcpp::_["losses"] = losses,
      // LINE NOTE: Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["unresolved"] = unresolved,
      // LINE NOTE: Rcpp::_["empirical_value"] = empirical_value,
      Rcpp::_["empirical_value"] = empirical_value,
      // LINE NOTE: Rcpp::_["alpha"] = alpha,
      Rcpp::_["alpha"] = alpha,
      // LINE NOTE: Rcpp::_["beta"] = beta,
      Rcpp::_["beta"] = beta,
      // LINE NOTE: Rcpp::_["estimate"] = estimate,
      Rcpp::_["estimate"] = estimate,
      // LINE NOTE: Rcpp::_["posterior_sd"] = posterior_sd,
      Rcpp::_["posterior_sd"] = posterior_sd,
      // LINE NOTE: Rcpp::_["lower_95"] = lower_95,
      Rcpp::_["lower_95"] = lower_95,
      // LINE NOTE: Rcpp::_["upper_95"] = upper_95,
      Rcpp::_["upper_95"] = upper_95,
      // LINE NOTE: Rcpp::_["prob_best"] = prob_best,
      Rcpp::_["prob_best"] = prob_best,
      // LINE NOTE: Rcpp::_["posterior_expected_regret"] = posterior_expected_regret,
      Rcpp::_["posterior_expected_regret"] = posterior_expected_regret,
      // LINE NOTE: Rcpp::_["selection_score"] = selection_score,
      Rcpp::_["selection_score"] = selection_score,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: }  // namespace backgammonr
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
}  // namespace backgammonr

// LINE NOTE: Rcpp::DataFrame allocation_trace_rows_to_data_frame(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::DataFrame allocation_trace_rows_to_data_frame(
    // LINE NOTE: const std::vector<AllocationTraceRow>& trace_rows) {
    const std::vector<AllocationTraceRow>& trace_rows) {
  // LINE NOTE: // Columnar conversion for optional trace output.
  // Columnar conversion for optional trace output.
  // LINE NOTE: const int n = static_cast<int>(trace_rows.size());
  const int n = static_cast<int>(trace_rows.size());
  // LINE NOTE: Rcpp::IntegerVector checkpoint(n);
  Rcpp::IntegerVector checkpoint(n);
  // LINE NOTE: Rcpp::IntegerVector selected_candidate(n);
  Rcpp::IntegerVector selected_candidate(n);
  // LINE NOTE: Rcpp::IntegerVector leader_index(n);
  Rcpp::IntegerVector leader_index(n);
  // LINE NOTE: Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector candidate_index(n);
  // LINE NOTE: Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector allocation_count(n);
  // LINE NOTE: Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector wins(n);
  // LINE NOTE: Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector losses(n);
  // LINE NOTE: Rcpp::IntegerVector unresolved(n);
  Rcpp::IntegerVector unresolved(n);
  // LINE NOTE: Rcpp::NumericVector empirical_value(n);
  Rcpp::NumericVector empirical_value(n);
  // LINE NOTE: Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector alpha(n);
  // LINE NOTE: Rcpp::NumericVector beta(n);
  Rcpp::NumericVector beta(n);
  // LINE NOTE: Rcpp::NumericVector estimate(n);
  Rcpp::NumericVector estimate(n);
  // LINE NOTE: Rcpp::NumericVector posterior_sd(n);
  Rcpp::NumericVector posterior_sd(n);
  // LINE NOTE: Rcpp::NumericVector lower_95(n);
  Rcpp::NumericVector lower_95(n);
  // LINE NOTE: Rcpp::NumericVector upper_95(n);
  Rcpp::NumericVector upper_95(n);
  // LINE NOTE: Rcpp::NumericVector selection_score(n);
  Rcpp::NumericVector selection_score(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: checkpoint[i] = trace_rows[i].checkpoint;
    checkpoint[i] = trace_rows[i].checkpoint;
    // LINE NOTE: selected_candidate[i] = trace_rows[i].selected_candidate;
    selected_candidate[i] = trace_rows[i].selected_candidate;
    // LINE NOTE: leader_index[i] = trace_rows[i].leader_index;
    leader_index[i] = trace_rows[i].leader_index;
    // LINE NOTE: candidate_index[i] = trace_rows[i].candidate_index;
    candidate_index[i] = trace_rows[i].candidate_index;
    // LINE NOTE: allocation_count[i] = trace_rows[i].allocation_count;
    allocation_count[i] = trace_rows[i].allocation_count;
    // LINE NOTE: wins[i] = trace_rows[i].wins;
    wins[i] = trace_rows[i].wins;
    // LINE NOTE: losses[i] = trace_rows[i].losses;
    losses[i] = trace_rows[i].losses;
    // LINE NOTE: unresolved[i] = trace_rows[i].unresolved;
    unresolved[i] = trace_rows[i].unresolved;
    // LINE NOTE: empirical_value[i] = trace_rows[i].empirical_value;
    empirical_value[i] = trace_rows[i].empirical_value;
    // LINE NOTE: alpha[i] = trace_rows[i].alpha;
    alpha[i] = trace_rows[i].alpha;
    // LINE NOTE: beta[i] = trace_rows[i].beta;
    beta[i] = trace_rows[i].beta;
    // LINE NOTE: estimate[i] = trace_rows[i].estimate;
    estimate[i] = trace_rows[i].estimate;
    // LINE NOTE: posterior_sd[i] = trace_rows[i].posterior_sd;
    posterior_sd[i] = trace_rows[i].posterior_sd;
    // LINE NOTE: lower_95[i] = trace_rows[i].lower_95;
    lower_95[i] = trace_rows[i].lower_95;
    // LINE NOTE: upper_95[i] = trace_rows[i].upper_95;
    upper_95[i] = trace_rows[i].upper_95;
    // LINE NOTE: selection_score[i] = trace_rows[i].selection_score;
    selection_score[i] = trace_rows[i].selection_score;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["checkpoint"] = checkpoint,
      Rcpp::_["checkpoint"] = checkpoint,
      // LINE NOTE: Rcpp::_["selected_candidate"] = selected_candidate,
      Rcpp::_["selected_candidate"] = selected_candidate,
      // LINE NOTE: Rcpp::_["leader_index"] = leader_index,
      Rcpp::_["leader_index"] = leader_index,
      // LINE NOTE: Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["candidate_index"] = candidate_index,
      // LINE NOTE: Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["allocation_count"] = allocation_count,
      // LINE NOTE: Rcpp::_["wins"] = wins,
      Rcpp::_["wins"] = wins,
      // LINE NOTE: Rcpp::_["losses"] = losses,
      Rcpp::_["losses"] = losses,
      // LINE NOTE: Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["unresolved"] = unresolved,
      // LINE NOTE: Rcpp::_["empirical_value"] = empirical_value,
      Rcpp::_["empirical_value"] = empirical_value,
      // LINE NOTE: Rcpp::_["alpha"] = alpha,
      Rcpp::_["alpha"] = alpha,
      // LINE NOTE: Rcpp::_["beta"] = beta,
      Rcpp::_["beta"] = beta,
      // LINE NOTE: Rcpp::_["estimate"] = estimate,
      Rcpp::_["estimate"] = estimate,
      // LINE NOTE: Rcpp::_["posterior_sd"] = posterior_sd,
      Rcpp::_["posterior_sd"] = posterior_sd,
      // LINE NOTE: Rcpp::_["lower_95"] = lower_95,
      Rcpp::_["lower_95"] = lower_95,
      // LINE NOTE: Rcpp::_["upper_95"] = upper_95,
      Rcpp::_["upper_95"] = upper_95,
      // LINE NOTE: Rcpp::_["selection_score"] = selection_score,
      Rcpp::_["selection_score"] = selection_score,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // [[Rcpp::export]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_allocation_evaluate(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_allocation_evaluate(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& legal_moves,
    const Rcpp::List& legal_moves,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const int total_budget,
    const int total_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const double unresolved_value,
    const double unresolved_value,
    // LINE NOTE: const int initial_allocations,
    const int initial_allocations,
    // LINE NOTE: const double ucb_exploration,
    const double ucb_exploration,
    // LINE NOTE: const double prior_alpha,
    const double prior_alpha,
    // LINE NOTE: const double prior_beta,
    const double prior_beta,
    // LINE NOTE: const std::string& dice_mode,
    const std::string& dice_mode,
    // LINE NOTE: const bool crn,
    const bool crn,
    // LINE NOTE: const bool fast_diagnostics,
    const bool fast_diagnostics,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Parse R inputs.
  // Parse R inputs.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{
  const backgammonr::RolloutConfig config{
      // LINE NOTE: total_budget,
      total_budget,
      // LINE NOTE: rollout_policy,
      rollout_policy,
      // LINE NOTE: max_rollout_turns,
      max_rollout_turns,
      // LINE NOTE: ucb_exploration,
      ucb_exploration,
      // LINE NOTE: prior_alpha,
      prior_alpha,
      // LINE NOTE: prior_beta,
      prior_beta,
      // LINE NOTE: initial_allocations,
      initial_allocations,
      // LINE NOTE: unresolved_value,
      unresolved_value,
      // LINE NOTE: dice_mode,
      dice_mode,
      // LINE NOTE: crn,
      crn,
      // LINE NOTE: seed,
      seed,
      // LINE NOTE: use_seed,
      use_seed,
      // LINE NOTE: fast_diagnostics};
      fast_diagnostics};
  // LINE NOTE: // Local RNG stream for this evaluation call.
  // Local RNG stream for this evaluation call.
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: const std::vector<backgammonr::ActionEvaluationSummary> summaries =
  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      // LINE NOTE: backgammonr::evaluate_move_sequences_with_allocation(
      backgammonr::evaluate_move_sequences_with_allocation(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: method,
          method,
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng);
          rng);
  // LINE NOTE: const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  // LINE NOTE: const int best_index = summaries[best_summary_index].candidate_index;
  const int best_index = summaries[best_summary_index].candidate_index;

  // LINE NOTE: return Rcpp::List::create(
  return Rcpp::List::create(
      // LINE NOTE: Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      // LINE NOTE: Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      // LINE NOTE: Rcpp::_["method"] = Rcpp::CharacterVector::create(
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          // LINE NOTE: backgammonr::canonicalize_allocation_method(method)),
          backgammonr::canonicalize_allocation_method(method)),
      // LINE NOTE: Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // [[Rcpp::export]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_allocation_evaluate_trace(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_allocation_evaluate_trace(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& legal_moves,
    const Rcpp::List& legal_moves,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const int total_budget,
    const int total_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const double unresolved_value,
    const double unresolved_value,
    // LINE NOTE: const int initial_allocations,
    const int initial_allocations,
    // LINE NOTE: const double ucb_exploration,
    const double ucb_exploration,
    // LINE NOTE: const double prior_alpha,
    const double prior_alpha,
    // LINE NOTE: const double prior_beta,
    const double prior_beta,
    // LINE NOTE: const std::string& dice_mode,
    const std::string& dice_mode,
    // LINE NOTE: const bool crn,
    const bool crn,
    // LINE NOTE: const bool fast_diagnostics,
    const bool fast_diagnostics,
    // LINE NOTE: const int trace_every,
    const int trace_every,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Same as evaluate, but also capture checkpoint trace rows.
  // Same as evaluate, but also capture checkpoint trace rows.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{
  const backgammonr::RolloutConfig config{
      // LINE NOTE: total_budget,
      total_budget,
      // LINE NOTE: rollout_policy,
      rollout_policy,
      // LINE NOTE: max_rollout_turns,
      max_rollout_turns,
      // LINE NOTE: ucb_exploration,
      ucb_exploration,
      // LINE NOTE: prior_alpha,
      prior_alpha,
      // LINE NOTE: prior_beta,
      prior_beta,
      // LINE NOTE: initial_allocations,
      initial_allocations,
      // LINE NOTE: unresolved_value,
      unresolved_value,
      // LINE NOTE: dice_mode,
      dice_mode,
      // LINE NOTE: crn,
      crn,
      // LINE NOTE: seed,
      seed,
      // LINE NOTE: use_seed,
      use_seed,
      // LINE NOTE: fast_diagnostics};
      fast_diagnostics};
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);
  // LINE NOTE: std::vector<AllocationTraceRow> trace_rows;
  std::vector<AllocationTraceRow> trace_rows;
  // LINE NOTE: // Reserve worst-case order-of-magnitude to reduce trace vector growth.
  // Reserve worst-case order-of-magnitude to reduce trace vector growth.
  // LINE NOTE: trace_rows.reserve(static_cast<std::size_t>(std::max(total_budget, 0)) *
  trace_rows.reserve(static_cast<std::size_t>(std::max(total_budget, 0)) *
      // LINE NOTE: static_cast<std::size_t>(std::max(static_cast<int>(parsed_moves.size()), 1)));
      static_cast<std::size_t>(std::max(static_cast<int>(parsed_moves.size()), 1)));

  // LINE NOTE: const std::vector<backgammonr::ActionEvaluationSummary> summaries =
  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      // LINE NOTE: evaluate_with_optional_trace(
      evaluate_with_optional_trace(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: method,
          method,
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng,
          rng,
          // LINE NOTE: trace_every,
          trace_every,
          // LINE NOTE: &trace_rows);
          &trace_rows);
  // LINE NOTE: const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  // LINE NOTE: const int best_index = summaries[best_summary_index].candidate_index;
  const int best_index = summaries[best_summary_index].candidate_index;

  // LINE NOTE: return Rcpp::List::create(
  return Rcpp::List::create(
      // LINE NOTE: Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      // LINE NOTE: Rcpp::_["trace"] = allocation_trace_rows_to_data_frame(trace_rows),
      Rcpp::_["trace"] = allocation_trace_rows_to_data_frame(trace_rows),
      // LINE NOTE: Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      // LINE NOTE: Rcpp::_["method"] = Rcpp::CharacterVector::create(
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          // LINE NOTE: backgammonr::canonicalize_allocation_method(method)),
          backgammonr::canonicalize_allocation_method(method)),
      // LINE NOTE: Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // [[Rcpp::export]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_profile_rollout_runtime(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_profile_rollout_runtime(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& roll,
    const Rcpp::List& roll,
    // LINE NOTE: const int legal_reps,
    const int legal_reps,
    // LINE NOTE: const int apply_reps,
    const int apply_reps,
    // LINE NOTE: const int one_rollout_reps,
    const int one_rollout_reps,
    // LINE NOTE: const int total_budget,
    const int total_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Validate repetition counts and budget for runtime profiling.
  // Validate repetition counts and budget for runtime profiling.
  // LINE NOTE: if (legal_reps < 1) {
  if (legal_reps < 1) {
    // LINE NOTE: throw std::range_error("`legal_reps` must be at least 1.");
    throw std::range_error("`legal_reps` must be at least 1.");
  // LINE NOTE: }
  }
  // LINE NOTE: if (apply_reps < 1) {
  if (apply_reps < 1) {
    // LINE NOTE: throw std::range_error("`apply_reps` must be at least 1.");
    throw std::range_error("`apply_reps` must be at least 1.");
  // LINE NOTE: }
  }
  // LINE NOTE: if (one_rollout_reps < 1) {
  if (one_rollout_reps < 1) {
    // LINE NOTE: throw std::range_error("`one_rollout_reps` must be at least 1.");
    throw std::range_error("`one_rollout_reps` must be at least 1.");
  // LINE NOTE: }
  }
  // LINE NOTE: if (total_budget < 1) {
  if (total_budget < 1) {
    // LINE NOTE: throw std::range_error("`total_budget` must be at least 1.");
    throw std::range_error("`total_budget` must be at least 1.");
  // LINE NOTE: }
  }

  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Time legal move generation.
  // Time legal move generation.
  // LINE NOTE: auto tic = std::chrono::steady_clock::now();
  auto tic = std::chrono::steady_clock::now();
  // LINE NOTE: std::vector<backgammonr::MoveSequence> legal_moves;
  std::vector<backgammonr::MoveSequence> legal_moves;
  // LINE NOTE: for (int i = 0; i < legal_reps; ++i) {
  for (int i = 0; i < legal_reps; ++i) {
    // LINE NOTE: legal_moves = backgammonr::generate_legal_move_sequences(
    legal_moves = backgammonr::generate_legal_move_sequences(
        // LINE NOTE: parsed_board, parsed_board.turn, parsed_roll);
        parsed_board, parsed_board.turn, parsed_roll);
  // LINE NOTE: }
  }
  // LINE NOTE: auto toc = std::chrono::steady_clock::now();
  auto toc = std::chrono::steady_clock::now();
  // LINE NOTE: const double legal_seconds = std::chrono::duration<double>(toc - tic).count();
  const double legal_seconds = std::chrono::duration<double>(toc - tic).count();

  // LINE NOTE: if (legal_moves.empty()) {
  if (legal_moves.empty()) {
    // LINE NOTE: // No legal moves: remaining timings are not defined.
    // No legal moves: remaining timings are not defined.
    // LINE NOTE: return Rcpp::List::create(
    return Rcpp::List::create(
        // LINE NOTE: Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(0),
        Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(0),
        // LINE NOTE: Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
        Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
        // LINE NOTE: Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        // LINE NOTE: Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        // LINE NOTE: Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL));
        Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL));
  // LINE NOTE: }
  }

  // LINE NOTE: const backgammonr::MoveSequence first_move = legal_moves.front();
  const backgammonr::MoveSequence first_move = legal_moves.front();

  // LINE NOTE: // Time move application hot path.
  // Time move application hot path.
  // LINE NOTE: tic = std::chrono::steady_clock::now();
  tic = std::chrono::steady_clock::now();
  // LINE NOTE: for (int i = 0; i < apply_reps; ++i) {
  for (int i = 0; i < apply_reps; ++i) {
    // LINE NOTE: (void) apply_sequence_without_full_validation(parsed_board, first_move);
    (void) apply_sequence_without_full_validation(parsed_board, first_move);
  // LINE NOTE: }
  }
  // LINE NOTE: toc = std::chrono::steady_clock::now();
  toc = std::chrono::steady_clock::now();
  // LINE NOTE: const double apply_seconds = std::chrono::duration<double>(toc - tic).count();
  const double apply_seconds = std::chrono::duration<double>(toc - tic).count();

  // LINE NOTE: const std::vector<backgammonr::MoveSequence> singleton_moves{first_move};
  const std::vector<backgammonr::MoveSequence> singleton_moves{first_move};
  // LINE NOTE: const backgammonr::RolloutConfig single_rollout_config{
  const backgammonr::RolloutConfig single_rollout_config{
      // LINE NOTE: 1,
      1,
      // LINE NOTE: rollout_policy,
      rollout_policy,
      // LINE NOTE: max_rollout_turns};
      max_rollout_turns};

  // LINE NOTE: // Time one-candidate one-rollout evaluations repeatedly.
  // Time one-candidate one-rollout evaluations repeatedly.
  // LINE NOTE: tic = std::chrono::steady_clock::now();
  tic = std::chrono::steady_clock::now();
  // LINE NOTE: for (int i = 0; i < one_rollout_reps; ++i) {
  for (int i = 0; i < one_rollout_reps; ++i) {
    // LINE NOTE: (void) backgammonr::evaluate_move_sequences_with_allocation(
    (void) backgammonr::evaluate_move_sequences_with_allocation(
        // LINE NOTE: parsed_board,
        parsed_board,
        // LINE NOTE: singleton_moves,
        singleton_moves,
        // LINE NOTE: "equal",
        "equal",
        // LINE NOTE: single_rollout_config,
        single_rollout_config,
        // LINE NOTE: rng);
        rng);
  // LINE NOTE: }
  }
  // LINE NOTE: toc = std::chrono::steady_clock::now();
  toc = std::chrono::steady_clock::now();
  // LINE NOTE: const double one_rollout_seconds = std::chrono::duration<double>(toc - tic).count();
  const double one_rollout_seconds = std::chrono::duration<double>(toc - tic).count();

  // LINE NOTE: const backgammonr::RolloutConfig batch_config{
  const backgammonr::RolloutConfig batch_config{
      // LINE NOTE: total_budget,
      total_budget,
      // LINE NOTE: rollout_policy,
      rollout_policy,
      // LINE NOTE: max_rollout_turns};
      max_rollout_turns};
  // LINE NOTE: // Time full batched evaluation for the provided budget.
  // Time full batched evaluation for the provided budget.
  // LINE NOTE: tic = std::chrono::steady_clock::now();
  tic = std::chrono::steady_clock::now();
  // LINE NOTE: (void) backgammonr::evaluate_move_sequences_with_allocation(
  (void) backgammonr::evaluate_move_sequences_with_allocation(
      // LINE NOTE: parsed_board,
      parsed_board,
      // LINE NOTE: legal_moves,
      legal_moves,
      // LINE NOTE: "equal",
      "equal",
      // LINE NOTE: batch_config,
      batch_config,
      // LINE NOTE: rng);
      rng);
  // LINE NOTE: toc = std::chrono::steady_clock::now();
  toc = std::chrono::steady_clock::now();
  // LINE NOTE: const double batched_seconds = std::chrono::duration<double>(toc - tic).count();
  const double batched_seconds = std::chrono::duration<double>(toc - tic).count();

  // LINE NOTE: return Rcpp::List::create(
  return Rcpp::List::create(
      // LINE NOTE: Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(static_cast<int>(legal_moves.size())),
      Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(static_cast<int>(legal_moves.size())),
      // LINE NOTE: Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
      Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
      // LINE NOTE: Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(apply_seconds),
      Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(apply_seconds),
      // LINE NOTE: Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(one_rollout_seconds),
      Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(one_rollout_seconds),
      // LINE NOTE: Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(batched_seconds),
      Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(batched_seconds),
      // LINE NOTE: Rcpp::_["legal_reps"] = Rcpp::IntegerVector::create(legal_reps),
      Rcpp::_["legal_reps"] = Rcpp::IntegerVector::create(legal_reps),
      // LINE NOTE: Rcpp::_["apply_reps"] = Rcpp::IntegerVector::create(apply_reps),
      Rcpp::_["apply_reps"] = Rcpp::IntegerVector::create(apply_reps),
      // LINE NOTE: Rcpp::_["one_rollout_reps"] = Rcpp::IntegerVector::create(one_rollout_reps),
      Rcpp::_["one_rollout_reps"] = Rcpp::IntegerVector::create(one_rollout_reps),
      // LINE NOTE: Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget),
      // LINE NOTE: Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(rollout_policy),
      Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(rollout_policy),
      // LINE NOTE: Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(max_rollout_turns));
      Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(max_rollout_turns));
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}
