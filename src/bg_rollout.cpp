// Rollout-based move evaluation and move-choice kernels.
#include "bg_rollout.h"

#include <stdexcept>
#include <vector>

#include "bg_allocation.h"
#include "bg_rng.h"

// -----------------------------------------------------------------------------
// bg_rollout.cpp
//
// This file provides the "plain rollout" C++ API used by R wrappers.
//
// Important design note:
// - We intentionally delegate core simulation-allocation logic to
//   evaluate_move_sequences_with_allocation(...).
// - For this rollout module, we force allocation method = "equal".
// - This keeps one allocation engine (in bg_allocation.cpp) as the single
//   source of truth, while offering a stable rollout-facing interface.
// -----------------------------------------------------------------------------

namespace backgammonr {

// Function: selection_uses_randomness
// Purpose: Report whether a selector needs RNG access at runtime.
// Called by: bg_simulation.cpp and bg_benchmark.cpp helpers that avoid creating
// RNGs for deterministic selectors.
// Notes: Validation is done first so bad labels fail fast everywhere.
bool selection_uses_randomness(const std::string& selection) {
  // Validate first so callers get consistent errors for unsupported labels.
  validate_selection(selection);
  return selection == "random" ||
      selection == "rollout" ||
      selection == "equal_rollout" ||
      selection == "greedy_rollout" ||
      selection == "ucb_rollout" ||
      selection == "thompson_rollout" ||
      selection == "ocba_rollout" ||
      selection == "ttts_rollout";
}

// Function: validate_selection
// Purpose: Enforce the supported selector vocabulary for rollout/game APIs.
// Called by: selection_uses_randomness(), simulation/benchmark entry points,
// and R-facing wrappers through shared engine code.
// Notes: This is the central guardrail for selector spelling.
void validate_selection(const std::string& selection) {
  if (selection != "first" &&
      selection != "random" &&
      selection != "aggressive" &&
      selection != "defensive" &&
      selection != "rollout" &&
      selection != "equal_rollout" &&
      selection != "greedy_rollout" &&
      selection != "ucb_rollout" &&
      selection != "thompson_rollout" &&
      selection != "ocba_rollout" &&
      selection != "ttts_rollout") {
    throw std::range_error(
        "`selection` must be one of \"first\", \"random\", \"aggressive\", \"defensive\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"thompson_rollout\", \"ttts_rollout\", or \"ocba_rollout\".");
  }
}

// Function: is_supported_rollout_policy
// Purpose: Return whether a rollout continuation policy label is supported.
// Called by: validate_rollout_config().
bool is_supported_rollout_policy(const std::string& policy) {
  return policy == "random" || policy == "aggressive" || policy == "defensive";
}

// Function: is_supported_dice_mode
// Purpose: Return whether a dice variance-control mode is supported.
// Called by: validate_rollout_config().
bool is_supported_dice_mode(const std::string& dice_mode) {
  return dice_mode == "iid" ||
      dice_mode == "stratified_first_roll" ||
      dice_mode == "stratified_first_two_rolls";
}

// Function: validate_rollout_config
// Purpose: Validate all rollout configuration fields before evaluation.
// Called by: allocation entry points and benchmark/simulation wrappers that
// accept rollout controls.
// Notes: This keeps numeric constraints and label constraints centralized.
void validate_rollout_config(const RolloutConfig& config) {
  // Budget must allocate at least one rollout.
  if (config.budget < 1) {
    throw std::range_error("`rollout_budget` must be at least 1.");
  }

  // Continuation policy must be one of the supported heuristic policies.
  if (!is_supported_rollout_policy(config.policy)) {
    throw std::range_error(
        "`rollout_policy` must be one of \"random\", \"aggressive\", or \"defensive\".");
  }

  // Turn cap may be zero (immediate unresolved) but not negative.
  if (config.max_turns < 0) {
    throw std::range_error("`max_rollout_turns` must be nonnegative.");
  }

  // UCB exploration coefficient (reused as TTTS beta in TTTS path) must be >= 0.
  if (config.ucb_exploration < 0.0) {
    throw std::range_error("`ucb_exploration` must be nonnegative.");
  }

  // Beta prior parameters must be positive for conjugate posterior updates.
  if (config.prior_alpha <= 0.0 || config.prior_beta <= 0.0) {
    throw std::range_error("`prior_alpha` and `prior_beta` must be positive.");
  }

  // Initial warm-start allocations cannot be negative.
  if (config.initial_allocations < 0) {
    throw std::range_error("`initial_allocations` must be nonnegative.");
  }

  // Unresolved reward must be a valid probability-scale value.
  if (config.unresolved_value < 0.0 || config.unresolved_value > 1.0) {
    throw std::range_error("`unresolved_value` must lie between 0 and 1.");
  }

  // Dice mode controls variance-reduction schedule for early rolls.
  if (!is_supported_dice_mode(config.dice_mode)) {
    throw std::range_error(
        "`dice_mode` must be one of \"iid\", \"stratified_first_roll\", or \"stratified_first_two_rolls\".");
  }
}

// Function: evaluate_rollout_move_sequences
// Purpose: Evaluate legal moves using equal allocation and return compact rows.
// Called by: R-side wrappers for equal-rollout evaluation and tests.
// Also used conceptually as the non-adaptive baseline against Thompson/UCB/OCBA.
// Notes: Delegates core sampling to `evaluate_move_sequences_with_allocation()`
// so all methods share one allocation engine.
std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Step 1: call the shared allocation engine with method fixed to "equal".
  // Step 2: map generic action summaries into rollout-specific summary rows.
  // Step 3: return only fields needed by equal-rollout consumers.
  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, "equal", config, rng);

  std::vector<RolloutMoveSummary> out;
  out.reserve(summaries.size());

  for (const ActionEvaluationSummary& summary : summaries) {
    // One output row per candidate action; this preserves one-to-one mapping
    // from the internal evaluator to the user-facing table.
    RolloutMoveSummary row;
    // Candidate index is 1-based in user-facing outputs.
    row.candidate_index = summary.candidate_index;
    // Raw outcome counts are carried over directly.
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    // Prefer empirical value when available; otherwise fall back to posterior estimate.
    row.win_rate = Rcpp::NumericVector::is_na(summary.empirical_value)
        ? summary.estimate
        : summary.empirical_value;
    out.push_back(row);
  }

  return out;
}

// Function: choose_rollout_move_sequence
// Purpose: Select one legal move under equal-allocation rollout logic.
// Called by: R wrappers when user requests one chosen move (not full table).
// Notes: Uses shared chooser to keep tie-breaking behavior consistent with
// other allocation methods.
MoveSequence choose_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Hard-code method = "equal" so this function remains the baseline chooser.
  return choose_move_sequence_with_allocation(board, legal_moves, "equal", config, rng);
}

// Function: rollout_move_summaries_to_data_frame
// Purpose: Convert C++ rollout summary structs into a columnar R data frame.
// Called by: internal wrappers that need a compact tabular result.
// Notes: Isolated here so R column naming/layout stays stable.
Rcpp::DataFrame rollout_move_summaries_to_data_frame(
    const std::vector<RolloutMoveSummary>& summaries) {
  const int n = static_cast<int>(summaries.size());
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector win_rate(n);

  for (int i = 0; i < n; ++i) {
    candidate_index[i] = summaries[i].candidate_index;
    wins[i] = summaries[i].wins;
    losses[i] = summaries[i].losses;
    unresolved[i] = summaries[i].unresolved;
    win_rate[i] = summaries[i].win_rate;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["win_rate"] = win_rate,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace backgammonr

// Function: bg_cpp_rollout_move_evaluate
// Purpose: Rcpp entry point for equal-rollout move evaluation.
// Called by: R function `evaluate_actions_equal_rollout()` via RcppExports.
// Notes: Returns standardized allocation-style table for compatibility with the
// package's shared print/summary workflow.
// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R-side lists/scalars into engine-native objects.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build per-call RNG.
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  // Return standardized action table so all methods share downstream tooling.
  return backgammonr::action_evaluation_summaries_to_data_frame(
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}

// Function: bg_cpp_rollout_move_choice
// Purpose: Rcpp entry point for selecting one equal-rollout move.
// Called by: R function `choose_action_equal_rollout()` via RcppExports.
// Notes: Mirrors evaluate wrapper parsing/configuration to keep behavior aligned.
// [[Rcpp::export]]
Rcpp::List bg_cpp_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R-side lists/scalars into engine-native objects.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build per-call RNG.
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  // Choose one move under equal-allocation rollout logic and convert to R list.
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}
