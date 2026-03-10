#include "bg_rollout.h"

#include <cstdint>
#include <random>
#include <stdexcept>
#include <vector>

#include "bg_allocation.h"

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

namespace {

// Build an RNG stream for this call.
// - If user supplied a seed, run deterministically.
// - Otherwise seed from std::random_device for non-deterministic behavior.
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

}  // namespace

namespace backgammonr {

// Returns whether this selection method requires randomness.
// This is used elsewhere to decide whether we need to allocate/seed an RNG.
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

// Validate a user-facing selection label.
// Keep accepted values synchronized with R-side argument matching.
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

// Validate a rollout continuation policy label.
bool is_supported_rollout_policy(const std::string& policy) {
  return policy == "random" || policy == "aggressive" || policy == "defensive";
}

// Validate dice stratification mode used by allocation/rollout experiments.
bool is_supported_dice_mode(const std::string& dice_mode) {
  return dice_mode == "iid" ||
      dice_mode == "stratified_first_roll" ||
      dice_mode == "stratified_first_two_rolls";
}

// Validate complete rollout configuration prior to simulation.
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

// Evaluate each legal move with equal-allocation rollouts and return compact
// win/loss/unresolved summaries.
std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):**
  // Step 1: call the shared allocation engine with method fixed to `"equal"`.
  // Step 2: map generic action summaries into rollout-specific summary rows.
  // Step 3: keep only fields needed for user interpretation in this context.
  // common evaluator in equal-allocation mode, then present cleaner output."
  // Reuse the shared allocation engine using canonical method = "equal".
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

// Choose one move under equal-allocation rollout policy.
MoveSequence choose_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Delegates move choice to the same shared
  // engine as other methods, but hard-codes equal allocation so behavior is
  // predictable and directly comparable in benchmarks.
  // using non-adaptive equal-budget rollout logic.
  // Again, selection delegates to shared allocation engine with "equal" method.
  return choose_move_sequence_with_allocation(board, legal_moves, "equal", config, rng);
}

// Convert rollout summary vector to an R data.frame.
Rcpp::DataFrame rollout_move_summaries_to_data_frame(
    const std::vector<RolloutMoveSummary>& summaries) {
  // **WHAT IT'S DOING (DETAILED):** Converts vector-of-struct storage (C++) to
  // columnar vectors (R data frame). This is required because R tables are
  // column-oriented and downstream plotting/printing expects that layout.
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

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):**
  // - Parse R inputs into engine-native board/move objects.
  // - Build rollout configuration from scalar parameters.
  // - Create deterministic RNG if seed is requested.
  // - Run equal-allocation evaluation and return standardized action table.
  // equal rollout allocation."
  // Parse and validate R-side inputs into C++ engine types.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build per-call RNG.
  std::mt19937 rng = init_rng(seed, use_seed);

  // Return full allocation-style table so downstream R summaries stay consistent.
  // We intentionally return the standardized allocation table (not a tiny custom
  // table) so all methods share the same downstream print/summary code paths.
  return backgammonr::action_evaluation_summaries_to_data_frame(
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):** Same parse/config/RNG steps as the evaluate
  // wrapper, but returns exactly one selected move sequence instead of a full
  // per-candidate table.
  // Parse and validate R-side inputs into C++ engine types.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build per-call RNG.
  std::mt19937 rng = init_rng(seed, use_seed);

  // Choose one move under equal-allocation rollout logic and convert to R list.
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}
