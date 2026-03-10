// LINE NOTE: #include "bg_rollout.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_rollout.h"

// LINE NOTE: #include <cstdint>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <cstdint>
// LINE NOTE: #include <random>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <random>
// LINE NOTE: #include <stdexcept>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <stdexcept>
// LINE NOTE: #include <vector>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <vector>

// LINE NOTE: #include "bg_allocation.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_allocation.h"

// LINE NOTE: // -----------------------------------------------------------------------------
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// -----------------------------------------------------------------------------
// LINE NOTE: // bg_rollout.cpp
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// bg_rollout.cpp
// LINE NOTE: //
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//
// LINE NOTE: // This file provides the "plain rollout" C++ API used by R wrappers.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// This file provides the "plain rollout" C++ API used by R wrappers.
// LINE NOTE: //
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//
// LINE NOTE: // Important design note:
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Important design note:
// LINE NOTE: // - We intentionally delegate core simulation-allocation logic to
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - We intentionally delegate core simulation-allocation logic to
// LINE NOTE: //   evaluate_move_sequences_with_allocation(...).
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//   evaluate_move_sequences_with_allocation(...).
// LINE NOTE: // - For this rollout module, we force allocation method = "equal".
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - For this rollout module, we force allocation method = "equal".
// LINE NOTE: // - This keeps one allocation engine (in bg_allocation.cpp) as the single
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - This keeps one allocation engine (in bg_allocation.cpp) as the single
// LINE NOTE: //   source of truth, while offering a stable rollout-facing interface.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
//   source of truth, while offering a stable rollout-facing interface.
// LINE NOTE: // -----------------------------------------------------------------------------
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// -----------------------------------------------------------------------------

// LINE NOTE: namespace {
// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace {

// LINE NOTE: // Build an RNG stream for this call.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Build an RNG stream for this call.
// LINE NOTE: // - If user supplied a seed, run deterministically.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - If user supplied a seed, run deterministically.
// LINE NOTE: // - Otherwise seed from std::random_device for non-deterministic behavior.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// - Otherwise seed from std::random_device for non-deterministic behavior.
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

// LINE NOTE: }  // namespace
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
}  // namespace

// LINE NOTE: namespace backgammonr {
// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace backgammonr {

// LINE NOTE: // Returns whether this selection method requires randomness.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Returns whether this selection method requires randomness.
// LINE NOTE: // This is used elsewhere to decide whether we need to allocate/seed an RNG.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// This is used elsewhere to decide whether we need to allocate/seed an RNG.
// LINE NOTE: bool selection_uses_randomness(const std::string& selection) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool selection_uses_randomness(const std::string& selection) {
  // LINE NOTE: // Validate first so callers get consistent errors for unsupported labels.
  // Validate first so callers get consistent errors for unsupported labels.
  // LINE NOTE: validate_selection(selection);
  validate_selection(selection);
  // LINE NOTE: return selection == "random" ||
  return selection == "random" ||
      // LINE NOTE: selection == "rollout" ||
      selection == "rollout" ||
      // LINE NOTE: selection == "equal_rollout" ||
      selection == "equal_rollout" ||
      // LINE NOTE: selection == "greedy_rollout" ||
      selection == "greedy_rollout" ||
      // LINE NOTE: selection == "ucb_rollout" ||
      selection == "ucb_rollout" ||
      // LINE NOTE: selection == "thompson_rollout" ||
      selection == "thompson_rollout" ||
      // LINE NOTE: selection == "ocba_rollout" ||
      selection == "ocba_rollout" ||
      // LINE NOTE: selection == "ttts_rollout";
      selection == "ttts_rollout";
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Validate a user-facing selection label.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate a user-facing selection label.
// LINE NOTE: // Keep accepted values synchronized with R-side argument matching.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Keep accepted values synchronized with R-side argument matching.
// LINE NOTE: void validate_selection(const std::string& selection) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void validate_selection(const std::string& selection) {
  // LINE NOTE: if (selection != "first" &&
  if (selection != "first" &&
      // LINE NOTE: selection != "random" &&
      selection != "random" &&
      // LINE NOTE: selection != "aggressive" &&
      selection != "aggressive" &&
      // LINE NOTE: selection != "defensive" &&
      selection != "defensive" &&
      // LINE NOTE: selection != "rollout" &&
      selection != "rollout" &&
      // LINE NOTE: selection != "equal_rollout" &&
      selection != "equal_rollout" &&
      // LINE NOTE: selection != "greedy_rollout" &&
      selection != "greedy_rollout" &&
      // LINE NOTE: selection != "ucb_rollout" &&
      selection != "ucb_rollout" &&
      // LINE NOTE: selection != "thompson_rollout" &&
      selection != "thompson_rollout" &&
      // LINE NOTE: selection != "ocba_rollout" &&
      selection != "ocba_rollout" &&
      // LINE NOTE: selection != "ttts_rollout") {
      selection != "ttts_rollout") {
    // LINE NOTE: throw std::range_error(
    throw std::range_error(
        // LINE NOTE: "`selection` must be one of \"first\", \"random\", \"aggressive\", \"defensive\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"thompson_rollout\", \"ttts_rollout\", or \"ocba_rollout\".");
        "`selection` must be one of \"first\", \"random\", \"aggressive\", \"defensive\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"thompson_rollout\", \"ttts_rollout\", or \"ocba_rollout\".");
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Validate a rollout continuation policy label.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate a rollout continuation policy label.
// LINE NOTE: bool is_supported_rollout_policy(const std::string& policy) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool is_supported_rollout_policy(const std::string& policy) {
  // LINE NOTE: return policy == "random" || policy == "aggressive" || policy == "defensive";
  return policy == "random" || policy == "aggressive" || policy == "defensive";
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Validate dice stratification mode used by allocation/rollout experiments.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate dice stratification mode used by allocation/rollout experiments.
// LINE NOTE: bool is_supported_dice_mode(const std::string& dice_mode) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool is_supported_dice_mode(const std::string& dice_mode) {
  // LINE NOTE: return dice_mode == "iid" ||
  return dice_mode == "iid" ||
      // LINE NOTE: dice_mode == "stratified_first_roll" ||
      dice_mode == "stratified_first_roll" ||
      // LINE NOTE: dice_mode == "stratified_first_two_rolls";
      dice_mode == "stratified_first_two_rolls";
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Validate complete rollout configuration prior to simulation.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate complete rollout configuration prior to simulation.
// LINE NOTE: void validate_rollout_config(const RolloutConfig& config) {
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void validate_rollout_config(const RolloutConfig& config) {
  // LINE NOTE: // Budget must allocate at least one rollout.
  // Budget must allocate at least one rollout.
  // LINE NOTE: if (config.budget < 1) {
  if (config.budget < 1) {
    // LINE NOTE: throw std::range_error("`rollout_budget` must be at least 1.");
    throw std::range_error("`rollout_budget` must be at least 1.");
  // LINE NOTE: }
  }

  // LINE NOTE: // Continuation policy must be one of the supported heuristic policies.
  // Continuation policy must be one of the supported heuristic policies.
  // LINE NOTE: if (!is_supported_rollout_policy(config.policy)) {
  if (!is_supported_rollout_policy(config.policy)) {
    // LINE NOTE: throw std::range_error(
    throw std::range_error(
        // LINE NOTE: "`rollout_policy` must be one of \"random\", \"aggressive\", or \"defensive\".");
        "`rollout_policy` must be one of \"random\", \"aggressive\", or \"defensive\".");
  // LINE NOTE: }
  }

  // LINE NOTE: // Turn cap may be zero (immediate unresolved) but not negative.
  // Turn cap may be zero (immediate unresolved) but not negative.
  // LINE NOTE: if (config.max_turns < 0) {
  if (config.max_turns < 0) {
    // LINE NOTE: throw std::range_error("`max_rollout_turns` must be nonnegative.");
    throw std::range_error("`max_rollout_turns` must be nonnegative.");
  // LINE NOTE: }
  }

  // LINE NOTE: // UCB exploration coefficient (reused as TTTS beta in TTTS path) must be >= 0.
  // UCB exploration coefficient (reused as TTTS beta in TTTS path) must be >= 0.
  // LINE NOTE: if (config.ucb_exploration < 0.0) {
  if (config.ucb_exploration < 0.0) {
    // LINE NOTE: throw std::range_error("`ucb_exploration` must be nonnegative.");
    throw std::range_error("`ucb_exploration` must be nonnegative.");
  // LINE NOTE: }
  }

  // LINE NOTE: // Beta prior parameters must be positive for conjugate posterior updates.
  // Beta prior parameters must be positive for conjugate posterior updates.
  // LINE NOTE: if (config.prior_alpha <= 0.0 || config.prior_beta <= 0.0) {
  if (config.prior_alpha <= 0.0 || config.prior_beta <= 0.0) {
    // LINE NOTE: throw std::range_error("`prior_alpha` and `prior_beta` must be positive.");
    throw std::range_error("`prior_alpha` and `prior_beta` must be positive.");
  // LINE NOTE: }
  }

  // LINE NOTE: // Initial warm-start allocations cannot be negative.
  // Initial warm-start allocations cannot be negative.
  // LINE NOTE: if (config.initial_allocations < 0) {
  if (config.initial_allocations < 0) {
    // LINE NOTE: throw std::range_error("`initial_allocations` must be nonnegative.");
    throw std::range_error("`initial_allocations` must be nonnegative.");
  // LINE NOTE: }
  }

  // LINE NOTE: // Unresolved reward must be a valid probability-scale value.
  // Unresolved reward must be a valid probability-scale value.
  // LINE NOTE: if (config.unresolved_value < 0.0 || config.unresolved_value > 1.0) {
  if (config.unresolved_value < 0.0 || config.unresolved_value > 1.0) {
    // LINE NOTE: throw std::range_error("`unresolved_value` must lie between 0 and 1.");
    throw std::range_error("`unresolved_value` must lie between 0 and 1.");
  // LINE NOTE: }
  }

  // LINE NOTE: // Dice mode controls variance-reduction schedule for early rolls.
  // Dice mode controls variance-reduction schedule for early rolls.
  // LINE NOTE: if (!is_supported_dice_mode(config.dice_mode)) {
  if (!is_supported_dice_mode(config.dice_mode)) {
    // LINE NOTE: throw std::range_error(
    throw std::range_error(
        // LINE NOTE: "`dice_mode` must be one of \"iid\", \"stratified_first_roll\", or \"stratified_first_two_rolls\".");
        "`dice_mode` must be one of \"iid\", \"stratified_first_roll\", or \"stratified_first_two_rolls\".");
  // LINE NOTE: }
  }
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Evaluate each legal move with equal-allocation rollouts and return compact
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Evaluate each legal move with equal-allocation rollouts and return compact
// LINE NOTE: // win/loss/unresolved summaries.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// win/loss/unresolved summaries.
// LINE NOTE: std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):**
  // Step 1: call the shared allocation engine with method fixed to `"equal"`.
  // Step 2: map generic action summaries into rollout-specific summary rows.
  // Step 3: keep only fields needed for user interpretation in this context.
  // **IN PLAIN ENGLISH:** This function is a thin adapter that says "run the
  // common evaluator in equal-allocation mode, then present cleaner output."
  // LINE NOTE: // Reuse the shared allocation engine using canonical method = "equal".
  // Reuse the shared allocation engine using canonical method = "equal".
  // LINE NOTE: const std::vector<ActionEvaluationSummary> summaries =
  const std::vector<ActionEvaluationSummary> summaries =
      // LINE NOTE: evaluate_move_sequences_with_allocation(board, legal_moves, "equal", config, rng);
      evaluate_move_sequences_with_allocation(board, legal_moves, "equal", config, rng);

  // LINE NOTE: std::vector<RolloutMoveSummary> out;
  std::vector<RolloutMoveSummary> out;
  // LINE NOTE: out.reserve(summaries.size());
  out.reserve(summaries.size());

  // LINE NOTE: for (const ActionEvaluationSummary& summary : summaries) {
  for (const ActionEvaluationSummary& summary : summaries) {
    // One output row per candidate action; this preserves one-to-one mapping
    // from the internal evaluator to the user-facing table.
    // LINE NOTE: RolloutMoveSummary row;
    RolloutMoveSummary row;
    // LINE NOTE: // Candidate index is 1-based in user-facing outputs.
    // Candidate index is 1-based in user-facing outputs.
    // LINE NOTE: row.candidate_index = summary.candidate_index;
    row.candidate_index = summary.candidate_index;
    // LINE NOTE: // Raw outcome counts are carried over directly.
    // Raw outcome counts are carried over directly.
    // LINE NOTE: row.wins = summary.wins;
    row.wins = summary.wins;
    // LINE NOTE: row.losses = summary.losses;
    row.losses = summary.losses;
    // LINE NOTE: row.unresolved = summary.unresolved;
    row.unresolved = summary.unresolved;
    // LINE NOTE: // Prefer empirical value when available; otherwise fall back to posterior estimate.
    // Prefer empirical value when available; otherwise fall back to posterior estimate.
    // LINE NOTE: row.win_rate = Rcpp::NumericVector::is_na(summary.empirical_value)
    row.win_rate = Rcpp::NumericVector::is_na(summary.empirical_value)
        // LINE NOTE: ? summary.estimate
        ? summary.estimate
        // LINE NOTE: : summary.empirical_value;
        : summary.empirical_value;
    // LINE NOTE: out.push_back(row);
    out.push_back(row);
  // LINE NOTE: }
  }

  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Choose one move under equal-allocation rollout policy.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Choose one move under equal-allocation rollout policy.
// LINE NOTE: MoveSequence choose_rollout_move_sequence(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
MoveSequence choose_rollout_move_sequence(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Delegates move choice to the same shared
  // engine as other methods, but hard-codes equal allocation so behavior is
  // predictable and directly comparable in benchmarks.
  // **IN PLAIN ENGLISH:** This asks the common decision engine to pick one move
  // using non-adaptive equal-budget rollout logic.
  // LINE NOTE: // Again, selection delegates to shared allocation engine with "equal" method.
  // Again, selection delegates to shared allocation engine with "equal" method.
  // LINE NOTE: return choose_move_sequence_with_allocation(board, legal_moves, "equal", config, rng);
  return choose_move_sequence_with_allocation(board, legal_moves, "equal", config, rng);
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // Convert rollout summary vector to an R data.frame.
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Convert rollout summary vector to an R data.frame.
// LINE NOTE: Rcpp::DataFrame rollout_move_summaries_to_data_frame(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::DataFrame rollout_move_summaries_to_data_frame(
    // LINE NOTE: const std::vector<RolloutMoveSummary>& summaries) {
    const std::vector<RolloutMoveSummary>& summaries) {
  // **WHAT IT'S DOING (DETAILED):** Converts vector-of-struct storage (C++) to
  // columnar vectors (R data frame). This is required because R tables are
  // column-oriented and downstream plotting/printing expects that layout.
  // **IN PLAIN ENGLISH:** We reshape C++ objects into an R table format.
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
  // LINE NOTE: Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector candidate_index(n);
  // LINE NOTE: Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector wins(n);
  // LINE NOTE: Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector losses(n);
  // LINE NOTE: Rcpp::IntegerVector unresolved(n);
  Rcpp::IntegerVector unresolved(n);
  // LINE NOTE: Rcpp::NumericVector win_rate(n);
  Rcpp::NumericVector win_rate(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: candidate_index[i] = summaries[i].candidate_index;
    candidate_index[i] = summaries[i].candidate_index;
    // LINE NOTE: wins[i] = summaries[i].wins;
    wins[i] = summaries[i].wins;
    // LINE NOTE: losses[i] = summaries[i].losses;
    losses[i] = summaries[i].losses;
    // LINE NOTE: unresolved[i] = summaries[i].unresolved;
    unresolved[i] = summaries[i].unresolved;
    // LINE NOTE: win_rate[i] = summaries[i].win_rate;
    win_rate[i] = summaries[i].win_rate;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["candidate_index"] = candidate_index,
      // LINE NOTE: Rcpp::_["wins"] = wins,
      Rcpp::_["wins"] = wins,
      // LINE NOTE: Rcpp::_["losses"] = losses,
      Rcpp::_["losses"] = losses,
      // LINE NOTE: Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["unresolved"] = unresolved,
      // LINE NOTE: Rcpp::_["win_rate"] = win_rate,
      Rcpp::_["win_rate"] = win_rate,
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

// LINE NOTE: // [[Rcpp::export]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// LINE NOTE: Rcpp::DataFrame bg_cpp_rollout_move_evaluate(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::DataFrame bg_cpp_rollout_move_evaluate(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& legal_moves,
    const Rcpp::List& legal_moves,
    // LINE NOTE: const int rollout_budget,
    const int rollout_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):**
  // - Parse R inputs into engine-native board/move objects.
  // - Build rollout configuration from scalar parameters.
  // - Create deterministic RNG if seed is requested.
  // - Run equal-allocation evaluation and return standardized action table.
  // **IN PLAIN ENGLISH:** This is the R-to-C++ gateway for "evaluate moves with
  // equal rollout allocation."
  // LINE NOTE: // Parse and validate R-side inputs into C++ engine types.
  // Parse and validate R-side inputs into C++ engine types.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // LINE NOTE: // Build per-call RNG.
  // Build per-call RNG.
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Return full allocation-style table so downstream R summaries stay consistent.
  // Return full allocation-style table so downstream R summaries stay consistent.
  // We intentionally return the standardized allocation table (not a tiny custom
  // table) so all methods share the same downstream print/summary code paths.
  // LINE NOTE: return backgammonr::action_evaluation_summaries_to_data_frame(
  return backgammonr::action_evaluation_summaries_to_data_frame(
      // LINE NOTE: backgammonr::evaluate_move_sequences_with_allocation(
      backgammonr::evaluate_move_sequences_with_allocation(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: "equal",
          "equal",
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng));
          rng));
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// LINE NOTE: // [[Rcpp::export]]
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_rollout_move_choice(
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_rollout_move_choice(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& legal_moves,
    const Rcpp::List& legal_moves,
    // LINE NOTE: const int rollout_budget,
    const int rollout_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):** Same parse/config/RNG steps as the evaluate
  // wrapper, but returns exactly one selected move sequence instead of a full
  // per-candidate table.
  // **IN PLAIN ENGLISH:** This is the "just tell me the move" version.
  // LINE NOTE: // Parse and validate R-side inputs into C++ engine types.
  // Parse and validate R-side inputs into C++ engine types.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // LINE NOTE: // Build per-call RNG.
  // Build per-call RNG.
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Choose one move under equal-allocation rollout logic and convert to R list.
  // Choose one move under equal-allocation rollout logic and convert to R list.
  // LINE NOTE: return backgammonr::move_sequence_to_list(
  return backgammonr::move_sequence_to_list(
      // LINE NOTE: backgammonr::choose_move_sequence_with_allocation(
      backgammonr::choose_move_sequence_with_allocation(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: "equal",
          "equal",
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng));
          rng));
// LINE NOTE: }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}
