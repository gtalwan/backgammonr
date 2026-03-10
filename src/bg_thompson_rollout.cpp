#include "bg_thompson_rollout.h"

#include <cstdint>
#include <random>
#include <stdexcept>
#include <vector>

#include "bg_allocation.h"

// -----------------------------------------------------------------------------
// bg_thompson_rollout.cpp
//
// Thompson-specific rollout wrappers.
//
// Why this file exists:
// - R users often want a dedicated "Thompson rollout" entry point.
// - Internally, we still route to the common allocation engine in
//   bg_allocation.cpp to avoid duplicated logic.
// - Here, we only fix method = "thompson" and shape the returned fields.
// -----------------------------------------------------------------------------

namespace {

// Function: init_rng
// Purpose: Build per-call RNG stream for Thompson rollout wrappers.
// Called by: bg_cpp_thompson_rollout_move_evaluate(),
// bg_cpp_thompson_rollout_move_choice().
// Notes: Keeps deterministic behavior when seed is supplied.
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

// Function: evaluate_thompson_rollout_move_sequences
// Purpose: Evaluate legal moves with Thompson allocation and return compact
// Thompson-focused summaries.
// Called by: Thompson-specific R wrappers and unit tests.
// Notes: Delegates to shared allocation engine to avoid duplicated logic.
std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Step 1: run shared allocator with method fixed to "thompson".
  // Step 2: map generic summaries into a Thompson-specific result struct.
  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, "thompson", config, rng);

  std::vector<ThompsonRolloutMoveSummary> out;
  out.reserve(summaries.size());

  for (const ActionEvaluationSummary& summary : summaries) {
    // Keep this mapping explicit so each output field is easy to audit against
    // the generic `ActionEvaluationSummary` source.
    ThompsonRolloutMoveSummary row;
    // Preserve candidate identity.
    row.candidate_index = summary.candidate_index;
    // Keep allocation intensity for interpretability.
    row.allocation_count = summary.allocation_count;
    // Keep raw outcomes for diagnostics.
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    // Keep posterior sufficient statistics (Beta alpha/beta).
    row.alpha = summary.alpha;
    row.beta = summary.beta;
    // Posterior mean is the Thompson value estimate shown to users.
    row.posterior_mean = summary.estimate;
    // Empirical win rate gives a direct frequentist-style view.
    row.empirical_win_rate = summary.empirical_value;
    out.push_back(row);
  }

  return out;
}

// Function: choose_thompson_rollout_move_sequence
// Purpose: Choose one legal move under Thompson allocation.
// Called by: Thompson R choice wrapper and benchmark paths.
// Notes: Shares tie-breaking and posterior logic with generic allocator.
MoveSequence choose_thompson_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Hard-code method = "thompson" and defer core selection to shared chooser.
  return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
}

// Function: thompson_rollout_move_summaries_to_data_frame
// Purpose: Convert Thompson summary structs into a stable R data-frame schema.
// Called by: Thompson evaluate wrappers and R-facing output helpers.
// Notes: Keeps column names and ordering stable for docs/examples/tests.
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
    const std::vector<ThompsonRolloutMoveSummary>& summaries) {
  const int n = static_cast<int>(summaries.size());
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector beta(n);
  Rcpp::NumericVector posterior_mean(n);
  Rcpp::NumericVector empirical_win_rate(n);

  for (int i = 0; i < n; ++i) {
    candidate_index[i] = summaries[i].candidate_index;
    allocation_count[i] = summaries[i].allocation_count;
    wins[i] = summaries[i].wins;
    losses[i] = summaries[i].losses;
    unresolved[i] = summaries[i].unresolved;
    alpha[i] = summaries[i].alpha;
    beta[i] = summaries[i].beta;
    posterior_mean[i] = summaries[i].posterior_mean;
    empirical_win_rate[i] = summaries[i].empirical_win_rate;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta,
      Rcpp::_["posterior_mean"] = posterior_mean,
      Rcpp::_["empirical_win_rate"] = empirical_win_rate,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace backgammonr

// [[Rcpp::export]]
// Function: bg_cpp_thompson_rollout_move_evaluate
// Purpose: Rcpp entry point for Thompson move evaluation (table output).
// Called by: R function `evaluate_actions_thompson()` through RcppExports.
// Notes: Returns standardized action-evaluation table used across methods.
Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R lists/scalars into C++ engine structures.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build local RNG stream.
  std::mt19937 rng = init_rng(seed, use_seed);

  // Return the full standardized evaluation table for consistency with
  // evaluate_actions_thompson() R-side wrappers.
  return backgammonr::action_evaluation_summaries_to_data_frame(
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          "thompson",
          config,
          rng));
}

// [[Rcpp::export]]
// Function: bg_cpp_thompson_rollout_move_choice
// Purpose: Rcpp entry point for one Thompson-selected move.
// Called by: R function `choose_action_thompson()` through RcppExports.
// Notes: Uses same parsing/config pattern as evaluate wrapper for consistency.
Rcpp::List bg_cpp_thompson_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R lists/scalars into C++ engine structures.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build local RNG stream.
  std::mt19937 rng = init_rng(seed, use_seed);

  // Compute Thompson-selected move and convert back to R representation.
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "thompson",
          config,
          rng));
}
