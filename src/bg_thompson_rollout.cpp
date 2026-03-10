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

// Construct RNG for this call.
// - Deterministic when a seed is provided.
// - Otherwise seeded from entropy source.
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

// Evaluate legal moves with Thompson allocation and return a Thompson-focused
// compact structure.
std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):**
  // 1) Call the shared allocation engine with method fixed to Thompson sampling.
  // 2) Receive generic action summaries (shared across all allocation methods).
  // 3) Project those fields into a Thompson-specific compact summary struct.
  // Shared engine call with canonical method label = "thompson".
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

// Choose a single move with Thompson allocation.
MoveSequence choose_thompson_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // **WHAT IT'S DOING (DETAILED):** Delegates to the shared chooser while
  // pinning method = `"thompson"`.
  return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
}

// Convert Thompson summaries to an R data.frame.
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
    const std::vector<ThompsonRolloutMoveSummary>& summaries) {
  // **WHAT IT'S DOING (DETAILED):** Marshals C++ Thompson summary rows into an
  // R data frame with explicit columns for allocation counts, outcomes, and
  // posterior parameters.
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
Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):**
  // - Parse R board/move lists into engine objects.
  // - Build rollout config.
  // - Initialize RNG stream (seeded or random-device).
  // - Run Thompson allocation evaluator.
  // - Return standardized action-evaluation table.
  // Parse R objects into C++ engine structures.
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
Rcpp::List bg_cpp_thompson_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):** Same parsing/config/RNG setup as the
  // evaluate wrapper, but returns only the selected move sequence.
  // Parse R objects into C++ engine structures.
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
