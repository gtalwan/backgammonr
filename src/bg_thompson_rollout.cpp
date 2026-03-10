// LINE NOTE: #include "bg_thompson_rollout.h"
#include "bg_thompson_rollout.h"

// LINE NOTE: #include <cstdint>
#include <cstdint>
// LINE NOTE: #include <random>
#include <random>
// LINE NOTE: #include <stdexcept>
#include <stdexcept>
// LINE NOTE: #include <vector>
#include <vector>

// LINE NOTE: #include "bg_allocation.h"
#include "bg_allocation.h"

// LINE NOTE: // -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// LINE NOTE: // bg_thompson_rollout.cpp
// bg_thompson_rollout.cpp
// LINE NOTE: //
//
// LINE NOTE: // Thompson-specific rollout wrappers.
// Thompson-specific rollout wrappers.
// LINE NOTE: //
//
// LINE NOTE: // Why this file exists:
// Why this file exists:
// LINE NOTE: // - R users often want a dedicated "Thompson rollout" entry point.
// - R users often want a dedicated "Thompson rollout" entry point.
// LINE NOTE: // - Internally, we still route to the common allocation engine in
// - Internally, we still route to the common allocation engine in
// LINE NOTE: //   bg_allocation.cpp to avoid duplicated logic.
//   bg_allocation.cpp to avoid duplicated logic.
// LINE NOTE: // - Here, we only fix method = "thompson" and shape the returned fields.
// - Here, we only fix method = "thompson" and shape the returned fields.
// LINE NOTE: // -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

// LINE NOTE: namespace {
namespace {

// LINE NOTE: // Construct RNG for this call.
// Construct RNG for this call.
// LINE NOTE: // - Deterministic when a seed is provided.
// - Deterministic when a seed is provided.
// LINE NOTE: // - Otherwise seeded from entropy source.
// - Otherwise seeded from entropy source.
// LINE NOTE: std::mt19937 init_rng(const int seed, const bool use_seed) {
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
}

// LINE NOTE: }  // namespace
}  // namespace

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: // Evaluate legal moves with Thompson allocation and return a Thompson-focused
// Evaluate legal moves with Thompson allocation and return a Thompson-focused
// LINE NOTE: // compact structure.
// compact structure.
// LINE NOTE: std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: // Shared engine call with canonical method label = "thompson".
  // Shared engine call with canonical method label = "thompson".
  // LINE NOTE: const std::vector<ActionEvaluationSummary> summaries =
  const std::vector<ActionEvaluationSummary> summaries =
      // LINE NOTE: evaluate_move_sequences_with_allocation(board, legal_moves, "thompson", config, rng);
      evaluate_move_sequences_with_allocation(board, legal_moves, "thompson", config, rng);

  // LINE NOTE: std::vector<ThompsonRolloutMoveSummary> out;
  std::vector<ThompsonRolloutMoveSummary> out;
  // LINE NOTE: out.reserve(summaries.size());
  out.reserve(summaries.size());

  // LINE NOTE: for (const ActionEvaluationSummary& summary : summaries) {
  for (const ActionEvaluationSummary& summary : summaries) {
    // LINE NOTE: ThompsonRolloutMoveSummary row;
    ThompsonRolloutMoveSummary row;
    // LINE NOTE: // Preserve candidate identity.
    // Preserve candidate identity.
    // LINE NOTE: row.candidate_index = summary.candidate_index;
    row.candidate_index = summary.candidate_index;
    // LINE NOTE: // Keep allocation intensity for interpretability.
    // Keep allocation intensity for interpretability.
    // LINE NOTE: row.allocation_count = summary.allocation_count;
    row.allocation_count = summary.allocation_count;
    // LINE NOTE: // Keep raw outcomes for diagnostics.
    // Keep raw outcomes for diagnostics.
    // LINE NOTE: row.wins = summary.wins;
    row.wins = summary.wins;
    // LINE NOTE: row.losses = summary.losses;
    row.losses = summary.losses;
    // LINE NOTE: row.unresolved = summary.unresolved;
    row.unresolved = summary.unresolved;
    // LINE NOTE: // Keep posterior sufficient statistics (Beta alpha/beta).
    // Keep posterior sufficient statistics (Beta alpha/beta).
    // LINE NOTE: row.alpha = summary.alpha;
    row.alpha = summary.alpha;
    // LINE NOTE: row.beta = summary.beta;
    row.beta = summary.beta;
    // LINE NOTE: // Posterior mean is the Thompson value estimate shown to users.
    // Posterior mean is the Thompson value estimate shown to users.
    // LINE NOTE: row.posterior_mean = summary.estimate;
    row.posterior_mean = summary.estimate;
    // LINE NOTE: // Empirical win rate gives a direct frequentist-style view.
    // Empirical win rate gives a direct frequentist-style view.
    // LINE NOTE: row.empirical_win_rate = summary.empirical_value;
    row.empirical_win_rate = summary.empirical_value;
    // LINE NOTE: out.push_back(row);
    out.push_back(row);
  // LINE NOTE: }
  }

  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Choose a single move with Thompson allocation.
// Choose a single move with Thompson allocation.
// LINE NOTE: MoveSequence choose_thompson_rollout_move_sequence(
MoveSequence choose_thompson_rollout_move_sequence(
    // LINE NOTE: const BoardState& board,
    const BoardState& board,
    // LINE NOTE: const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
    // LINE NOTE: const RolloutConfig& config,
    const RolloutConfig& config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
  return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
// LINE NOTE: }
}

// LINE NOTE: // Convert Thompson summaries to an R data.frame.
// Convert Thompson summaries to an R data.frame.
// LINE NOTE: Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
    // LINE NOTE: const std::vector<ThompsonRolloutMoveSummary>& summaries) {
    const std::vector<ThompsonRolloutMoveSummary>& summaries) {
  // LINE NOTE: const int n = static_cast<int>(summaries.size());
  const int n = static_cast<int>(summaries.size());
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
  // LINE NOTE: Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector alpha(n);
  // LINE NOTE: Rcpp::NumericVector beta(n);
  Rcpp::NumericVector beta(n);
  // LINE NOTE: Rcpp::NumericVector posterior_mean(n);
  Rcpp::NumericVector posterior_mean(n);
  // LINE NOTE: Rcpp::NumericVector empirical_win_rate(n);
  Rcpp::NumericVector empirical_win_rate(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: candidate_index[i] = summaries[i].candidate_index;
    candidate_index[i] = summaries[i].candidate_index;
    // LINE NOTE: allocation_count[i] = summaries[i].allocation_count;
    allocation_count[i] = summaries[i].allocation_count;
    // LINE NOTE: wins[i] = summaries[i].wins;
    wins[i] = summaries[i].wins;
    // LINE NOTE: losses[i] = summaries[i].losses;
    losses[i] = summaries[i].losses;
    // LINE NOTE: unresolved[i] = summaries[i].unresolved;
    unresolved[i] = summaries[i].unresolved;
    // LINE NOTE: alpha[i] = summaries[i].alpha;
    alpha[i] = summaries[i].alpha;
    // LINE NOTE: beta[i] = summaries[i].beta;
    beta[i] = summaries[i].beta;
    // LINE NOTE: posterior_mean[i] = summaries[i].posterior_mean;
    posterior_mean[i] = summaries[i].posterior_mean;
    // LINE NOTE: empirical_win_rate[i] = summaries[i].empirical_win_rate;
    empirical_win_rate[i] = summaries[i].empirical_win_rate;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
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
      // LINE NOTE: Rcpp::_["alpha"] = alpha,
      Rcpp::_["alpha"] = alpha,
      // LINE NOTE: Rcpp::_["beta"] = beta,
      Rcpp::_["beta"] = beta,
      // LINE NOTE: Rcpp::_["posterior_mean"] = posterior_mean,
      Rcpp::_["posterior_mean"] = posterior_mean,
      // LINE NOTE: Rcpp::_["empirical_win_rate"] = empirical_win_rate,
      Rcpp::_["empirical_win_rate"] = empirical_win_rate,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
}

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
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
  // LINE NOTE: // Parse R objects into C++ engine structures.
  // Parse R objects into C++ engine structures.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // LINE NOTE: // Build local RNG stream.
  // Build local RNG stream.
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Return the full standardized evaluation table for consistency with
  // Return the full standardized evaluation table for consistency with
  // LINE NOTE: // evaluate_actions_thompson() R-side wrappers.
  // evaluate_actions_thompson() R-side wrappers.
  // LINE NOTE: return backgammonr::action_evaluation_summaries_to_data_frame(
  return backgammonr::action_evaluation_summaries_to_data_frame(
      // LINE NOTE: backgammonr::evaluate_move_sequences_with_allocation(
      backgammonr::evaluate_move_sequences_with_allocation(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: "thompson",
          "thompson",
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng));
          rng));
// LINE NOTE: }
}

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_thompson_rollout_move_choice(
Rcpp::List bg_cpp_thompson_rollout_move_choice(
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
  // LINE NOTE: // Parse R objects into C++ engine structures.
  // Parse R objects into C++ engine structures.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::MoveSequence> parsed_moves =
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      // LINE NOTE: backgammonr::parse_move_sequence_vector(legal_moves);
      backgammonr::parse_move_sequence_vector(legal_moves);
  // LINE NOTE: const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // LINE NOTE: // Build local RNG stream.
  // Build local RNG stream.
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Compute Thompson-selected move and convert back to R representation.
  // Compute Thompson-selected move and convert back to R representation.
  // LINE NOTE: return backgammonr::move_sequence_to_list(
  return backgammonr::move_sequence_to_list(
      // LINE NOTE: backgammonr::choose_move_sequence_with_allocation(
      backgammonr::choose_move_sequence_with_allocation(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_moves,
          parsed_moves,
          // LINE NOTE: "thompson",
          "thompson",
          // LINE NOTE: config,
          config,
          // LINE NOTE: rng));
          rng));
// LINE NOTE: }
}
