#include "bg_thompson_rollout.h"

#include <cstdint>
#include <random>
#include <stdexcept>
#include <vector>

#include "bg_allocation.h"

namespace {

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

std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, "thompson", config, rng);

  std::vector<ThompsonRolloutMoveSummary> out;
  out.reserve(summaries.size());

  for (const ActionEvaluationSummary& summary : summaries) {
    ThompsonRolloutMoveSummary row;
    row.candidate_index = summary.candidate_index;
    row.allocation_count = summary.allocation_count;
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    row.alpha = summary.alpha;
    row.beta = summary.beta;
    row.posterior_mean = summary.estimate;
    row.empirical_win_rate = summary.empirical_value;
    out.push_back(row);
  }

  return out;
}

MoveSequence choose_thompson_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
}

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
Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  std::mt19937 rng = init_rng(seed, use_seed);

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
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "thompson",
          config,
          rng));
}
