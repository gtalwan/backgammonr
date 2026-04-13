// Public allocation API wrappers.
//
// The heavy lifting lives in the internal allocation core and policy files.
// This file only keeps the stable package-facing allocation API:
// - method validation / canonicalization
// - move-sequence evaluation and choice
// - summary-table conversion

#include "alloc_interface.h"

#include <cmath>
#include <limits>

#include "alloc_core.h"

namespace backgammonr {

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
  return allocation::evaluate_with_optional_trace(
      board,
      legal_moves,
      method,
      config,
      rng,
      1,
      nullptr);
}

int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries) {
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
