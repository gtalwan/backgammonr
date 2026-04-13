#ifndef BACKGAMMONR_BG_ALLOCATION_H
#define BACKGAMMONR_BG_ALLOCATION_H

#include <Rcpp.h>
#include <random>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_move.h"
#include "bg_rollout.h"

namespace backgammonr {

struct ActionEvaluationSummary {
  int candidate_index{0};
  int n_equivalent_sequences{1};
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
  double prob_best{0.0};
  double posterior_expected_regret{0.0};
  double selection_score{0.5};
};

bool is_supported_allocation_method(const std::string& method);
void validate_allocation_method(const std::string& method);
std::string canonicalize_allocation_method(const std::string& method);

std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng);

MoveSequence choose_move_sequence_with_allocation(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng);

int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries);

Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
    const std::vector<ActionEvaluationSummary>& summaries);

}  // namespace backgammonr

#endif
