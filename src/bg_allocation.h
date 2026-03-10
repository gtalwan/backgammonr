// LINE NOTE: #ifndef BACKGAMMONR_BG_ALLOCATION_H
#ifndef BACKGAMMONR_BG_ALLOCATION_H
// LINE NOTE: #define BACKGAMMONR_BG_ALLOCATION_H
#define BACKGAMMONR_BG_ALLOCATION_H

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>
// LINE NOTE: #include <random>
#include <random>
// LINE NOTE: #include <string>
#include <string>
// LINE NOTE: #include <vector>
#include <vector>

// LINE NOTE: #include "bg_board.h"
#include "bg_board.h"
// LINE NOTE: #include "bg_move.h"
#include "bg_move.h"
// LINE NOTE: #include "bg_rollout.h"
#include "bg_rollout.h"

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: struct ActionEvaluationSummary {
struct ActionEvaluationSummary {
// LINE NOTE:   int candidate_index{0};
  int candidate_index{0};
// LINE NOTE:   int n_equivalent_sequences{1};
  int n_equivalent_sequences{1};
// LINE NOTE:   int allocation_count{0};
  int allocation_count{0};
// LINE NOTE:   int wins{0};
  int wins{0};
// LINE NOTE:   int losses{0};
  int losses{0};
// LINE NOTE:   int unresolved{0};
  int unresolved{0};
// LINE NOTE:   double empirical_value{NA_REAL};
  double empirical_value{NA_REAL};
// LINE NOTE:   double alpha{1.0};
  double alpha{1.0};
// LINE NOTE:   double beta{1.0};
  double beta{1.0};
// LINE NOTE:   double estimate{0.5};
  double estimate{0.5};
// LINE NOTE:   double posterior_sd{0.0};
  double posterior_sd{0.0};
// LINE NOTE:   double lower_95{0.0};
  double lower_95{0.0};
// LINE NOTE:   double upper_95{1.0};
  double upper_95{1.0};
// LINE NOTE:   double prob_best{0.0};
  double prob_best{0.0};
// LINE NOTE:   double posterior_expected_regret{0.0};
  double posterior_expected_regret{0.0};
// LINE NOTE:   double selection_score{0.5};
  double selection_score{0.5};
// LINE NOTE: };
};

// LINE NOTE: bool is_supported_allocation_method(const std::string& method);
bool is_supported_allocation_method(const std::string& method);
// LINE NOTE: void validate_allocation_method(const std::string& method);
void validate_allocation_method(const std::string& method);
// LINE NOTE: std::string canonicalize_allocation_method(const std::string& method);
std::string canonicalize_allocation_method(const std::string& method);

// LINE NOTE: std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
std::vector<ActionEvaluationSummary> evaluate_move_sequences_with_allocation(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const std::string& method,
    const std::string& method,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);

// LINE NOTE: MoveSequence choose_move_sequence_with_allocation(
MoveSequence choose_move_sequence_with_allocation(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const std::string& method,
    const std::string& method,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);

// LINE NOTE: int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries);
int best_candidate_index(const std::vector<ActionEvaluationSummary>& summaries);

// LINE NOTE: Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
Rcpp::DataFrame action_evaluation_summaries_to_data_frame(
// LINE NOTE:     const std::vector<ActionEvaluationSummary>& summaries);
    const std::vector<ActionEvaluationSummary>& summaries);

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: #endif
#endif
