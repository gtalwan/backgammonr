// LINE NOTE: #ifndef BACKGAMMONR_BG_THOMPSON_ROLLOUT_H
#ifndef BACKGAMMONR_BG_THOMPSON_ROLLOUT_H
// LINE NOTE: #define BACKGAMMONR_BG_THOMPSON_ROLLOUT_H
#define BACKGAMMONR_BG_THOMPSON_ROLLOUT_H

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>
// LINE NOTE: #include <random>
#include <random>
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

// LINE NOTE: struct ThompsonRolloutMoveSummary {
struct ThompsonRolloutMoveSummary {
// LINE NOTE:   int candidate_index{0};
  int candidate_index{0};
// LINE NOTE:   int allocation_count{0};
  int allocation_count{0};
// LINE NOTE:   int wins{0};
  int wins{0};
// LINE NOTE:   int losses{0};
  int losses{0};
// LINE NOTE:   int unresolved{0};
  int unresolved{0};
// LINE NOTE:   double alpha{1.0};
  double alpha{1.0};
// LINE NOTE:   double beta{1.0};
  double beta{1.0};
// LINE NOTE:   double posterior_mean{0.5};
  double posterior_mean{0.5};
// LINE NOTE:   double empirical_win_rate{NA_REAL};
  double empirical_win_rate{NA_REAL};
// LINE NOTE: };
};

// LINE NOTE: std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);
// LINE NOTE: MoveSequence choose_thompson_rollout_move_sequence(
MoveSequence choose_thompson_rollout_move_sequence(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);
// LINE NOTE: Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
// LINE NOTE:     const std::vector<ThompsonRolloutMoveSummary>& summaries);
    const std::vector<ThompsonRolloutMoveSummary>& summaries);

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: #endif
#endif
