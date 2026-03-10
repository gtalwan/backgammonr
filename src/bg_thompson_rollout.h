#ifndef BACKGAMMONR_BG_THOMPSON_ROLLOUT_H
#define BACKGAMMONR_BG_THOMPSON_ROLLOUT_H

#include <Rcpp.h>
#include <random>
#include <vector>

#include "bg_board.h"
#include "bg_move.h"
#include "bg_rollout.h"

namespace backgammonr {

struct ThompsonRolloutMoveSummary {
  int candidate_index{0};
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int unresolved{0};
  double alpha{1.0};
  double beta{1.0};
  double posterior_mean{0.5};
  double empirical_win_rate{NA_REAL};
};

std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng);
MoveSequence choose_thompson_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng);
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
    const std::vector<ThompsonRolloutMoveSummary>& summaries);

}  // namespace backgammonr

#endif
