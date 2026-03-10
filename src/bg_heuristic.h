#ifndef BACKGAMMONR_BG_HEURISTIC_H
#define BACKGAMMONR_BG_HEURISTIC_H

#include <Rcpp.h>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_move.h"

namespace backgammonr {

struct BoardFeatures {
  int own_bar{0};
  int opponent_bar{0};
  int own_off{0};
  int opponent_off{0};
  int own_blots{0};
  int opponent_blots{0};
  int own_made_points{0};
  int own_home_made_points{0};
  int own_direct_hit_opportunities{0};
  int own_direct_hit_risk{0};
  int own_contact_checkers{0};
  int own_pip_count{0};
};

BoardFeatures extract_board_features(const BoardState& board, int player);
double aggressive_board_score(const BoardState& board, int player);
double defensive_board_score(const BoardState& board, int player);
double heuristic_board_score(const BoardState& board, int player, const std::string& selection);
MoveSequence choose_best_heuristic_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& selection);
Rcpp::List board_features_to_list(const BoardFeatures& features);

}  // namespace backgammonr

#endif
