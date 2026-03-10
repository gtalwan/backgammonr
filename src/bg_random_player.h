#ifndef BACKGAMMONR_BG_RANDOM_PLAYER_H
#define BACKGAMMONR_BG_RANDOM_PLAYER_H

#include <random>
#include <vector>

#include "bg_move.h"

namespace backgammonr {

MoveSequence choose_random_move_sequence(const std::vector<MoveSequence>& legal_moves, std::mt19937& rng);

}  // namespace backgammonr

#endif
