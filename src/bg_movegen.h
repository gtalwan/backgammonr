#ifndef BACKGAMMONR_BG_MOVEGEN_H
#define BACKGAMMONR_BG_MOVEGEN_H

#include <random>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"
#include "bg_move.h"

namespace backgammonr {

std::vector<MoveSequence> generate_legal_move_sequences(
    const BoardState& board,
    int player,
    const DiceRoll& roll);

bool play_random_turn_rollout_fast(
    BoardState& board,
    const DiceRoll& roll,
    std::mt19937& rng);

}  // namespace backgammonr

#endif
