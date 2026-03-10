#ifndef BACKGAMMONR_BG_RULES_H
#define BACKGAMMONR_BG_RULES_H

#include <array>
#include <optional>
#include <vector>

#include "bg_board.h"
#include "bg_move.h"

namespace backgammonr {

int player_index(int player);
int opponent_index(int player);
int player_checker_count_on_point(const BoardState& board, int player, int point);
int opponent_checker_count_on_point(const BoardState& board, int player, int point);
bool point_is_open_to_player(const BoardState& board, int player, int point);
bool point_has_opponent_blot(const BoardState& board, int player, int point);
bool player_has_bar_checkers(const BoardState& board, int player);
bool is_home_point(int player, int point);
bool all_checkers_in_home(const BoardState& board, int player);
int bar_entry_point(int player, int die);
bool has_checkers_farther_from_off_in_home(const BoardState& board, int player, int point);
std::optional<MoveStep> legal_step_from_source(const BoardState& board, int player, int from, int die);
int legal_steps_for_die_into_array(
    const BoardState& board,
    int player,
    int die,
    std::array<MoveStep, kNumPoints>& steps);
void legal_steps_for_die_into(const BoardState& board, int player, int die, std::vector<MoveStep>& steps);
std::vector<MoveStep> legal_steps_for_die(const BoardState& board, int player, int die);
void apply_move_step_unchecked_inplace(BoardState& board, int player, const MoveStep& step);
void apply_move_step_inplace(BoardState& board, int player, const MoveStep& step);
BoardState apply_move_step_unchecked(const BoardState& board, int player, const MoveStep& step);
BoardState apply_move_step(const BoardState& board, int player, const MoveStep& step);

}  // namespace backgammonr

#endif
