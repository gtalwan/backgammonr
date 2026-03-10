#ifndef BACKGAMMONR_BG_GAME_H
#define BACKGAMMONR_BG_GAME_H

#include <Rcpp.h>
#include <optional>
#include <random>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"
#include "bg_move.h"
#include "bg_rollout.h"

namespace backgammonr {

struct TurnResult {
  BoardState board_before{};
  BoardState board_after{};
  int player{1};
  DiceRoll roll{};
  std::vector<MoveSequence> legal_moves{};
  std::optional<MoveSequence> chosen_move{std::nullopt};
  std::string selection{"first"};
  bool turn_passed{false};
  bool game_over{false};
  int winner{0};
};

struct GameResult {
  BoardState initial_board{};
  BoardState final_board{};
  std::vector<TurnResult> turns{};
  bool game_over{false};
  int winner{0};
  int n_turns{0};
  bool turn_limit_reached{false};
  bool used_scripted_rolls{false};
  bool roll_sequence_exhausted{false};
  std::string selection{"first"};
  std::string player1_selection{"first"};
  std::string player2_selection{"first"};
};

bool board_is_terminal(const BoardState& board);
int board_winner(const BoardState& board);
BoardState apply_move_sequence_to_board(const BoardState& board, const MoveSequence& sequence);
MoveSequence choose_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& selection,
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());
TurnResult play_turn_with_roll(
    const BoardState& board,
    const DiceRoll& roll,
    const std::string& selection = "first",
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());
TurnResult play_turn_random(const BoardState& board, std::mt19937& rng, const std::string& selection = "first", const RolloutConfig& rollout_config = RolloutConfig());
GameResult play_game_random(
    const BoardState& initial_board,
    int max_turns,
    std::mt19937& rng,
    const std::string& selection = "first",
    const RolloutConfig& rollout_config = RolloutConfig());
GameResult play_game_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    int max_turns,
    const std::string& selection = "first",
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());
GameResult play_game_random_matchup(
    const BoardState& initial_board,
    int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config = RolloutConfig());
GameResult play_game_with_rolls_matchup(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());
Rcpp::List turn_result_to_list(const TurnResult& result);
Rcpp::List game_result_to_list(const GameResult& result);

}  // namespace backgammonr

#endif
