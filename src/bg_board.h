#ifndef BACKGAMMONR_BG_BOARD_H
#define BACKGAMMONR_BG_BOARD_H

#include <Rcpp.h>
#include <array>
#include <string>
#include <vector>

namespace backgammonr {

inline constexpr int kNumPoints = 24;
inline constexpr int kNumPlayers = 2;
inline constexpr int kCheckersPerPlayer = 15;

struct BoardState {
  std::array<int, kNumPoints> points{};
  std::array<int, kNumPlayers> bar{};
  std::array<int, kNumPlayers> off{};
  int turn{1};
};

BoardState initial_board_state(int turn);
Rcpp::List board_to_list(const BoardState& board);
std::vector<std::string> validate_board_list(const Rcpp::List& board);
BoardState parse_board_list(const Rcpp::List& board);
Rcpp::List clone_board_list(const Rcpp::List& board);

}  // namespace backgammonr

#endif
