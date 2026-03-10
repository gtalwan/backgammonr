#ifndef BACKGAMMONR_BG_DICE_H
#define BACKGAMMONR_BG_DICE_H

#include <Rcpp.h>
#include <array>
#include <random>
#include <string>
#include <vector>

namespace backgammonr {

inline constexpr int kNumDice = 2;
inline constexpr int kDoubleExpandedDice = 4;
inline constexpr int kMinDieValue = 1;
inline constexpr int kMaxDieValue = 6;

struct DiceRoll {
  std::array<int, kNumDice> dice{};

  bool is_double() const {
    return dice[0] == dice[1];
  }
};

DiceRoll make_roll(int die1, int die2);
DiceRoll roll_dice(std::mt19937& rng);
Rcpp::IntegerVector expanded_roll(const DiceRoll& roll);
Rcpp::List roll_to_list(const DiceRoll& roll);
std::vector<std::string> validate_roll_list(const Rcpp::List& roll);
DiceRoll parse_roll_list(const Rcpp::List& roll);

}  // namespace backgammonr

#endif
