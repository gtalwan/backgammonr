#ifndef BACKGAMMONR_BG_MOVE_H
#define BACKGAMMONR_BG_MOVE_H

#include <Rcpp.h>
#include <optional>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"

namespace backgammonr {

inline constexpr int kBarPosition = 0;
inline constexpr int kOffPosition = kNumPoints + 1;

struct MoveStep {
  int from{kBarPosition};
  int to{1};
  int die{kMinDieValue};
  bool hit{false};
};

struct MoveSequence {
  int player{1};
  std::vector<MoveStep> steps{};
  std::optional<DiceRoll> roll{std::nullopt};
};

MoveStep make_move_step(int from, int to, int die, bool hit = false);
Rcpp::List move_step_to_list(const MoveStep& step);
std::vector<std::string> validate_move_step_list(const Rcpp::List& step);
MoveStep parse_move_step_list(const Rcpp::List& step);
std::vector<MoveStep> parse_move_step_vector(const Rcpp::List& steps);
MoveSequence make_move_sequence(
    int player,
    const std::vector<MoveStep>& steps,
    const std::optional<DiceRoll>& roll = std::nullopt);
std::vector<std::string> validate_move_sequence_list(const Rcpp::List& sequence);
MoveSequence parse_move_sequence_list(const Rcpp::List& sequence);
std::vector<MoveSequence> parse_move_sequence_vector(const Rcpp::List& sequences);
bool move_sequences_equal(const MoveSequence& lhs, const MoveSequence& rhs);
Rcpp::List move_sequence_to_list(const MoveSequence& sequence);

}  // namespace backgammonr

#endif
