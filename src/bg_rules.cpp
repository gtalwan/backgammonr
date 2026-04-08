// Core backgammon rule helpers shared across engine components.
#include "bg_rules.h"

#include <algorithm>
#include <sstream>
#include <stdexcept>

namespace {

void validate_player(const int player) {
  if (player != 1 && player != -1) {
    throw std::range_error("`player` must be either 1L or -1L.");
  }
}

void validate_point(const int point) {
  if (point < 1 || point > backgammonr::kNumPoints) {
    throw std::range_error("`point` must be between 1 and 24.");
  }
}

inline int player_index_fast(const int player) {
  return player == 1 ? 0 : 1;
}

inline int opponent_index_fast(const int player) {
  return 1 - player_index_fast(player);
}

inline int player_checker_count_on_point_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int point) {
  const int raw = board.points[point - 1];
  if (player == 1) {
    return raw > 0 ? raw : 0;
  }
  return raw < 0 ? -raw : 0;
}

inline int opponent_checker_count_on_point_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int point) {
  const int raw = board.points[point - 1];
  if (player == 1) {
    return raw < 0 ? -raw : 0;
  }
  return raw > 0 ? raw : 0;
}

inline bool point_is_open_to_player_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int point) {
  return opponent_checker_count_on_point_fast(board, player, point) < 2;
}

inline bool point_has_opponent_blot_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int point) {
  return opponent_checker_count_on_point_fast(board, player, point) == 1;
}

inline bool player_has_bar_checkers_fast(
    const backgammonr::BoardState& board,
    const int player) {
  return board.bar[player_index_fast(player)] > 0;
}

inline bool is_home_point_fast(const int player, const int point) {
  if (player == 1) {
    return point >= 1 && point <= 6;
  }
  return point >= 19 && point <= 24;
}

bool all_checkers_in_home_fast(const backgammonr::BoardState& board, const int player) {
  if (player_has_bar_checkers_fast(board, player)) {
    return false;
  }

  if (player == 1) {
    for (int point = 7; point <= backgammonr::kNumPoints; ++point) {
      if (board.points[point - 1] > 0) {
        return false;
      }
    }
    return true;
  }

  for (int point = 1; point <= 18; ++point) {
    if (board.points[point - 1] < 0) {
      return false;
    }
  }
  return true;
}

inline int bar_entry_point_fast(const int player, const int die) {
  if (player == 1) {
    return backgammonr::kNumPoints + 1 - die;
  }
  return die;
}

bool has_checkers_farther_from_off_in_home_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int point) {
  if (!is_home_point_fast(player, point)) {
    return false;
  }

  if (player == 1) {
    for (int other_point = point + 1; other_point <= 6; ++other_point) {
      if (board.points[other_point - 1] > 0) {
        return true;
      }
    }
    return false;
  }

  for (int other_point = 19; other_point < point; ++other_point) {
    if (board.points[other_point - 1] < 0) {
      return true;
    }
  }
  return false;
}

std::optional<backgammonr::MoveStep> legal_step_from_source_fast(
    const backgammonr::BoardState& board,
    const int player,
    const int from,
    const int die) {
  if (die < backgammonr::kMinDieValue || die > backgammonr::kMaxDieValue) {
    return std::nullopt;
  }

  if (player_has_bar_checkers_fast(board, player)) {
    if (from != backgammonr::kBarPosition) {
      return std::nullopt;
    }

    const int to = bar_entry_point_fast(player, die);
    if (!point_is_open_to_player_fast(board, player, to)) {
      return std::nullopt;
    }

    backgammonr::MoveStep step;
    step.from = backgammonr::kBarPosition;
    step.to = to;
    step.die = die;
    step.hit = point_has_opponent_blot_fast(board, player, to);
    return step;
  }

  if (from == backgammonr::kBarPosition) {
    return std::nullopt;
  }

  if (from < 1 || from > backgammonr::kNumPoints) {
    return std::nullopt;
  }

  if (player_checker_count_on_point_fast(board, player, from) <= 0) {
    return std::nullopt;
  }

  const int to = player == 1 ? from - die : from + die;
  if (to >= 1 && to <= backgammonr::kNumPoints) {
    if (!point_is_open_to_player_fast(board, player, to)) {
      return std::nullopt;
    }

    backgammonr::MoveStep step;
    step.from = from;
    step.to = to;
    step.die = die;
    step.hit = point_has_opponent_blot_fast(board, player, to);
    return step;
  }

  if (!all_checkers_in_home_fast(board, player)) {
    return std::nullopt;
  }

  if (!is_home_point_fast(player, from)) {
    return std::nullopt;
  }

  const bool exact_bear_off = player == 1
      ? (from - die == 0)
      : (from + die == backgammonr::kOffPosition);
  const bool overshoot_bear_off = player == 1
      ? (from - die < 0)
      : (from + die > backgammonr::kOffPosition);

  if (exact_bear_off) {
    backgammonr::MoveStep step;
    step.from = from;
    step.to = backgammonr::kOffPosition;
    step.die = die;
    step.hit = false;
    return step;
  }

  if (overshoot_bear_off &&
      !has_checkers_farther_from_off_in_home_fast(board, player, from)) {
    backgammonr::MoveStep step;
    step.from = from;
    step.to = backgammonr::kOffPosition;
    step.die = die;
    step.hit = false;
    return step;
  }

  return std::nullopt;
}

}  // namespace

namespace backgammonr {

int player_index(const int player) {
  validate_player(player);
  return player_index_fast(player);
}

int opponent_index(const int player) {
  validate_player(player);
  return opponent_index_fast(player);
}

int player_checker_count_on_point(const BoardState& board, const int player, const int point) {
  validate_player(player);
  validate_point(point);
  return player_checker_count_on_point_fast(board, player, point);
}

int opponent_checker_count_on_point(const BoardState& board, const int player, const int point) {
  validate_player(player);
  validate_point(point);
  return opponent_checker_count_on_point_fast(board, player, point);
}

bool point_is_open_to_player(const BoardState& board, const int player, const int point) {
  validate_player(player);
  validate_point(point);
  return point_is_open_to_player_fast(board, player, point);
}

bool point_has_opponent_blot(const BoardState& board, const int player, const int point) {
  validate_player(player);
  validate_point(point);
  return point_has_opponent_blot_fast(board, player, point);
}

bool player_has_bar_checkers(const BoardState& board, const int player) {
  validate_player(player);
  return player_has_bar_checkers_fast(board, player);
}

bool is_home_point(const int player, const int point) {
  validate_player(player);
  validate_point(point);
  return is_home_point_fast(player, point);
}

bool all_checkers_in_home(const BoardState& board, const int player) {
  validate_player(player);
  return all_checkers_in_home_fast(board, player);
}

int bar_entry_point(const int player, const int die) {
  validate_player(player);

  if (die < kMinDieValue || die > kMaxDieValue) {
    throw std::range_error("`die` must be between 1 and 6.");
  }

  return bar_entry_point_fast(player, die);
}

bool has_checkers_farther_from_off_in_home(
    const BoardState& board,
    const int player,
    const int point) {
  validate_player(player);
  validate_point(point);
  return has_checkers_farther_from_off_in_home_fast(board, player, point);
}

std::optional<MoveStep> legal_step_from_source(
    const BoardState& board,
    const int player,
    const int from,
    const int die) {
  validate_player(player);
  return legal_step_from_source_fast(board, player, from, die);
}

void legal_steps_for_die_into(
    const BoardState& board,
    const int player,
    const int die,
    std::vector<MoveStep>& steps) {
  std::array<MoveStep, kNumPoints> stack_steps{};
  const int n_steps = legal_steps_for_die_into_array(board, player, die, stack_steps);
  steps.clear();
  steps.reserve(n_steps);
  for (int i = 0; i < n_steps; ++i) {
    steps.push_back(stack_steps[i]);
  }
}

int legal_steps_for_die_into_array(
    const BoardState& board,
    const int player,
    const int die,
    std::array<MoveStep, kNumPoints>& steps) {
  validate_player(player);
  int n_steps = 0;

  if (player_has_bar_checkers_fast(board, player)) {
    const std::optional<MoveStep> entry_step =
        legal_step_from_source_fast(board, player, kBarPosition, die);
    if (entry_step.has_value()) {
      steps[n_steps] = entry_step.value();
      n_steps += 1;
    }
    return n_steps;
  }

  if (player == 1) {
    for (int from = kNumPoints; from >= 1; --from) {
      const std::optional<MoveStep> step =
          legal_step_from_source_fast(board, player, from, die);
      if (step.has_value()) {
        steps[n_steps] = step.value();
        n_steps += 1;
      }
    }
    return n_steps;
  }

  for (int from = 1; from <= kNumPoints; ++from) {
    const std::optional<MoveStep> step =
        legal_step_from_source_fast(board, player, from, die);
    if (step.has_value()) {
      steps[n_steps] = step.value();
      n_steps += 1;
    }
  }

  return n_steps;
}

std::vector<MoveStep> legal_steps_for_die(const BoardState& board, const int player, const int die) {
  std::vector<MoveStep> steps;
  legal_steps_for_die_into(board, player, die, steps);
  return steps;
}

void apply_move_step_unchecked_inplace(
    BoardState& board,
    const int player,
    const MoveStep& step) {
  const int current_player_index = player_index_fast(player);
  const int other_player_index = opponent_index_fast(player);

  if (step.from == kBarPosition) {
    board.bar[current_player_index] -= 1;
  } else {
    board.points[step.from - 1] -= player;
  }

  if (step.to == kOffPosition) {
    board.off[current_player_index] += 1;
    return;
  }

  if (step.hit) {
    board.bar[other_player_index] += 1;
    board.points[step.to - 1] = player;
    return;
  }

  board.points[step.to - 1] += player;
}

void apply_move_step_inplace(
    BoardState& board,
    const int player,
    const MoveStep& step) {
  validate_player(player);

  const std::optional<MoveStep> legal_step =
      legal_step_from_source_fast(board, player, step.from, step.die);
  if (!legal_step.has_value()) {
    throw std::range_error("Attempted to apply an illegal move step.");
  }

  if (legal_step->to != step.to || legal_step->hit != step.hit) {
    throw std::range_error("Move step does not match the legal move implied by board state and die.");
  }

  apply_move_step_unchecked_inplace(board, player, step);
}

BoardState apply_move_step_unchecked(
    const BoardState& board,
    const int player,
    const MoveStep& step) {
  BoardState out = board;
  apply_move_step_unchecked_inplace(out, player, step);
  return out;
}

BoardState apply_move_step(const BoardState& board, const int player, const MoveStep& step) {
  BoardState out = board;
  apply_move_step_inplace(out, player, step);
  return out;
}

}  // namespace backgammonr
