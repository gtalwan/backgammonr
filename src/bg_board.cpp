#include "bg_board.h"

#include <cmath>
#include <sstream>
#include <stdexcept>

namespace {

bool has_named_element(const Rcpp::List& board, const char* name) {
  return board.containsElementNamed(name);
}

bool is_integer_vector_sexp(const SEXP x) {
  return TYPEOF(x) == INTSXP;
}

std::string collapse_messages(const std::vector<std::string>& messages) {
  std::ostringstream oss;

  for (std::size_t i = 0; i < messages.size(); ++i) {
    if (i > 0) {
      oss << "\n";
    }
    oss << messages[i];
  }

  return oss.str();
}

Rcpp::IntegerVector get_integer_vector_checked(
    const Rcpp::List& board,
    const char* name,
    const int expected_length,
    std::vector<std::string>& messages) {
  if (!has_named_element(board, name)) {
    messages.push_back(std::string("Missing required field `") + name + "`.");
    return Rcpp::IntegerVector();
  }

  SEXP raw = board[name];
  if (!is_integer_vector_sexp(raw)) {
    messages.push_back(std::string("`") + name + "` must be stored as an integer vector.");
    return Rcpp::IntegerVector();
  }

  Rcpp::IntegerVector values(raw);

  if (values.size() != expected_length) {
    std::ostringstream oss;
    oss << "`" << name << "` must have length " << expected_length << ".";
    messages.push_back(oss.str());
    return Rcpp::IntegerVector();
  }

  for (int i = 0; i < values.size(); ++i) {
    if (values[i] == NA_INTEGER) {
      messages.push_back(std::string("`") + name + "` cannot contain `NA` values.");
      return Rcpp::IntegerVector();
    }
  }

  return values;
}

int get_turn_checked(const Rcpp::List& board, std::vector<std::string>& messages) {
  if (!has_named_element(board, "turn")) {
    messages.push_back("Missing required field `turn`.");
    return 0;
  }

  SEXP raw = board["turn"];
  if (!is_integer_vector_sexp(raw)) {
    messages.push_back("`turn` must be stored as an integer scalar.");
    return 0;
  }

  Rcpp::IntegerVector turn(raw);
  if (turn.size() != 1) {
    messages.push_back("`turn` must have length 1.");
    return 0;
  }

  if (turn[0] == NA_INTEGER || (turn[0] != 1 && turn[0] != -1)) {
    messages.push_back("`turn` must be either 1L or -1L.");
    return 0;
  }

  return turn[0];
}

void check_nonnegative(
    const Rcpp::IntegerVector& values,
    const char* name,
    std::vector<std::string>& messages) {
  for (int i = 0; i < values.size(); ++i) {
    if (values[i] < 0) {
      messages.push_back(std::string("`") + name + "` must be nonnegative.");
      return;
    }
  }
}

void check_point_bounds(
    const Rcpp::IntegerVector& points,
    std::vector<std::string>& messages) {
  for (int i = 0; i < points.size(); ++i) {
    if (std::abs(points[i]) > backgammonr::kCheckersPerPlayer) {
      std::ostringstream oss;
      oss << "Absolute checker count at point " << (i + 1)
          << " cannot exceed " << backgammonr::kCheckersPerPlayer << ".";
      messages.push_back(oss.str());
      return;
    }
  }
}

void check_total_checkers(
    const Rcpp::IntegerVector& points,
    const Rcpp::IntegerVector& bar,
    const Rcpp::IntegerVector& off,
    std::vector<std::string>& messages) {
  int player1_total = bar[0] + off[0];
  int player2_total = bar[1] + off[1];

  for (int i = 0; i < points.size(); ++i) {
    if (points[i] > 0) {
      player1_total += points[i];
    } else if (points[i] < 0) {
      player2_total += -points[i];
    }
  }

  if (player1_total != backgammonr::kCheckersPerPlayer) {
    std::ostringstream oss;
    oss << "Player 1 must have exactly "
        << backgammonr::kCheckersPerPlayer
        << " total checkers; found " << player1_total << ".";
    messages.push_back(oss.str());
  }

  if (player2_total != backgammonr::kCheckersPerPlayer) {
    std::ostringstream oss;
    oss << "Player 2 must have exactly "
        << backgammonr::kCheckersPerPlayer
        << " total checkers; found " << player2_total << ".";
    messages.push_back(oss.str());
  }
}

}  // namespace

namespace backgammonr {

BoardState initial_board_state(const int turn) {
  BoardState board;
  board.turn = turn;

  // Standard opening position in absolute 1:24 coordinates.
  // Player 1: +2 on 24, +5 on 13, +3 on 8, +5 on 6.
  // Player 2: -2 on 1, -5 on 12, -3 on 17, -5 on 19.
  board.points[23] =  2;
  board.points[12] =  5;
  board.points[7]  =  3;
  board.points[5]  =  5;

  board.points[0]  = -2;
  board.points[11] = -5;
  board.points[16] = -3;
  board.points[18] = -5;

  return board;
}

Rcpp::List board_to_list(const BoardState& board) {
  Rcpp::IntegerVector points(kNumPoints);
  Rcpp::IntegerVector bar(kNumPlayers);
  Rcpp::IntegerVector off(kNumPlayers);
  Rcpp::IntegerVector turn(1);

  for (int i = 0; i < kNumPoints; ++i) {
    points[i] = board.points[i];
  }

  for (int i = 0; i < kNumPlayers; ++i) {
    bar[i] = board.bar[i];
    off[i] = board.off[i];
  }

  turn[0] = board.turn;

  return Rcpp::List::create(
    Rcpp::_["points"] = points,
    Rcpp::_["bar"] = bar,
    Rcpp::_["off"] = off,
    Rcpp::_["turn"] = turn
  );
}

std::vector<std::string> validate_board_list(const Rcpp::List& board) {
  std::vector<std::string> messages;

  const Rcpp::IntegerVector points = get_integer_vector_checked(board, "points", kNumPoints, messages);
  const Rcpp::IntegerVector bar = get_integer_vector_checked(board, "bar", kNumPlayers, messages);
  const Rcpp::IntegerVector off = get_integer_vector_checked(board, "off", kNumPlayers, messages);
  const int turn = get_turn_checked(board, messages);

  if (!messages.empty()) {
    return messages;
  }

  check_nonnegative(bar, "bar", messages);
  check_nonnegative(off, "off", messages);
  check_point_bounds(points, messages);
  check_total_checkers(points, bar, off, messages);

  if (off[0] == kCheckersPerPlayer && off[1] == kCheckersPerPlayer) {
    messages.push_back("Both players cannot simultaneously have all 15 checkers borne off.");
  }

  if (turn != 1 && turn != -1) {
    messages.push_back("`turn` must be either 1L or -1L.");
  }

  return messages;
}

BoardState parse_board_list(const Rcpp::List& board) {
  const std::vector<std::string> messages = validate_board_list(board);
  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  BoardState out;
  const Rcpp::IntegerVector points = board["points"];
  const Rcpp::IntegerVector bar = board["bar"];
  const Rcpp::IntegerVector off = board["off"];
  const Rcpp::IntegerVector turn = board["turn"];

  for (int i = 0; i < kNumPoints; ++i) {
    out.points[i] = points[i];
  }

  for (int i = 0; i < kNumPlayers; ++i) {
    out.bar[i] = bar[i];
    out.off[i] = off[i];
  }

  out.turn = turn[0];
  return out;
}

Rcpp::List clone_board_list(const Rcpp::List& board) {
  const BoardState parsed = parse_board_list(board);
  Rcpp::List out = Rcpp::clone(board);
  const Rcpp::List normalized = board_to_list(parsed);

  out["points"] = normalized["points"];
  out["bar"] = normalized["bar"];
  out["off"] = normalized["off"];
  out["turn"] = normalized["turn"];

  return out;
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::List bg_cpp_board_initial(const int turn = 1) {
  if (turn != 1 && turn != -1) {
    throw std::range_error("`turn` must be either 1L or -1L.");
  }

  return backgammonr::board_to_list(backgammonr::initial_board_state(turn));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_board_clone(const Rcpp::List& board) {
  return backgammonr::clone_board_list(board);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_board_validate(const Rcpp::List& board) {
  const std::vector<std::string> messages = backgammonr::validate_board_list(board);

  return Rcpp::List::create(
    Rcpp::_["valid"] = messages.empty(),
    Rcpp::_["messages"] = Rcpp::wrap(messages)
  );
}
