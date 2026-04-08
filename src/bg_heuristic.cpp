// Heuristic board scoring, feature extraction, and move-choice kernels.
#include "bg_heuristic.h"

#include <limits>
#include <sstream>
#include <stdexcept>

#include "bg_game.h"
#include "bg_rules.h"

namespace {

void validate_player(const int player) {
  if (player != 1 && player != -1) {
    throw std::range_error("`player` must be either 1L or -1L.");
  }
}

void validate_heuristic_selection(const std::string& selection) {
  if (selection != "aggressive" && selection != "defensive") {
    throw std::range_error("`selection` must be either \"aggressive\" or \"defensive\".");
  }
}

int pip_distance_to_off(const int player, const int point) {
  validate_player(player);

  if (point < 1 || point > backgammonr::kNumPoints) {
    throw std::range_error("`point` must be between 1 and 24.");
  }

  if (player == 1) {
    return point;
  }

  return backgammonr::kOffPosition - point;
}

bool point_has_player_contact(const backgammonr::BoardState& board, const int player, const int point) {
  validate_player(player);

  if (player == 1) {
    for (int opponent_point = 1; opponent_point < point; ++opponent_point) {
      if (backgammonr::opponent_checker_count_on_point(board, player, opponent_point) > 0) {
        return true;
      }
    }
    return false;
  }

  for (int opponent_point = point + 1; opponent_point <= backgammonr::kNumPoints; ++opponent_point) {
    if (backgammonr::opponent_checker_count_on_point(board, player, opponent_point) > 0) {
      return true;
    }
  }

  return false;
}

bool blot_is_directly_hittable_by_die(
    const backgammonr::BoardState& board,
    const int attacker,
    const int target_point,
    const int die) {
  validate_player(attacker);

  const int defender = -attacker;
  if (backgammonr::player_checker_count_on_point(board, defender, target_point) != 1) {
    return false;
  }

  if (backgammonr::player_has_bar_checkers(board, attacker)) {
    return backgammonr::bar_entry_point(attacker, die) == target_point;
  }

  const int source_point = attacker == 1 ? target_point + die : target_point - die;
  if (source_point < 1 || source_point > backgammonr::kNumPoints) {
    return false;
  }

  return backgammonr::player_checker_count_on_point(board, attacker, source_point) > 0;
}

int count_direct_shot_dice_against_blots(const backgammonr::BoardState& board, const int attacker) {
  validate_player(attacker);

  int count = 0;
  const int defender = -attacker;

  for (int target_point = 1; target_point <= backgammonr::kNumPoints; ++target_point) {
    if (backgammonr::player_checker_count_on_point(board, defender, target_point) != 1) {
      continue;
    }

    for (int die = backgammonr::kMinDieValue; die <= backgammonr::kMaxDieValue; ++die) {
      if (blot_is_directly_hittable_by_die(board, attacker, target_point, die)) {
        ++count;
      }
    }
  }

  return count;
}

backgammonr::BoardState apply_sequence_without_full_validation(
    const backgammonr::BoardState& board,
    const backgammonr::MoveSequence& sequence) {
  backgammonr::BoardState out = board;
  for (const backgammonr::MoveStep& step : sequence.steps) {
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  }
  out.turn = -sequence.player;
  return out;
}

}  // namespace

namespace backgammonr {

BoardFeatures extract_board_features(const BoardState& board, const int player) {
  validate_player(player);

  BoardFeatures features;
  const int own_index = player_index(player);
  const int opponent_index_value = opponent_index(player);

  features.own_bar = board.bar[own_index];
  features.opponent_bar = board.bar[opponent_index_value];
  features.own_off = board.off[own_index];
  features.opponent_off = board.off[opponent_index_value];
  features.own_direct_hit_opportunities = count_direct_shot_dice_against_blots(board, player);
  features.own_direct_hit_risk = count_direct_shot_dice_against_blots(board, -player);
  features.own_pip_count = features.own_bar * 25;

  for (int point = 1; point <= kNumPoints; ++point) {
    const int own_count = player_checker_count_on_point(board, player, point);
    const int opp_count = opponent_checker_count_on_point(board, player, point);

    if (own_count == 1) {
      ++features.own_blots;
    }

    if (opp_count == 1) {
      ++features.opponent_blots;
    }

    if (own_count >= 2) {
      ++features.own_made_points;
      if (is_home_point(player, point)) {
        ++features.own_home_made_points;
      }
    }

    if (own_count > 0 && point_has_player_contact(board, player, point)) {
      features.own_contact_checkers += own_count;
    }

    features.own_pip_count += own_count * pip_distance_to_off(player, point);
  }

  return features;
}

double aggressive_board_score(const BoardState& board, const int player) {
  const BoardFeatures f = extract_board_features(board, player);

  return
      120.0 * static_cast<double>(f.opponent_bar) +
      12.0 * static_cast<double>(f.own_direct_hit_opportunities) +
      8.0 * static_cast<double>(f.own_made_points) +
      3.0 * static_cast<double>(f.own_home_made_points) +
      2.0 * static_cast<double>(f.own_contact_checkers) +
      5.0 * static_cast<double>(f.own_off) -
      15.0 * static_cast<double>(f.own_bar) -
      3.0 * static_cast<double>(f.own_blots) -
      1.5 * static_cast<double>(f.own_direct_hit_risk) -
      0.05 * static_cast<double>(f.own_pip_count);
}

double defensive_board_score(const BoardState& board, const int player) {
  const BoardFeatures f = extract_board_features(board, player);

  return
      10.0 * static_cast<double>(f.own_made_points) +
      6.0 * static_cast<double>(f.own_home_made_points) +
      4.0 * static_cast<double>(f.own_off) +
      2.0 * static_cast<double>(f.opponent_bar) -
      20.0 * static_cast<double>(f.own_bar) -
      15.0 * static_cast<double>(f.own_blots) -
      20.0 * static_cast<double>(f.own_direct_hit_risk) -
      3.0 * static_cast<double>(f.own_contact_checkers) -
      0.10 * static_cast<double>(f.own_pip_count);
}

double heuristic_board_score(const BoardState& board, const int player, const std::string& selection) {
  validate_heuristic_selection(selection);

  if (selection == "aggressive") {
    return aggressive_board_score(board, player);
  }

  return defensive_board_score(board, player);
}

MoveSequence choose_best_heuristic_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& selection) {
  validate_heuristic_selection(selection);

  if (legal_moves.empty()) {
    throw std::range_error("Cannot choose a move from an empty legal-move set.");
  }

  double best_score = -std::numeric_limits<double>::infinity();
  int best_index = -1;

  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    const MoveSequence& candidate = legal_moves[i];
    const BoardState board_after = apply_sequence_without_full_validation(board, candidate);
    const double score = heuristic_board_score(board_after, candidate.player, selection);

    if (best_index < 0 || score > best_score) {
      best_score = score;
      best_index = i;
    }
  }

  return legal_moves[best_index];
}

Rcpp::List board_features_to_list(const BoardFeatures& features) {
  return Rcpp::List::create(
    Rcpp::_["own_bar"] = Rcpp::IntegerVector::create(features.own_bar),
    Rcpp::_["opponent_bar"] = Rcpp::IntegerVector::create(features.opponent_bar),
    Rcpp::_["own_off"] = Rcpp::IntegerVector::create(features.own_off),
    Rcpp::_["opponent_off"] = Rcpp::IntegerVector::create(features.opponent_off),
    Rcpp::_["own_blots"] = Rcpp::IntegerVector::create(features.own_blots),
    Rcpp::_["opponent_blots"] = Rcpp::IntegerVector::create(features.opponent_blots),
    Rcpp::_["own_made_points"] = Rcpp::IntegerVector::create(features.own_made_points),
    Rcpp::_["own_home_made_points"] = Rcpp::IntegerVector::create(features.own_home_made_points),
    Rcpp::_["own_direct_hit_opportunities"] = Rcpp::IntegerVector::create(features.own_direct_hit_opportunities),
    Rcpp::_["own_direct_hit_risk"] = Rcpp::IntegerVector::create(features.own_direct_hit_risk),
    Rcpp::_["own_contact_checkers"] = Rcpp::IntegerVector::create(features.own_contact_checkers),
    Rcpp::_["own_pip_count"] = Rcpp::IntegerVector::create(features.own_pip_count)
  );
}

}  // namespace backgammonr

// [[Rcpp::export]]
double bg_cpp_heuristic_board_score(
    const Rcpp::List& board,
    const int player,
    const std::string& selection) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  return backgammonr::heuristic_board_score(parsed_board, player, selection);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_heuristic_board_features(
    const Rcpp::List& board,
    const int player) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  return backgammonr::board_features_to_list(backgammonr::extract_board_features(parsed_board, player));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_heuristic_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& selection) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves = backgammonr::parse_move_sequence_vector(legal_moves);
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_best_heuristic_move_sequence(parsed_board, parsed_moves, selection));
}
