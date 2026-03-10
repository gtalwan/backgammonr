// LINE NOTE: #include "bg_simulation.h"
#include "bg_simulation.h"
// LINE NOTE: // This translation unit implements the multi-game simulation layer that
// This translation unit implements the multi-game simulation layer that
// LINE NOTE: // aggregates per-game stochastic outcomes into study-ready summary tables.
// aggregates per-game stochastic outcomes into study-ready summary tables.

// LINE NOTE: #include <algorithm>
#include <algorithm>
// LINE NOTE: #include <cstdint>
#include <cstdint>
// LINE NOTE: #include <random>
#include <random>
// LINE NOTE: #include <sstream>
#include <sstream>
// LINE NOTE: #include <stdexcept>
#include <stdexcept>
// LINE NOTE: #include <string>
#include <string>
// LINE NOTE: #include <vector>
#include <vector>

// LINE NOTE: #include "bg_game.h"
#include "bg_game.h"
// LINE NOTE: #include "bg_movegen.h"
#include "bg_movegen.h"
// LINE NOTE: #include "bg_rules.h"
#include "bg_rules.h"

// LINE NOTE: namespace {
namespace {

// LINE NOTE: // Reuse the engine's randomness predicate so scripted simulation can decide
// Reuse the engine's randomness predicate so scripted simulation can decide
// LINE NOTE: // whether it needs an RNG at all.
// whether it needs an RNG at all.
// LINE NOTE: bool selection_uses_randomness(const std::string& selection) {
bool selection_uses_randomness(const std::string& selection) {
  // LINE NOTE: return backgammonr::selection_uses_randomness(selection);
  return backgammonr::selection_uses_randomness(selection);
// LINE NOTE: }
}

// LINE NOTE: // Validate number of games argument.
// Validate number of games argument.
// LINE NOTE: void validate_n_games(const int n_games) {
void validate_n_games(const int n_games) {
  // LINE NOTE: if (n_games < 1) {
  if (n_games < 1) {
    // LINE NOTE: throw std::range_error("`n_games` must be at least 1.");
    throw std::range_error("`n_games` must be at least 1.");
  // LINE NOTE: }
  }
// LINE NOTE: }
}

// LINE NOTE: // Validate per-game turn cap.
// Validate per-game turn cap.
// LINE NOTE: void validate_max_turns(const int max_turns) {
void validate_max_turns(const int max_turns) {
  // LINE NOTE: if (max_turns < 0) {
  if (max_turns < 0) {
    // LINE NOTE: throw std::range_error("`max_turns` must be nonnegative.");
    throw std::range_error("`max_turns` must be nonnegative.");
  // LINE NOTE: }
  }
// LINE NOTE: }
}

// LINE NOTE: // Build deterministic or nondeterministic RNG depending on user arguments.
// Build deterministic or nondeterministic RNG depending on user arguments.
// LINE NOTE: std::mt19937 init_rng(const int seed, const bool use_seed) {
std::mt19937 init_rng(const int seed, const bool use_seed) {
  // LINE NOTE: std::mt19937 rng;
  std::mt19937 rng;

  // LINE NOTE: if (use_seed) {
  if (use_seed) {
    // LINE NOTE: if (seed < 0) {
    if (seed < 0) {
      // LINE NOTE: throw std::range_error("`seed` must be nonnegative when supplied.");
      throw std::range_error("`seed` must be nonnegative when supplied.");
    // LINE NOTE: }
    }
    // LINE NOTE: rng.seed(static_cast<std::uint32_t>(seed));
    rng.seed(static_cast<std::uint32_t>(seed));
  // LINE NOTE: } else {
  } else {
    // LINE NOTE: std::random_device rd;
    std::random_device rd;
    // LINE NOTE: rng.seed(rd());
    rng.seed(rd());
  // LINE NOTE: }
  }

  // LINE NOTE: return rng;
  return rng;
// LINE NOTE: }
}

// LINE NOTE: // Parse list of scripted rolls from R into engine roll objects.
// Parse list of scripted rolls from R into engine roll objects.
// LINE NOTE: std::vector<backgammonr::DiceRoll> parse_roll_vector(const Rcpp::List& rolls) {
std::vector<backgammonr::DiceRoll> parse_roll_vector(const Rcpp::List& rolls) {
  // LINE NOTE: std::vector<backgammonr::DiceRoll> out;
  std::vector<backgammonr::DiceRoll> out;
  // LINE NOTE: out.reserve(rolls.size());
  out.reserve(rolls.size());

  // LINE NOTE: for (int i = 0; i < rolls.size(); ++i) {
  for (int i = 0; i < rolls.size(); ++i) {
    // LINE NOTE: SEXP roll_sexp = rolls[i];
    SEXP roll_sexp = rolls[i];
    // LINE NOTE: if (!Rf_isNewList(roll_sexp)) {
    if (!Rf_isNewList(roll_sexp)) {
      // LINE NOTE: std::ostringstream oss;
      std::ostringstream oss;
      // LINE NOTE: oss << "`rolls[[" << (i + 1) << "]]` must be a roll-like list.";
      oss << "`rolls[[" << (i + 1) << "]]` must be a roll-like list.";
      // LINE NOTE: throw std::range_error(oss.str());
      throw std::range_error(oss.str());
    // LINE NOTE: }
    }

    // LINE NOTE: out.push_back(backgammonr::parse_roll_list(Rcpp::List(roll_sexp)));
    out.push_back(backgammonr::parse_roll_list(Rcpp::List(roll_sexp)));
  // LINE NOTE: }
  }

  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Pick active selection rule based on whose turn it is.
// Pick active selection rule based on whose turn it is.
// LINE NOTE: const std::string& selection_for_player_unchecked(
const std::string& selection_for_player_unchecked(
    // LINE NOTE: const int player,
    const int player,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection) {
    const std::string& player2_selection) {
  // LINE NOTE: return player == 1 ? player1_selection : player2_selection;
  return player == 1 ? player1_selection : player2_selection;
// LINE NOTE: }
}

// LINE NOTE: // Convert winner code to stable user-facing label.
// Convert winner code to stable user-facing label.
// LINE NOTE: std::string winner_label(const int winner) {
std::string winner_label(const int winner) {
  // LINE NOTE: if (winner == 1) {
  if (winner == 1) {
    // LINE NOTE: return "player_1";
    return "player_1";
  // LINE NOTE: }
  }

  // LINE NOTE: if (winner == -1) {
  if (winner == -1) {
    // LINE NOTE: return "player_2";
    return "player_2";
  // LINE NOTE: }
  }

  // LINE NOTE: return "none";
  return "none";
// LINE NOTE: }
}

// LINE NOTE: // Fast path for pure random-policy turns (hot loop in simulations).
// Fast path for pure random-policy turns (hot loop in simulations).
// LINE NOTE: void play_random_turn_with_roll_lightweight(
void play_random_turn_with_roll_lightweight(
    // LINE NOTE: backgammonr::BoardState& board,
    backgammonr::BoardState& board,
    // LINE NOTE: const backgammonr::DiceRoll& roll,
    const backgammonr::DiceRoll& roll,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
// LINE NOTE: }
}

// LINE NOTE: // Simulate one game with random dice.
// Simulate one game with random dice.
// LINE NOTE: backgammonr::SimulatedGameSummary simulate_one_game_random(
backgammonr::SimulatedGameSummary simulate_one_game_random(
    // LINE NOTE: const backgammonr::BoardState& initial_board,
    const backgammonr::BoardState& initial_board,
    // LINE NOTE: const int game_id,
    const int game_id,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: std::mt19937& rng,
    std::mt19937& rng,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const backgammonr::RolloutConfig& rollout_config) {
    const backgammonr::RolloutConfig& rollout_config) {
  // LINE NOTE: backgammonr::SimulatedGameSummary out;
  backgammonr::SimulatedGameSummary out;
  // LINE NOTE: out.game_id = game_id;
  out.game_id = game_id;

  // LINE NOTE: // Early exit if initial board is already terminal.
  // Early exit if initial board is already terminal.
  // LINE NOTE: if (backgammonr::board_is_terminal(initial_board)) {
  if (backgammonr::board_is_terminal(initial_board)) {
    // LINE NOTE: out.game_over = true;
    out.game_over = true;
    // LINE NOTE: out.winner = backgammonr::board_winner(initial_board);
    out.winner = backgammonr::board_winner(initial_board);
    // LINE NOTE: return out;
    return out;
  // LINE NOTE: }
  }

  // LINE NOTE: backgammonr::BoardState current = initial_board;
  backgammonr::BoardState current = initial_board;
  // LINE NOTE: for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
  for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
    // LINE NOTE: // Resolve selection policy for current player.
    // Resolve selection policy for current player.
    // LINE NOTE: const std::string& turn_selection =
    const std::string& turn_selection =
        // LINE NOTE: selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    // LINE NOTE: if (turn_selection == "random") {
    if (turn_selection == "random") {
      // LINE NOTE: // Keep random-policy simulation lightweight.
      // Keep random-policy simulation lightweight.
      // LINE NOTE: play_random_turn_with_roll_lightweight(current, backgammonr::roll_dice(rng), rng);
      play_random_turn_with_roll_lightweight(current, backgammonr::roll_dice(rng), rng);
    // LINE NOTE: } else {
    } else {
      // LINE NOTE: // Delegate non-random policies to the general turn engine.
      // Delegate non-random policies to the general turn engine.
      // LINE NOTE: const backgammonr::TurnResult turn_result =
      const backgammonr::TurnResult turn_result =
          // LINE NOTE: backgammonr::play_turn_random(current, rng, turn_selection, rollout_config);
          backgammonr::play_turn_random(current, rng, turn_selection, rollout_config);
      // LINE NOTE: current = turn_result.board_after;
      current = turn_result.board_after;
      // LINE NOTE: if (turn_result.game_over) {
      if (turn_result.game_over) {
        // LINE NOTE: out.game_over = true;
        out.game_over = true;
        // LINE NOTE: out.winner = turn_result.winner;
        out.winner = turn_result.winner;
        // LINE NOTE: out.n_turns += 1;
        out.n_turns += 1;
        // LINE NOTE: break;
        break;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
    // LINE NOTE: out.n_turns += 1;
    out.n_turns += 1;

    // LINE NOTE: // Random turn path can finish game without TurnResult object.
    // Random turn path can finish game without TurnResult object.
    // LINE NOTE: if (backgammonr::board_is_terminal(current)) {
    if (backgammonr::board_is_terminal(current)) {
      // LINE NOTE: out.game_over = true;
      out.game_over = true;
      // LINE NOTE: out.winner = backgammonr::board_winner(current);
      out.winner = backgammonr::board_winner(current);
      // LINE NOTE: break;
      break;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: out.turn_limit_reached = !out.game_over && out.n_turns == max_turns;
  out.turn_limit_reached = !out.game_over && out.n_turns == max_turns;
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Simulate one game with scripted roll sequence.
// Simulate one game with scripted roll sequence.
// LINE NOTE: backgammonr::SimulatedGameSummary simulate_one_game_with_rolls(
backgammonr::SimulatedGameSummary simulate_one_game_with_rolls(
    // LINE NOTE: const backgammonr::BoardState& initial_board,
    const backgammonr::BoardState& initial_board,
    // LINE NOTE: const std::vector<backgammonr::DiceRoll>& rolls,
    const std::vector<backgammonr::DiceRoll>& rolls,
    // LINE NOTE: const int game_id,
    const int game_id,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: std::mt19937* rng,
    std::mt19937* rng,
    // LINE NOTE: const backgammonr::RolloutConfig& rollout_config) {
    const backgammonr::RolloutConfig& rollout_config) {
  // LINE NOTE: backgammonr::SimulatedGameSummary out;
  backgammonr::SimulatedGameSummary out;
  // LINE NOTE: out.game_id = game_id;
  out.game_id = game_id;

  // LINE NOTE: if (backgammonr::board_is_terminal(initial_board)) {
  if (backgammonr::board_is_terminal(initial_board)) {
    // LINE NOTE: out.game_over = true;
    out.game_over = true;
    // LINE NOTE: out.winner = backgammonr::board_winner(initial_board);
    out.winner = backgammonr::board_winner(initial_board);
    // LINE NOTE: return out;
    return out;
  // LINE NOTE: }
  }

  // LINE NOTE: backgammonr::BoardState current = initial_board;
  backgammonr::BoardState current = initial_board;
  // LINE NOTE: for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
  for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
    // LINE NOTE: // Script exhausted before turn cap.
    // Script exhausted before turn cap.
    // LINE NOTE: if (turn_index >= static_cast<int>(rolls.size())) {
    if (turn_index >= static_cast<int>(rolls.size())) {
      // LINE NOTE: out.roll_sequence_exhausted = true;
      out.roll_sequence_exhausted = true;
      // LINE NOTE: break;
      break;
    // LINE NOTE: }
    }

    // LINE NOTE: const std::string& turn_selection =
    const std::string& turn_selection =
        // LINE NOTE: selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    // LINE NOTE: if (turn_selection == "random") {
    if (turn_selection == "random") {
      // LINE NOTE: if (rng == nullptr) {
      if (rng == nullptr) {
        // LINE NOTE: throw std::range_error("Random selection with scripted rolls requires an RNG.");
        throw std::range_error("Random selection with scripted rolls requires an RNG.");
      // LINE NOTE: }
      }
      // LINE NOTE: // Random-policy fast path with supplied roll.
      // Random-policy fast path with supplied roll.
      // LINE NOTE: play_random_turn_with_roll_lightweight(current, rolls[turn_index], *rng);
      play_random_turn_with_roll_lightweight(current, rolls[turn_index], *rng);
    // LINE NOTE: } else {
    } else {
      // LINE NOTE: // Non-random policies still use scripted roll.
      // Non-random policies still use scripted roll.
      // LINE NOTE: const backgammonr::TurnResult turn_result =
      const backgammonr::TurnResult turn_result =
          // LINE NOTE: backgammonr::play_turn_with_roll(current, rolls[turn_index], turn_selection, rng, rollout_config);
          backgammonr::play_turn_with_roll(current, rolls[turn_index], turn_selection, rng, rollout_config);
      // LINE NOTE: current = turn_result.board_after;
      current = turn_result.board_after;
      // LINE NOTE: if (turn_result.game_over) {
      if (turn_result.game_over) {
        // LINE NOTE: out.game_over = true;
        out.game_over = true;
        // LINE NOTE: out.winner = turn_result.winner;
        out.winner = turn_result.winner;
        // LINE NOTE: out.n_turns += 1;
        out.n_turns += 1;
        // LINE NOTE: break;
        break;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }
    // LINE NOTE: out.n_turns += 1;
    out.n_turns += 1;

    // LINE NOTE: if (backgammonr::board_is_terminal(current)) {
    if (backgammonr::board_is_terminal(current)) {
      // LINE NOTE: out.game_over = true;
      out.game_over = true;
      // LINE NOTE: out.winner = backgammonr::board_winner(current);
      out.winner = backgammonr::board_winner(current);
      // LINE NOTE: break;
      break;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: out.turn_limit_reached = !out.game_over && !out.roll_sequence_exhausted && out.n_turns == max_turns;
  out.turn_limit_reached = !out.game_over && !out.roll_sequence_exhausted && out.n_turns == max_turns;
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Convert per-game simulation records into rectangular R table.
// Convert per-game simulation records into rectangular R table.
// LINE NOTE: Rcpp::DataFrame games_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
Rcpp::DataFrame games_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
  // LINE NOTE: const int n = static_cast<int>(result.games.size());
  const int n = static_cast<int>(result.games.size());
  // LINE NOTE: Rcpp::IntegerVector game_id(n);
  Rcpp::IntegerVector game_id(n);
  // LINE NOTE: Rcpp::IntegerVector winner(n);
  Rcpp::IntegerVector winner(n);
  // LINE NOTE: Rcpp::CharacterVector winner_label_vec(n);
  Rcpp::CharacterVector winner_label_vec(n);
  // LINE NOTE: Rcpp::IntegerVector n_turns(n);
  Rcpp::IntegerVector n_turns(n);
  // LINE NOTE: Rcpp::LogicalVector game_over(n);
  Rcpp::LogicalVector game_over(n);
  // LINE NOTE: Rcpp::LogicalVector turn_limit_reached(n);
  Rcpp::LogicalVector turn_limit_reached(n);
  // LINE NOTE: Rcpp::LogicalVector roll_sequence_exhausted(n);
  Rcpp::LogicalVector roll_sequence_exhausted(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: const backgammonr::SimulatedGameSummary& game = result.games[i];
    const backgammonr::SimulatedGameSummary& game = result.games[i];
    // LINE NOTE: game_id[i] = game.game_id;
    game_id[i] = game.game_id;
    // LINE NOTE: winner[i] = game.winner;
    winner[i] = game.winner;
    // LINE NOTE: winner_label_vec[i] = winner_label(game.winner);
    winner_label_vec[i] = winner_label(game.winner);
    // LINE NOTE: n_turns[i] = game.n_turns;
    n_turns[i] = game.n_turns;
    // LINE NOTE: game_over[i] = game.game_over;
    game_over[i] = game.game_over;
    // LINE NOTE: turn_limit_reached[i] = game.turn_limit_reached;
    turn_limit_reached[i] = game.turn_limit_reached;
    // LINE NOTE: roll_sequence_exhausted[i] = game.roll_sequence_exhausted;
    roll_sequence_exhausted[i] = game.roll_sequence_exhausted;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["game_id"] = game_id,
      Rcpp::_["game_id"] = game_id,
      // LINE NOTE: Rcpp::_["winner"] = winner,
      Rcpp::_["winner"] = winner,
      // LINE NOTE: Rcpp::_["winner_label"] = winner_label_vec,
      Rcpp::_["winner_label"] = winner_label_vec,
      // LINE NOTE: Rcpp::_["n_turns"] = n_turns,
      Rcpp::_["n_turns"] = n_turns,
      // LINE NOTE: Rcpp::_["game_over"] = game_over,
      Rcpp::_["game_over"] = game_over,
      // LINE NOTE: Rcpp::_["turn_limit_reached"] = turn_limit_reached,
      Rcpp::_["turn_limit_reached"] = turn_limit_reached,
      // LINE NOTE: Rcpp::_["roll_sequence_exhausted"] = roll_sequence_exhausted,
      Rcpp::_["roll_sequence_exhausted"] = roll_sequence_exhausted,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
}

// LINE NOTE: // Aggregate matchup-level summary statistics.
// Aggregate matchup-level summary statistics.
// LINE NOTE: Rcpp::DataFrame summary_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
Rcpp::DataFrame summary_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
  // LINE NOTE: int completed_games = 0;
  int completed_games = 0;
  // LINE NOTE: int unresolved_games = 0;
  int unresolved_games = 0;
  // LINE NOTE: int player1_wins = 0;
  int player1_wins = 0;
  // LINE NOTE: int player2_wins = 0;
  int player2_wins = 0;
  // LINE NOTE: int turn_limit_reached_games = 0;
  int turn_limit_reached_games = 0;
  // LINE NOTE: int roll_sequence_exhausted_games = 0;
  int roll_sequence_exhausted_games = 0;
  // LINE NOTE: int min_turns = 0;
  int min_turns = 0;
  // LINE NOTE: int max_turns = 0;
  int max_turns = 0;
  // LINE NOTE: double mean_turns = NA_REAL;
  double mean_turns = NA_REAL;

  // LINE NOTE: if (!result.games.empty()) {
  if (!result.games.empty()) {
    // LINE NOTE: min_turns = result.games.front().n_turns;
    min_turns = result.games.front().n_turns;
    // LINE NOTE: max_turns = result.games.front().n_turns;
    max_turns = result.games.front().n_turns;
    // LINE NOTE: long long turn_sum = 0;
    long long turn_sum = 0;

    // LINE NOTE: for (const backgammonr::SimulatedGameSummary& game : result.games) {
    for (const backgammonr::SimulatedGameSummary& game : result.games) {
      // LINE NOTE: turn_sum += static_cast<long long>(game.n_turns);
      turn_sum += static_cast<long long>(game.n_turns);
      // LINE NOTE: min_turns = std::min(min_turns, game.n_turns);
      min_turns = std::min(min_turns, game.n_turns);
      // LINE NOTE: max_turns = std::max(max_turns, game.n_turns);
      max_turns = std::max(max_turns, game.n_turns);

      // LINE NOTE: if (game.winner == 1) {
      if (game.winner == 1) {
        // LINE NOTE: ++player1_wins;
        ++player1_wins;
      // LINE NOTE: } else if (game.winner == -1) {
      } else if (game.winner == -1) {
        // LINE NOTE: ++player2_wins;
        ++player2_wins;
      // LINE NOTE: } else {
      } else {
        // LINE NOTE: ++unresolved_games;
        ++unresolved_games;
      // LINE NOTE: }
      }

      // LINE NOTE: if (game.game_over) {
      if (game.game_over) {
        // LINE NOTE: ++completed_games;
        ++completed_games;
      // LINE NOTE: }
      }
      // LINE NOTE: if (game.turn_limit_reached) {
      if (game.turn_limit_reached) {
        // LINE NOTE: ++turn_limit_reached_games;
        ++turn_limit_reached_games;
      // LINE NOTE: }
      }
      // LINE NOTE: if (game.roll_sequence_exhausted) {
      if (game.roll_sequence_exhausted) {
        // LINE NOTE: ++roll_sequence_exhausted_games;
        ++roll_sequence_exhausted_games;
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: mean_turns = static_cast<double>(turn_sum) / static_cast<double>(result.games.size());
    mean_turns = static_cast<double>(turn_sum) / static_cast<double>(result.games.size());
  // LINE NOTE: }
  }

  // LINE NOTE: const double player1_win_rate = completed_games > 0
  const double player1_win_rate = completed_games > 0
      // LINE NOTE: ? static_cast<double>(player1_wins) / static_cast<double>(completed_games)
      ? static_cast<double>(player1_wins) / static_cast<double>(completed_games)
      // LINE NOTE: : NA_REAL;
      : NA_REAL;
  // LINE NOTE: const double player2_win_rate = completed_games > 0
  const double player2_win_rate = completed_games > 0
      // LINE NOTE: ? static_cast<double>(player2_wins) / static_cast<double>(completed_games)
      ? static_cast<double>(player2_wins) / static_cast<double>(completed_games)
      // LINE NOTE: : NA_REAL;
      : NA_REAL;

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
      Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
      // LINE NOTE: Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
      Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
      // LINE NOTE: Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
      Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
      // LINE NOTE: Rcpp::_["completed_games"] = Rcpp::IntegerVector::create(completed_games),
      Rcpp::_["completed_games"] = Rcpp::IntegerVector::create(completed_games),
      // LINE NOTE: Rcpp::_["unresolved_games"] = Rcpp::IntegerVector::create(unresolved_games),
      Rcpp::_["unresolved_games"] = Rcpp::IntegerVector::create(unresolved_games),
      // LINE NOTE: Rcpp::_["player1_wins"] = Rcpp::IntegerVector::create(player1_wins),
      Rcpp::_["player1_wins"] = Rcpp::IntegerVector::create(player1_wins),
      // LINE NOTE: Rcpp::_["player2_wins"] = Rcpp::IntegerVector::create(player2_wins),
      Rcpp::_["player2_wins"] = Rcpp::IntegerVector::create(player2_wins),
      // LINE NOTE: Rcpp::_["player1_win_rate"] = Rcpp::NumericVector::create(player1_win_rate),
      Rcpp::_["player1_win_rate"] = Rcpp::NumericVector::create(player1_win_rate),
      // LINE NOTE: Rcpp::_["player2_win_rate"] = Rcpp::NumericVector::create(player2_win_rate),
      Rcpp::_["player2_win_rate"] = Rcpp::NumericVector::create(player2_win_rate),
      // LINE NOTE: Rcpp::_["mean_turns"] = Rcpp::NumericVector::create(mean_turns),
      Rcpp::_["mean_turns"] = Rcpp::NumericVector::create(mean_turns),
      // LINE NOTE: Rcpp::_["min_turns"] = Rcpp::IntegerVector::create(min_turns),
      Rcpp::_["min_turns"] = Rcpp::IntegerVector::create(min_turns),
      // LINE NOTE: Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(max_turns),
      Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(max_turns),
      // LINE NOTE: Rcpp::_["turn_limit_reached_games"] = Rcpp::IntegerVector::create(turn_limit_reached_games),
      Rcpp::_["turn_limit_reached_games"] = Rcpp::IntegerVector::create(turn_limit_reached_games),
      // LINE NOTE: Rcpp::_["roll_sequence_exhausted_games"] = Rcpp::IntegerVector::create(roll_sequence_exhausted_games),
      Rcpp::_["roll_sequence_exhausted_games"] = Rcpp::IntegerVector::create(roll_sequence_exhausted_games),
      // LINE NOTE: Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
      Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
      // LINE NOTE: Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
      Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
      // LINE NOTE: Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
      Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
      // LINE NOTE: Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns),
      Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns),
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
}

// LINE NOTE: }  // namespace
}  // namespace

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: // Public C++ API: simulate many games with random dice.
// Public C++ API: simulate many games with random dice.
// LINE NOTE: MatchupSimulationResult simulate_matchup_random(
MatchupSimulationResult simulate_matchup_random(
    // LINE NOTE: const BoardState& initial_board,
    const BoardState& initial_board,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: std::mt19937& rng,
    std::mt19937& rng,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const RolloutConfig& rollout_config) {
    const RolloutConfig& rollout_config) {
  // LINE NOTE: // Validate selector labels and simulation controls before running any game.
  // Validate selector labels and simulation controls before running any game.
  // LINE NOTE: validate_selection(player1_selection);
  validate_selection(player1_selection);
  // LINE NOTE: validate_selection(player2_selection);
  validate_selection(player2_selection);
  // LINE NOTE: validate_n_games(n_games);
  validate_n_games(n_games);
  // LINE NOTE: validate_max_turns(max_turns);
  validate_max_turns(max_turns);
  // LINE NOTE: validate_rollout_config(rollout_config);
  validate_rollout_config(rollout_config);

  // LINE NOTE: MatchupSimulationResult result;
  MatchupSimulationResult result;
  // LINE NOTE: result.initial_board = initial_board;
  result.initial_board = initial_board;
  // LINE NOTE: result.n_games = n_games;
  result.n_games = n_games;
  // LINE NOTE: result.max_turns = max_turns;
  result.max_turns = max_turns;
  // LINE NOTE: result.used_scripted_rolls = false;
  result.used_scripted_rolls = false;
  // LINE NOTE: result.player1_selection = player1_selection;
  result.player1_selection = player1_selection;
  // LINE NOTE: result.player2_selection = player2_selection;
  result.player2_selection = player2_selection;
  // LINE NOTE: result.rollout_budget = rollout_config.budget;
  result.rollout_budget = rollout_config.budget;
  // LINE NOTE: result.rollout_policy = rollout_config.policy;
  result.rollout_policy = rollout_config.policy;
  // LINE NOTE: result.max_rollout_turns = rollout_config.max_turns;
  result.max_rollout_turns = rollout_config.max_turns;
  // LINE NOTE: result.games.reserve(n_games);
  result.games.reserve(n_games);

  // LINE NOTE: for (int game_id = 1; game_id <= n_games; ++game_id) {
  for (int game_id = 1; game_id <= n_games; ++game_id) {
    // LINE NOTE: // Each game starts from the same initial board and independent dice stream.
    // Each game starts from the same initial board and independent dice stream.
    // LINE NOTE: result.games.push_back(simulate_one_game_random(
    result.games.push_back(simulate_one_game_random(
        // LINE NOTE: initial_board,
        initial_board,
        // LINE NOTE: game_id,
        game_id,
        // LINE NOTE: max_turns,
        max_turns,
        // LINE NOTE: rng,
        rng,
        // LINE NOTE: player1_selection,
        player1_selection,
        // LINE NOTE: player2_selection,
        player2_selection,
        // LINE NOTE: rollout_config));
        rollout_config));
  // LINE NOTE: }
  }

  // LINE NOTE: return result;
  return result;
// LINE NOTE: }
}

// LINE NOTE: // Public C++ API: simulate many games using scripted dice sequence.
// Public C++ API: simulate many games using scripted dice sequence.
// LINE NOTE: MatchupSimulationResult simulate_matchup_with_rolls(
MatchupSimulationResult simulate_matchup_with_rolls(
    // LINE NOTE: const BoardState& initial_board,
    const BoardState& initial_board,
    // LINE NOTE: const std::vector<DiceRoll>& rolls,
    const std::vector<DiceRoll>& rolls,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: std::mt19937* rng,
    std::mt19937* rng,
    // LINE NOTE: const RolloutConfig& rollout_config) {
    const RolloutConfig& rollout_config) {
  // LINE NOTE: // Same validation contract as random-dice entry point.
  // Same validation contract as random-dice entry point.
  // LINE NOTE: validate_selection(player1_selection);
  validate_selection(player1_selection);
  // LINE NOTE: validate_selection(player2_selection);
  validate_selection(player2_selection);
  // LINE NOTE: validate_n_games(n_games);
  validate_n_games(n_games);
  // LINE NOTE: validate_max_turns(max_turns);
  validate_max_turns(max_turns);
  // LINE NOTE: validate_rollout_config(rollout_config);
  validate_rollout_config(rollout_config);

  // LINE NOTE: MatchupSimulationResult result;
  MatchupSimulationResult result;
  // LINE NOTE: result.initial_board = initial_board;
  result.initial_board = initial_board;
  // LINE NOTE: result.n_games = n_games;
  result.n_games = n_games;
  // LINE NOTE: result.max_turns = max_turns;
  result.max_turns = max_turns;
  // LINE NOTE: result.used_scripted_rolls = true;
  result.used_scripted_rolls = true;
  // LINE NOTE: result.player1_selection = player1_selection;
  result.player1_selection = player1_selection;
  // LINE NOTE: result.player2_selection = player2_selection;
  result.player2_selection = player2_selection;
  // LINE NOTE: result.rollout_budget = rollout_config.budget;
  result.rollout_budget = rollout_config.budget;
  // LINE NOTE: result.rollout_policy = rollout_config.policy;
  result.rollout_policy = rollout_config.policy;
  // LINE NOTE: result.max_rollout_turns = rollout_config.max_turns;
  result.max_rollout_turns = rollout_config.max_turns;
  // LINE NOTE: result.games.reserve(n_games);
  result.games.reserve(n_games);

  // LINE NOTE: for (int game_id = 1; game_id <= n_games; ++game_id) {
  for (int game_id = 1; game_id <= n_games; ++game_id) {
    // LINE NOTE: // Each replay uses identical scripted roll prefix but independent board state.
    // Each replay uses identical scripted roll prefix but independent board state.
    // LINE NOTE: result.games.push_back(simulate_one_game_with_rolls(
    result.games.push_back(simulate_one_game_with_rolls(
        // LINE NOTE: initial_board,
        initial_board,
        // LINE NOTE: rolls,
        rolls,
        // LINE NOTE: game_id,
        game_id,
        // LINE NOTE: max_turns,
        max_turns,
        // LINE NOTE: player1_selection,
        player1_selection,
        // LINE NOTE: player2_selection,
        player2_selection,
        // LINE NOTE: rng,
        rng,
        // LINE NOTE: rollout_config));
        rollout_config));
  // LINE NOTE: }
  }

  // LINE NOTE: return result;
  return result;
// LINE NOTE: }
}

// LINE NOTE: // Convert internal result object to R list object used by R wrappers.
// Convert internal result object to R list object used by R wrappers.
// LINE NOTE: Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result) {
Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result) {
  // LINE NOTE: // Keep both granular (games) and aggregate (summary/settings) views.
  // Keep both granular (games) and aggregate (summary/settings) views.
  // LINE NOTE: return Rcpp::List::create(
  return Rcpp::List::create(
      // LINE NOTE: Rcpp::_["initial_board"] = board_to_list(result.initial_board),
      Rcpp::_["initial_board"] = board_to_list(result.initial_board),
      // LINE NOTE: Rcpp::_["games"] = games_to_data_frame(result),
      Rcpp::_["games"] = games_to_data_frame(result),
      // LINE NOTE: Rcpp::_["summary"] = summary_to_data_frame(result),
      Rcpp::_["summary"] = summary_to_data_frame(result),
      // LINE NOTE: Rcpp::_["settings"] = Rcpp::List::create(
      Rcpp::_["settings"] = Rcpp::List::create(
          // LINE NOTE: Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
          Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
          // LINE NOTE: Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
          Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
          // LINE NOTE: Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
          Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
          // LINE NOTE: Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(result.max_turns),
          Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(result.max_turns),
          // LINE NOTE: Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
          Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
          // LINE NOTE: Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
          Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
          // LINE NOTE: Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
          Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
          // LINE NOTE: Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns)));
          Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns)));
// LINE NOTE: }
}

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_simulate_matchup_random(
Rcpp::List bg_cpp_simulate_matchup_random(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Parse board and initialize RNG once for the full simulation batch.
  // Parse board and initialize RNG once for the full simulation batch.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: // Non-rollout wrapper uses default rollout config values.
  // Non-rollout wrapper uses default rollout config values.
  // LINE NOTE: return backgammonr::matchup_simulation_result_to_list(
  return backgammonr::matchup_simulation_result_to_list(
      // LINE NOTE: backgammonr::simulate_matchup_random(
      backgammonr::simulate_matchup_random(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: n_games,
          n_games,
          // LINE NOTE: max_turns,
          max_turns,
          // LINE NOTE: rng,
          rng,
          // LINE NOTE: player1_selection,
          player1_selection,
          // LINE NOTE: player2_selection,
          player2_selection,
          // LINE NOTE: backgammonr::RolloutConfig()));
          backgammonr::RolloutConfig()));
// LINE NOTE: }
}

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_simulate_matchup_scripted(
Rcpp::List bg_cpp_simulate_matchup_scripted(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& rolls,
    const Rcpp::List& rolls,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Parse board + scripted rolls up front.
  // Parse board + scripted rolls up front.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);

  // LINE NOTE: std::mt19937 rng;
  std::mt19937 rng;
  // LINE NOTE: std::mt19937* rng_ptr = nullptr;
  std::mt19937* rng_ptr = nullptr;
  // LINE NOTE: // If no policy uses randomness we can skip RNG allocation entirely.
  // If no policy uses randomness we can skip RNG allocation entirely.
  // LINE NOTE: if (selection_uses_randomness(player1_selection) || selection_uses_randomness(player2_selection)) {
  if (selection_uses_randomness(player1_selection) || selection_uses_randomness(player2_selection)) {
    // LINE NOTE: rng = init_rng(seed, use_seed);
    rng = init_rng(seed, use_seed);
    // LINE NOTE: rng_ptr = &rng;
    rng_ptr = &rng;
  // LINE NOTE: }
  }

  // LINE NOTE: return backgammonr::matchup_simulation_result_to_list(
  return backgammonr::matchup_simulation_result_to_list(
      // LINE NOTE: backgammonr::simulate_matchup_with_rolls(
      backgammonr::simulate_matchup_with_rolls(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_rolls,
          parsed_rolls,
          // LINE NOTE: n_games,
          n_games,
          // LINE NOTE: max_turns,
          max_turns,
          // LINE NOTE: player1_selection,
          player1_selection,
          // LINE NOTE: player2_selection,
          player2_selection,
          // LINE NOTE: rng_ptr,
          rng_ptr,
          // LINE NOTE: backgammonr::RolloutConfig()));
          backgammonr::RolloutConfig()));
// LINE NOTE: }
}

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_simulate_matchup_random_rollout(
Rcpp::List bg_cpp_simulate_matchup_random_rollout(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const int rollout_budget,
    const int rollout_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Random-dice rollout matchup wrapper (rollout policies enabled).
  // Random-dice rollout matchup wrapper (rollout policies enabled).
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);
  // LINE NOTE: // Rollout-specific config used when players are rollout family selectors.
  // Rollout-specific config used when players are rollout family selectors.
  // LINE NOTE: const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  // LINE NOTE: return backgammonr::matchup_simulation_result_to_list(
  return backgammonr::matchup_simulation_result_to_list(
      // LINE NOTE: backgammonr::simulate_matchup_random(
      backgammonr::simulate_matchup_random(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: n_games,
          n_games,
          // LINE NOTE: max_turns,
          max_turns,
          // LINE NOTE: rng,
          rng,
          // LINE NOTE: player1_selection,
          player1_selection,
          // LINE NOTE: player2_selection,
          player2_selection,
          // LINE NOTE: rollout_config));
          rollout_config));
// LINE NOTE: }
}

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_simulate_matchup_scripted_rollout(
Rcpp::List bg_cpp_simulate_matchup_scripted_rollout(
    // LINE NOTE: const Rcpp::List& board,
    const Rcpp::List& board,
    // LINE NOTE: const Rcpp::List& rolls,
    const Rcpp::List& rolls,
    // LINE NOTE: const int n_games,
    const int n_games,
    // LINE NOTE: const int max_turns,
    const int max_turns,
    // LINE NOTE: const std::string& player1_selection,
    const std::string& player1_selection,
    // LINE NOTE: const std::string& player2_selection,
    const std::string& player2_selection,
    // LINE NOTE: const int rollout_budget,
    const int rollout_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Scripted-dice rollout matchup wrapper (rollout policies enabled).
  // Scripted-dice rollout matchup wrapper (rollout policies enabled).
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);
  // LINE NOTE: // Same rollout config but with scripted rolls.
  // Same rollout config but with scripted rolls.
  // LINE NOTE: const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  // LINE NOTE: return backgammonr::matchup_simulation_result_to_list(
  return backgammonr::matchup_simulation_result_to_list(
      // LINE NOTE: backgammonr::simulate_matchup_with_rolls(
      backgammonr::simulate_matchup_with_rolls(
          // LINE NOTE: parsed_board,
          parsed_board,
          // LINE NOTE: parsed_rolls,
          parsed_rolls,
          // LINE NOTE: n_games,
          n_games,
          // LINE NOTE: max_turns,
          max_turns,
          // LINE NOTE: player1_selection,
          player1_selection,
          // LINE NOTE: player2_selection,
          player2_selection,
          // LINE NOTE: &rng,
          &rng,
          // LINE NOTE: rollout_config));
          rollout_config));
// LINE NOTE: }
}
