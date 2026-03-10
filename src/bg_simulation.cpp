#include "bg_simulation.h"
// This translation unit implements the multi-game simulation layer that
// aggregates per-game stochastic outcomes into study-ready summary tables.

#include <algorithm>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "bg_game.h"
#include "bg_movegen.h"
#include "bg_rng.h"
#include "bg_rules.h"

namespace {

// Function: selection_uses_randomness
// Purpose: Proxy to shared selector randomness check in rollout module.
// Called by: bg_cpp_simulate_matchup_scripted() to skip RNG setup when both
// policies are deterministic.
bool selection_uses_randomness(const std::string& selection) {
  return backgammonr::selection_uses_randomness(selection);
}

// Function: validate_n_games
// Purpose: Validate simulation replication count.
// Called by: simulate_matchup_random(), simulate_matchup_with_rolls().
void validate_n_games(const int n_games) {
  if (n_games < 1) {
    throw std::range_error("`n_games` must be at least 1.");
  }
}

// Function: validate_max_turns
// Purpose: Validate per-game turn cap.
// Called by: simulate_matchup_random(), simulate_matchup_with_rolls().
void validate_max_turns(const int max_turns) {
  if (max_turns < 0) {
    throw std::range_error("`max_turns` must be nonnegative.");
  }
}

// Function: parse_roll_vector
// Purpose: Parse R list of roll objects into C++ DiceRoll vector.
// Called by: scripted simulation wrappers (with/without rollout config).
std::vector<backgammonr::DiceRoll> parse_roll_vector(const Rcpp::List& rolls) {
  std::vector<backgammonr::DiceRoll> out;
  out.reserve(rolls.size());

  for (int i = 0; i < rolls.size(); ++i) {
    SEXP roll_sexp = rolls[i];
    if (!Rf_isNewList(roll_sexp)) {
      std::ostringstream oss;
      oss << "`rolls[[" << (i + 1) << "]]` must be a roll-like list.";
      throw std::range_error(oss.str());
    }

    out.push_back(backgammonr::parse_roll_list(Rcpp::List(roll_sexp)));
  }

  return out;
}

// Function: selection_for_player_unchecked
// Purpose: Resolve current player's selector without extra branching in loops.
// Called by: simulate_one_game_random(), simulate_one_game_with_rolls().
const std::string& selection_for_player_unchecked(
    const int player,
    const std::string& player1_selection,
    const std::string& player2_selection) {
  return player == 1 ? player1_selection : player2_selection;
}

// Function: winner_label
// Purpose: Convert winner integer code into stable text label.
// Called by: games_to_data_frame().
std::string winner_label(const int winner) {
  if (winner == 1) {
    return "player_1";
  }

  if (winner == -1) {
    return "player_2";
  }

  return "none";
}

// Function: play_random_turn_with_roll_lightweight
// Purpose: Fast random-policy turn execution used in hot simulation loops.
// Called by: simulate_one_game_random(), simulate_one_game_with_rolls().
void play_random_turn_with_roll_lightweight(
    backgammonr::BoardState& board,
    const backgammonr::DiceRoll& roll,
    std::mt19937& rng) {
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
}

// Function: simulate_one_game_random
// Purpose: Simulate a single game trajectory under random dice generation.
// Called by: simulate_matchup_random().
// Notes: This is the per-replication kernel for random-dice experiments.
backgammonr::SimulatedGameSummary simulate_one_game_random(
    const backgammonr::BoardState& initial_board,
    const int game_id,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const backgammonr::RolloutConfig& rollout_config) {
  // One game-level record is filled as the trajectory evolves.
  backgammonr::SimulatedGameSummary out;
  out.game_id = game_id;

  // Early exit if initial board is already terminal.
  if (backgammonr::board_is_terminal(initial_board)) {
    out.game_over = true;
    out.winner = backgammonr::board_winner(initial_board);
    return out;
  }

  backgammonr::BoardState current = initial_board;
  for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
    // One loop iteration = one full turn transition in the simulated game.
    // Resolve selection policy for current player.
    const std::string& turn_selection =
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    if (turn_selection == "random") {
      // Fast path: random policy can bypass some heavier policy machinery.
      // Keep random-policy simulation lightweight.
      play_random_turn_with_roll_lightweight(current, backgammonr::roll_dice(rng), rng);
    } else {
      // Delegate non-random policies to the general turn engine.
      const backgammonr::TurnResult turn_result =
          backgammonr::play_turn_random(current, rng, turn_selection, rollout_config);
      current = turn_result.board_after;
      if (turn_result.game_over) {
        out.game_over = true;
        out.winner = turn_result.winner;
        out.n_turns += 1;
        break;
      }
    }
    out.n_turns += 1;

    // Random turn path can finish game without TurnResult object.
    // We still need a terminal check here because the fast path does not return
    // an explicit `TurnResult` object carrying `game_over`.
    if (backgammonr::board_is_terminal(current)) {
      out.game_over = true;
      out.winner = backgammonr::board_winner(current);
      break;
    }
  }

  out.turn_limit_reached = !out.game_over && out.n_turns == max_turns;
  return out;
}

// Function: simulate_one_game_with_rolls
// Purpose: Simulate a single game trajectory with scripted dice outcomes.
// Called by: simulate_matchup_with_rolls().
// Notes: Used for controlled/replayable comparisons across selection methods.
backgammonr::SimulatedGameSummary simulate_one_game_with_rolls(
    const backgammonr::BoardState& initial_board,
    const std::vector<backgammonr::DiceRoll>& rolls,
    const int game_id,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const backgammonr::RolloutConfig& rollout_config) {
  // Same structure as random-dice kernel, but roll values are externally fixed.
  backgammonr::SimulatedGameSummary out;
  out.game_id = game_id;

  if (backgammonr::board_is_terminal(initial_board)) {
    out.game_over = true;
    out.winner = backgammonr::board_winner(initial_board);
    return out;
  }

  backgammonr::BoardState current = initial_board;
  for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
    // Script exhausted before turn cap.
    if (turn_index >= static_cast<int>(rolls.size())) {
      // Script ended before the simulation reached terminal state or turn cap.
      out.roll_sequence_exhausted = true;
      break;
    }

    const std::string& turn_selection =
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    if (turn_selection == "random") {
      // Random selection still needs RNG for tie-breaking / random move choice,
      // even though dice are scripted.
      if (rng == nullptr) {
        throw std::range_error("Random selection with scripted rolls requires an RNG.");
      }
      // Random-policy fast path with supplied roll.
      play_random_turn_with_roll_lightweight(current, rolls[turn_index], *rng);
    } else {
      // Non-random policies still use scripted roll.
      const backgammonr::TurnResult turn_result =
          backgammonr::play_turn_with_roll(current, rolls[turn_index], turn_selection, rng, rollout_config);
      current = turn_result.board_after;
      if (turn_result.game_over) {
        out.game_over = true;
        out.winner = turn_result.winner;
        out.n_turns += 1;
        break;
      }
    }
    out.n_turns += 1;

    if (backgammonr::board_is_terminal(current)) {
      out.game_over = true;
      out.winner = backgammonr::board_winner(current);
      break;
    }
  }

  out.turn_limit_reached = !out.game_over && !out.roll_sequence_exhausted && out.n_turns == max_turns;
  return out;
}

// Function: games_to_data_frame
// Purpose: Convert per-game simulation records into row-wise R table.
// Called by: matchup_simulation_result_to_list().
Rcpp::DataFrame games_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
  const int n = static_cast<int>(result.games.size());
  Rcpp::IntegerVector game_id(n);
  Rcpp::IntegerVector winner(n);
  Rcpp::CharacterVector winner_label_vec(n);
  Rcpp::IntegerVector n_turns(n);
  Rcpp::LogicalVector game_over(n);
  Rcpp::LogicalVector turn_limit_reached(n);
  Rcpp::LogicalVector roll_sequence_exhausted(n);

  for (int i = 0; i < n; ++i) {
    const backgammonr::SimulatedGameSummary& game = result.games[i];
    game_id[i] = game.game_id;
    winner[i] = game.winner;
    winner_label_vec[i] = winner_label(game.winner);
    n_turns[i] = game.n_turns;
    game_over[i] = game.game_over;
    turn_limit_reached[i] = game.turn_limit_reached;
    roll_sequence_exhausted[i] = game.roll_sequence_exhausted;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["game_id"] = game_id,
      Rcpp::_["winner"] = winner,
      Rcpp::_["winner_label"] = winner_label_vec,
      Rcpp::_["n_turns"] = n_turns,
      Rcpp::_["game_over"] = game_over,
      Rcpp::_["turn_limit_reached"] = turn_limit_reached,
      Rcpp::_["roll_sequence_exhausted"] = roll_sequence_exhausted,
      Rcpp::_["stringsAsFactors"] = false);
}

// Function: summary_to_data_frame
// Purpose: Aggregate game-level outputs into one-row matchup summary metrics.
// Called by: matchup_simulation_result_to_list().
Rcpp::DataFrame summary_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
  int completed_games = 0;
  int unresolved_games = 0;
  int player1_wins = 0;
  int player2_wins = 0;
  int turn_limit_reached_games = 0;
  int roll_sequence_exhausted_games = 0;
  int min_turns = 0;
  int max_turns = 0;
  double mean_turns = NA_REAL;

  if (!result.games.empty()) {
    // Initialize min/max from first game, then update during scan.
    min_turns = result.games.front().n_turns;
    max_turns = result.games.front().n_turns;
    long long turn_sum = 0;

    for (const backgammonr::SimulatedGameSummary& game : result.games) {
      // Single-pass accumulation keeps summary creation O(n_games).
      turn_sum += static_cast<long long>(game.n_turns);
      min_turns = std::min(min_turns, game.n_turns);
      max_turns = std::max(max_turns, game.n_turns);

      if (game.winner == 1) {
        ++player1_wins;
      } else if (game.winner == -1) {
        ++player2_wins;
      } else {
        ++unresolved_games;
      }

      if (game.game_over) {
        ++completed_games;
      }
      if (game.turn_limit_reached) {
        ++turn_limit_reached_games;
      }
      if (game.roll_sequence_exhausted) {
        ++roll_sequence_exhausted_games;
      }
    }

    mean_turns = static_cast<double>(turn_sum) / static_cast<double>(result.games.size());
  }

  const double player1_win_rate = completed_games > 0
      ? static_cast<double>(player1_wins) / static_cast<double>(completed_games)
      : NA_REAL;
  const double player2_win_rate = completed_games > 0
      ? static_cast<double>(player2_wins) / static_cast<double>(completed_games)
      : NA_REAL;

  return Rcpp::DataFrame::create(
      Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
      Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
      Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
      Rcpp::_["completed_games"] = Rcpp::IntegerVector::create(completed_games),
      Rcpp::_["unresolved_games"] = Rcpp::IntegerVector::create(unresolved_games),
      Rcpp::_["player1_wins"] = Rcpp::IntegerVector::create(player1_wins),
      Rcpp::_["player2_wins"] = Rcpp::IntegerVector::create(player2_wins),
      Rcpp::_["player1_win_rate"] = Rcpp::NumericVector::create(player1_win_rate),
      Rcpp::_["player2_win_rate"] = Rcpp::NumericVector::create(player2_win_rate),
      Rcpp::_["mean_turns"] = Rcpp::NumericVector::create(mean_turns),
      Rcpp::_["min_turns"] = Rcpp::IntegerVector::create(min_turns),
      Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(max_turns),
      Rcpp::_["turn_limit_reached_games"] = Rcpp::IntegerVector::create(turn_limit_reached_games),
      Rcpp::_["roll_sequence_exhausted_games"] = Rcpp::IntegerVector::create(roll_sequence_exhausted_games),
      Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
      Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
      Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
      Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns),
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace

namespace backgammonr {

// Function: simulate_matchup_random
// Purpose: Batch driver for `n_games` independent random-dice simulations.
// Called by: bg_cpp_simulate_matchup_random(),
// bg_cpp_simulate_matchup_random_rollout(), benchmark_matchup_random().
MatchupSimulationResult simulate_matchup_random(
    const BoardState& initial_board,
    const int n_games,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config) {
  // Validate selector labels and simulation controls before running any game.
  validate_selection(player1_selection);
  validate_selection(player2_selection);
  validate_n_games(n_games);
  validate_max_turns(max_turns);
  validate_rollout_config(rollout_config);

  MatchupSimulationResult result;
  result.initial_board = initial_board;
  result.n_games = n_games;
  result.max_turns = max_turns;
  result.used_scripted_rolls = false;
  result.player1_selection = player1_selection;
  result.player2_selection = player2_selection;
  result.rollout_budget = rollout_config.budget;
  result.rollout_policy = rollout_config.policy;
  result.max_rollout_turns = rollout_config.max_turns;
  result.games.reserve(n_games);

  for (int game_id = 1; game_id <= n_games; ++game_id) {
    // Each call consumes fresh RNG state so outcomes are independent draws.
    // Each game starts from the same initial board and independent dice stream.
    result.games.push_back(simulate_one_game_random(
        initial_board,
        game_id,
        max_turns,
        rng,
        player1_selection,
        player2_selection,
        rollout_config));
  }

  return result;
}

// Function: simulate_matchup_with_rolls
// Purpose: Batch driver for `n_games` scripted-dice simulations.
// Called by: bg_cpp_simulate_matchup_scripted(),
// bg_cpp_simulate_matchup_scripted_rollout(), benchmark_matchup_with_rolls().
MatchupSimulationResult simulate_matchup_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // Same validation contract as random-dice entry point.
  validate_selection(player1_selection);
  validate_selection(player2_selection);
  validate_n_games(n_games);
  validate_max_turns(max_turns);
  validate_rollout_config(rollout_config);

  MatchupSimulationResult result;
  result.initial_board = initial_board;
  result.n_games = n_games;
  result.max_turns = max_turns;
  result.used_scripted_rolls = true;
  result.player1_selection = player1_selection;
  result.player2_selection = player2_selection;
  result.rollout_budget = rollout_config.budget;
  result.rollout_policy = rollout_config.policy;
  result.max_rollout_turns = rollout_config.max_turns;
  result.games.reserve(n_games);

  for (int game_id = 1; game_id <= n_games; ++game_id) {
    // Each replay uses identical scripted roll prefix but independent board state.
    result.games.push_back(simulate_one_game_with_rolls(
        initial_board,
        rolls,
        game_id,
        max_turns,
        player1_selection,
        player2_selection,
        rng,
        rollout_config));
  }

  return result;
}

// Function: matchup_simulation_result_to_list
// Purpose: Package simulation output into R-friendly list bundle.
// Called by: all exported simulation wrappers in this file.
Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result) {
  // Keep both granular (games) and aggregate (summary/settings) views.
  return Rcpp::List::create(
      Rcpp::_["initial_board"] = board_to_list(result.initial_board),
      Rcpp::_["games"] = games_to_data_frame(result),
      Rcpp::_["summary"] = summary_to_data_frame(result),
      Rcpp::_["settings"] = Rcpp::List::create(
          Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
          Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection),
          Rcpp::_["n_games"] = Rcpp::IntegerVector::create(result.n_games),
          Rcpp::_["max_turns"] = Rcpp::IntegerVector::create(result.max_turns),
          Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
          Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(result.rollout_budget),
          Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(result.rollout_policy),
          Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(result.max_rollout_turns)));
}

}  // namespace backgammonr

// Function: bg_cpp_simulate_matchup_random
// Purpose: Rcpp entry point for random-dice simulation without explicit rollout
// policy tuning.
// Called by: R wrapper `simulate_matchup(..., scripted_rolls = NULL)`.
// [[Rcpp::export]]
Rcpp::List bg_cpp_simulate_matchup_random(
    const Rcpp::List& board,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // Parse board and initialize RNG once for the full simulation batch.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  // Non-rollout wrapper uses default rollout config values.
  return backgammonr::matchup_simulation_result_to_list(
      backgammonr::simulate_matchup_random(
          parsed_board,
          n_games,
          max_turns,
          rng,
          player1_selection,
          player2_selection,
          backgammonr::RolloutConfig()));
}

// Function: bg_cpp_simulate_matchup_scripted
// Purpose: Rcpp entry point for scripted-dice simulation.
// Called by: R wrapper `simulate_matchup(..., scripted_rolls = <list>)`.
// [[Rcpp::export]]
Rcpp::List bg_cpp_simulate_matchup_scripted(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // RNG is only created when at least one selected policy is stochastic.
  // Parse board + scripted rolls up front.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  // If no policy uses randomness we can skip RNG allocation entirely.
  if (selection_uses_randomness(player1_selection) || selection_uses_randomness(player2_selection)) {
    rng = backgammonr::init_rng(seed, use_seed);
    rng_ptr = &rng;
  }

  return backgammonr::matchup_simulation_result_to_list(
      backgammonr::simulate_matchup_with_rolls(
          parsed_board,
          parsed_rolls,
          n_games,
          max_turns,
          player1_selection,
          player2_selection,
          rng_ptr,
          backgammonr::RolloutConfig()));
}

// Function: bg_cpp_simulate_matchup_random_rollout
// Purpose: Rcpp entry point for random-dice simulation with explicit rollout
// config fields.
// Called by: R wrapper path used when rollout knobs are provided.
// [[Rcpp::export]]
Rcpp::List bg_cpp_simulate_matchup_random_rollout(
    const Rcpp::List& board,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Same as random wrapper, but threads rollout-policy parameters explicitly.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);
  // Rollout-specific config used when players are rollout family selectors.
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::matchup_simulation_result_to_list(
      backgammonr::simulate_matchup_random(
          parsed_board,
          n_games,
          max_turns,
          rng,
          player1_selection,
          player2_selection,
          rollout_config));
}

// Function: bg_cpp_simulate_matchup_scripted_rollout
// Purpose: Rcpp entry point for scripted-dice simulation with explicit rollout
// config fields.
// Called by: R scripted simulation wrapper with rollout controls.
// [[Rcpp::export]]
Rcpp::List bg_cpp_simulate_matchup_scripted_rollout(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Scripted-dice wrapper with rollout controls for reproducible comparisons.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);
  // Same rollout config but with scripted rolls.
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::matchup_simulation_result_to_list(
      backgammonr::simulate_matchup_with_rolls(
          parsed_board,
          parsed_rolls,
          n_games,
          max_turns,
          player1_selection,
          player2_selection,
          &rng,
          rollout_config));
}
