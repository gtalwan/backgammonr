// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_simulation.h"
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// This translation unit implements the multi-game simulation layer that
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// aggregates per-game stochastic outcomes into study-ready summary tables.

// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <algorithm>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <cstdint>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <random>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <sstream>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <stdexcept>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <string>
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include <vector>

// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_game.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_movegen.h"
// **WHAT IT'S DOING:** Loads a required C++ header so this file can use needed data structures, math utilities, or package interfaces.
// **IN PLAIN ENGLISH:** Think of this like bringing the right tools into the room before starting the analysis work.
#include "bg_rules.h"

// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace {

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Reuse the engine's randomness predicate so scripted simulation can decide
// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// whether it needs an RNG at all.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
bool selection_uses_randomness(const std::string& selection) {
  return backgammonr::selection_uses_randomness(selection);
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate number of games argument.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void validate_n_games(const int n_games) {
  if (n_games < 1) {
    throw std::range_error("`n_games` must be at least 1.");
  }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Validate per-game turn cap.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void validate_max_turns(const int max_turns) {
  if (max_turns < 0) {
    throw std::range_error("`max_turns` must be nonnegative.");
  }
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Build deterministic or nondeterministic RNG depending on user arguments.
// **WHAT IT'S DOING:** Creates a Mersenne Twister random-number generator used for reproducible stochastic simulation.
// **IN PLAIN ENGLISH:** This is the randomness engine that drives rollouts and posterior sampling.
std::mt19937 init_rng(const int seed, const bool use_seed) {
  std::mt19937 rng;

  if (use_seed) {
    if (seed < 0) {
      throw std::range_error("`seed` must be nonnegative when supplied.");
    }
    rng.seed(static_cast<std::uint32_t>(seed));
  } else {
    std::random_device rd;
    rng.seed(rd());
  }

  return rng;
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Parse list of scripted rolls from R into engine roll objects.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Pick active selection rule based on whose turn it is.
// **WHAT IT'S DOING:** Declares an immutable value to make intent explicit and prevent accidental mutation.
// **IN PLAIN ENGLISH:** This locks a value so it cannot be changed later by mistake.
const std::string& selection_for_player_unchecked(
    const int player,
    const std::string& player1_selection,
    const std::string& player2_selection) {
  return player == 1 ? player1_selection : player2_selection;
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Convert winner code to stable user-facing label.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
std::string winner_label(const int winner) {
  if (winner == 1) {
    return "player_1";
  }

  if (winner == -1) {
    return "player_2";
  }

  return "none";
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Fast path for pure random-policy turns (hot loop in simulations).
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
void play_random_turn_with_roll_lightweight(
    backgammonr::BoardState& board,
    const backgammonr::DiceRoll& roll,
    std::mt19937& rng) {
  (void) backgammonr::play_random_turn_rollout_fast(board, roll, rng);
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Simulate one game with random dice.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
backgammonr::SimulatedGameSummary simulate_one_game_random(
    const backgammonr::BoardState& initial_board,
    const int game_id,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const backgammonr::RolloutConfig& rollout_config) {
  // **WHAT IT'S DOING (DETAILED):**
  // This function plays exactly one stochastic game trajectory.
  // - It starts from a fixed board state.
  // - Each turn chooses a policy based on whose turn it is.
  // - It applies one move, checks terminal status, and records metadata.
  // - It stops on game-over or turn budget exhaustion.
  // **IN PLAIN ENGLISH:** Run one complete game simulation and record who won,
  // how long it took, and whether we hit stopping limits.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Simulate one game with scripted roll sequence.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
backgammonr::SimulatedGameSummary simulate_one_game_with_rolls(
    const backgammonr::BoardState& initial_board,
    const std::vector<backgammonr::DiceRoll>& rolls,
    const int game_id,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const backgammonr::RolloutConfig& rollout_config) {
  // **WHAT IT'S DOING (DETAILED):** Same one-game simulation skeleton as the
  // random-dice version, but dice outcomes come from a fixed scripted sequence.
  // This supports controlled experiments (shared randomness / replayability).
  // **IN PLAIN ENGLISH:** Play one game using pre-specified dice so runs are
  // reproducible and easier to compare across methods.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Convert per-game simulation records into rectangular R table.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Aggregate matchup-level summary statistics.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::DataFrame summary_to_data_frame(const backgammonr::MatchupSimulationResult& result) {
  // **WHAT IT'S DOING (DETAILED):** Reduces per-game records into one-row
  // benchmark summary statistics: completion counts, win rates, and turn-length
  // distribution summaries.
  // **IN PLAIN ENGLISH:** Turn many game rows into one scoreboard row.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
}  // namespace

// **WHAT IT'S DOING:** Opens a namespace scope so related symbols stay organized and do not collide with similarly named code elsewhere.
// **IN PLAIN ENGLISH:** This creates a labeled section so names are easier to manage and safer to reuse.
namespace backgammonr {

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Public C++ API: simulate many games with random dice.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
MatchupSimulationResult simulate_matchup_random(
    const BoardState& initial_board,
    const int n_games,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config) {
  // **WHAT IT'S DOING (DETAILED):** Batch driver for random-dice experiments.
  // Validates configuration once, then replays `n_games` independent trajectories
  // from the same initial board and stores all game-level outcomes.
  // **IN PLAIN ENGLISH:** Run lots of random games and collect them in one result.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Public C++ API: simulate many games using scripted dice sequence.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
MatchupSimulationResult simulate_matchup_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // **WHAT IT'S DOING (DETAILED):** Batch driver for scripted-dice experiments.
  // Every game sees the same scripted roll prefix, isolating policy effects from
  // dice-sequence variability.
  // **IN PLAIN ENGLISH:** Compare methods under a controlled dice script.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// Convert internal result object to R list object used by R wrappers.
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
}  // namespace backgammonr

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_simulate_matchup_random(
    const Rcpp::List& board,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):** Exported Rcpp entry point for random-dice
  // simulations without explicit rollout-policy parameters.
  // **IN PLAIN ENGLISH:** R calls this when you want quick random-matchup sims.
  // Parse board and initialize RNG once for the full simulation batch.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);

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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
Rcpp::List bg_cpp_simulate_matchup_scripted(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // **WHAT IT'S DOING (DETAILED):** Exported Rcpp entry point for scripted-dice
  // simulations. RNG is created only if any selected policy actually uses
  // randomness, avoiding unnecessary setup.
  // **IN PLAIN ENGLISH:** Run scripted-roll simulations efficiently; skip RNG
  // overhead when deterministic policies are used.
  // Parse board + scripted rolls up front.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  // If no policy uses randomness we can skip RNG allocation entirely.
  if (selection_uses_randomness(player1_selection) || selection_uses_randomness(player2_selection)) {
    rng = init_rng(seed, use_seed);
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
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
  // **WHAT IT'S DOING (DETAILED):** Same as `bg_cpp_simulate_matchup_random`,
  // but explicitly threads rollout-policy parameters into the simulation config.
  // **IN PLAIN ENGLISH:** Random-dice batch simulation where rollout behavior is configurable.
  // Random-dice rollout matchup wrapper (rollout policies enabled).
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}

// **WHAT IT'S DOING:** Documents intent for the next code line or block so behavior is easier to audit and maintain.
// **IN PLAIN ENGLISH:** This sentence is there to explain why the next step exists.
// [[Rcpp::export]]
// **WHAT IT'S DOING:** Performs the next low-level step in the statistical allocation pipeline.
// **IN PLAIN ENGLISH:** This is one small instruction that helps turn noisy rollout outcomes into stable decision summaries.
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
  // **WHAT IT'S DOING (DETAILED):** Scripted-dice + rollout-config export path.
  // Useful for reproducible method comparisons where both dice sequence and
  // rollout policy parameters are controlled.
  // **IN PLAIN ENGLISH:** Controlled benchmark mode: fixed dice and configurable rollout policy.
  // Scripted-dice rollout matchup wrapper (rollout policies enabled).
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  std::mt19937 rng = init_rng(seed, use_seed);
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
// **WHAT IT'S DOING:** Ends the current block scope and returns to the outer context.
// **IN PLAIN ENGLISH:** This closes the section that just finished running.
}
