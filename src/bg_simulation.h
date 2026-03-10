#ifndef BACKGAMMONR_BG_SIMULATION_H
#define BACKGAMMONR_BG_SIMULATION_H

#include <Rcpp.h>
#include <random>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"
#include "bg_rollout.h"

namespace backgammonr {

struct SimulatedGameSummary {
  int game_id{0};
  int winner{0};
  int n_turns{0};
  bool game_over{false};
  bool turn_limit_reached{false};
  bool roll_sequence_exhausted{false};
};

struct MatchupSimulationResult {
  BoardState initial_board{};
  std::vector<SimulatedGameSummary> games{};
  int n_games{0};
  int max_turns{0};
  bool used_scripted_rolls{false};
  std::string player1_selection{"random"};
  std::string player2_selection{"random"};
  int rollout_budget{kDefaultRolloutBudget};
  std::string rollout_policy{"random"};
  int max_rollout_turns{kDefaultRolloutMaxTurns};
};

MatchupSimulationResult simulate_matchup_random(
    const BoardState& initial_board,
    int n_games,
    int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config = RolloutConfig());

MatchupSimulationResult simulate_matchup_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    int n_games,
    int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());

Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result);

}  // namespace backgammonr

#endif
