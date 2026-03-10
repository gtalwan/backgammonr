// LINE NOTE: #ifndef BACKGAMMONR_BG_SIMULATION_H
#ifndef BACKGAMMONR_BG_SIMULATION_H
// LINE NOTE: #define BACKGAMMONR_BG_SIMULATION_H
#define BACKGAMMONR_BG_SIMULATION_H

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>
// LINE NOTE: #include <random>
#include <random>
// LINE NOTE: #include <string>
#include <string>
// LINE NOTE: #include <vector>
#include <vector>

// LINE NOTE: #include "bg_board.h"
#include "bg_board.h"
// LINE NOTE: #include "bg_dice.h"
#include "bg_dice.h"
// LINE NOTE: #include "bg_rollout.h"
#include "bg_rollout.h"

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: struct SimulatedGameSummary {
struct SimulatedGameSummary {
// LINE NOTE:   int game_id{0};
  int game_id{0};
// LINE NOTE:   int winner{0};
  int winner{0};
// LINE NOTE:   int n_turns{0};
  int n_turns{0};
// LINE NOTE:   bool game_over{false};
  bool game_over{false};
// LINE NOTE:   bool turn_limit_reached{false};
  bool turn_limit_reached{false};
// LINE NOTE:   bool roll_sequence_exhausted{false};
  bool roll_sequence_exhausted{false};
// LINE NOTE: };
};

// LINE NOTE: struct MatchupSimulationResult {
struct MatchupSimulationResult {
// LINE NOTE:   BoardState initial_board{};
  BoardState initial_board{};
// LINE NOTE:   std::vector<SimulatedGameSummary> games{};
  std::vector<SimulatedGameSummary> games{};
// LINE NOTE:   int n_games{0};
  int n_games{0};
// LINE NOTE:   int max_turns{0};
  int max_turns{0};
// LINE NOTE:   bool used_scripted_rolls{false};
  bool used_scripted_rolls{false};
// LINE NOTE:   std::string player1_selection{"random"};
  std::string player1_selection{"random"};
// LINE NOTE:   std::string player2_selection{"random"};
  std::string player2_selection{"random"};
// LINE NOTE:   int rollout_budget{kDefaultRolloutBudget};
  int rollout_budget{kDefaultRolloutBudget};
// LINE NOTE:   std::string rollout_policy{"random"};
  std::string rollout_policy{"random"};
// LINE NOTE:   int max_rollout_turns{kDefaultRolloutMaxTurns};
  int max_rollout_turns{kDefaultRolloutMaxTurns};
// LINE NOTE: };
};

// LINE NOTE: MatchupSimulationResult simulate_matchup_random(
MatchupSimulationResult simulate_matchup_random(
// LINE NOTE:     const BoardState& initial_board,
    const BoardState& initial_board,
// LINE NOTE:     int n_games,
    int n_games,
// LINE NOTE:     int max_turns,
    int max_turns,
// LINE NOTE:     std::mt19937& rng,
    std::mt19937& rng,
// LINE NOTE:     const std::string& player1_selection,
    const std::string& player1_selection,
// LINE NOTE:     const std::string& player2_selection,
    const std::string& player2_selection,
// LINE NOTE:     const RolloutConfig& rollout_config = RolloutConfig());
    const RolloutConfig& rollout_config = RolloutConfig());

// LINE NOTE: MatchupSimulationResult simulate_matchup_with_rolls(
MatchupSimulationResult simulate_matchup_with_rolls(
// LINE NOTE:     const BoardState& initial_board,
    const BoardState& initial_board,
// LINE NOTE:     const std::vector<DiceRoll>& rolls,
    const std::vector<DiceRoll>& rolls,
// LINE NOTE:     int n_games,
    int n_games,
// LINE NOTE:     int max_turns,
    int max_turns,
// LINE NOTE:     const std::string& player1_selection,
    const std::string& player1_selection,
// LINE NOTE:     const std::string& player2_selection,
    const std::string& player2_selection,
// LINE NOTE:     std::mt19937* rng = nullptr,
    std::mt19937* rng = nullptr,
// LINE NOTE:     const RolloutConfig& rollout_config = RolloutConfig());
    const RolloutConfig& rollout_config = RolloutConfig());

// LINE NOTE: Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result);
Rcpp::List matchup_simulation_result_to_list(const MatchupSimulationResult& result);

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: #endif
#endif
