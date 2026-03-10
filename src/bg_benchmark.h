#ifndef BACKGAMMONR_BG_BENCHMARK_H
#define BACKGAMMONR_BG_BENCHMARK_H

#include <Rcpp.h>
#include <optional>
#include <random>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_dice.h"
#include "bg_rollout.h"
#include "bg_simulation.h"

namespace backgammonr {

struct MatchupBenchmarkResult {
  MatchupSimulationResult simulation{};
  double runtime_seconds{0.0};
};

struct MoveBenchmarkCase {
  std::string case_id{};
  BoardState board{};
  DiceRoll roll{};
};

struct MoveBenchmarkRow {
  std::string case_id{};
  std::string method{};
  int n_legal_moves{0};
  int chosen_index{0};
  int reference_choice_index{NA_INTEGER};
  int match_reference{NA_LOGICAL};
  double runtime_seconds{0.0};
};

struct MoveBenchmarkResult {
  std::vector<MoveBenchmarkRow> rows{};
  std::vector<std::string> methods{};
  std::optional<std::string> reference_method{std::nullopt};
};

MatchupBenchmarkResult benchmark_matchup_random(
    const BoardState& initial_board,
    int n_games,
    int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config = RolloutConfig());

MatchupBenchmarkResult benchmark_matchup_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    int n_games,
    int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng = nullptr,
    const RolloutConfig& rollout_config = RolloutConfig());

std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases);

MoveBenchmarkResult benchmark_move_evaluators(
    const std::vector<MoveBenchmarkCase>& cases,
    const std::vector<std::string>& methods,
    const std::optional<std::string>& reference_method,
    const RolloutConfig& method_rollout_config,
    const RolloutConfig& reference_rollout_config,
    std::mt19937& rng);

Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result);
Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result);
Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result);
Rcpp::List move_benchmark_result_to_list(
    const MoveBenchmarkResult& result,
    const RolloutConfig& method_rollout_config,
    const RolloutConfig& reference_rollout_config);

}  // namespace backgammonr

#endif
