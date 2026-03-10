// LINE NOTE: #ifndef BACKGAMMONR_BG_BENCHMARK_H
#ifndef BACKGAMMONR_BG_BENCHMARK_H
// LINE NOTE: #define BACKGAMMONR_BG_BENCHMARK_H
#define BACKGAMMONR_BG_BENCHMARK_H

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>
// LINE NOTE: #include <optional>
#include <optional>
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
// LINE NOTE: #include "bg_simulation.h"
#include "bg_simulation.h"

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: struct MatchupBenchmarkResult {
struct MatchupBenchmarkResult {
// LINE NOTE:   MatchupSimulationResult simulation{};
  MatchupSimulationResult simulation{};
// LINE NOTE:   double runtime_seconds{0.0};
  double runtime_seconds{0.0};
// LINE NOTE: };
};

// LINE NOTE: struct MoveBenchmarkCase {
struct MoveBenchmarkCase {
// LINE NOTE:   std::string case_id{};
  std::string case_id{};
// LINE NOTE:   BoardState board{};
  BoardState board{};
// LINE NOTE:   DiceRoll roll{};
  DiceRoll roll{};
// LINE NOTE: };
};

// LINE NOTE: struct MoveBenchmarkRow {
struct MoveBenchmarkRow {
// LINE NOTE:   std::string case_id{};
  std::string case_id{};
// LINE NOTE:   std::string method{};
  std::string method{};
// LINE NOTE:   int n_legal_moves{0};
  int n_legal_moves{0};
// LINE NOTE:   int chosen_index{0};
  int chosen_index{0};
// LINE NOTE:   int reference_choice_index{NA_INTEGER};
  int reference_choice_index{NA_INTEGER};
// LINE NOTE:   int match_reference{NA_LOGICAL};
  int match_reference{NA_LOGICAL};
// LINE NOTE:   double runtime_seconds{0.0};
  double runtime_seconds{0.0};
// LINE NOTE: };
};

// LINE NOTE: struct MoveBenchmarkResult {
struct MoveBenchmarkResult {
// LINE NOTE:   std::vector<MoveBenchmarkRow> rows{};
  std::vector<MoveBenchmarkRow> rows{};
// LINE NOTE:   std::vector<std::string> methods{};
  std::vector<std::string> methods{};
// LINE NOTE:   std::optional<std::string> reference_method{std::nullopt};
  std::optional<std::string> reference_method{std::nullopt};
// LINE NOTE: };
};

// LINE NOTE: MatchupBenchmarkResult benchmark_matchup_random(
MatchupBenchmarkResult benchmark_matchup_random(
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

// LINE NOTE: MatchupBenchmarkResult benchmark_matchup_with_rolls(
MatchupBenchmarkResult benchmark_matchup_with_rolls(
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

// LINE NOTE: std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases);
std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases);

// LINE NOTE: MoveBenchmarkResult benchmark_move_evaluators(
MoveBenchmarkResult benchmark_move_evaluators(
// LINE NOTE:     const std::vector<MoveBenchmarkCase>& cases,
    const std::vector<MoveBenchmarkCase>& cases,
// LINE NOTE:     const std::vector<std::string>& methods,
    const std::vector<std::string>& methods,
// LINE NOTE:     const std::optional<std::string>& reference_method,
    const std::optional<std::string>& reference_method,
// LINE NOTE:     const RolloutConfig& method_rollout_config,
    const RolloutConfig& method_rollout_config,
// LINE NOTE:     const RolloutConfig& reference_rollout_config,
    const RolloutConfig& reference_rollout_config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);

// LINE NOTE: Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result);
Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result);
// LINE NOTE: Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result);
Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result);
// LINE NOTE: Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result);
Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result);
// LINE NOTE: Rcpp::List move_benchmark_result_to_list(
Rcpp::List move_benchmark_result_to_list(
// LINE NOTE:     const MoveBenchmarkResult& result,
    const MoveBenchmarkResult& result,
// LINE NOTE:     const RolloutConfig& method_rollout_config,
    const RolloutConfig& method_rollout_config,
// LINE NOTE:     const RolloutConfig& reference_rollout_config);
    const RolloutConfig& reference_rollout_config);

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: #endif
#endif
