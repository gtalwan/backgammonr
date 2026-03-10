// LINE NOTE: #include "bg_benchmark.h"
#include "bg_benchmark.h"
// LINE NOTE: // This translation unit contains benchmark-oriented orchestration for the
// This translation unit contains benchmark-oriented orchestration for the
// LINE NOTE: // rollout/statistical APIs. The game engine produces stochastic outcomes;
// rollout/statistical APIs. The game engine produces stochastic outcomes;
// LINE NOTE: // this file standardizes how we compare methods on shared cases and collect
// this file standardizes how we compare methods on shared cases and collect
// LINE NOTE: // reproducible runtime + decision-quality summaries.
// reproducible runtime + decision-quality summaries.

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>

// LINE NOTE: #include <chrono>
#include <chrono>
// LINE NOTE: #include <cstdint>
#include <cstdint>
// LINE NOTE: #include <functional>
#include <functional>
// LINE NOTE: #include <optional>
#include <optional>
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
// LINE NOTE: #include "bg_move.h"
#include "bg_move.h"
// LINE NOTE: #include "bg_movegen.h"
#include "bg_movegen.h"

// LINE NOTE: namespace {
namespace {

// LINE NOTE: using Clock = std::chrono::steady_clock;
using Clock = std::chrono::steady_clock;

// LINE NOTE: // Create deterministic/non-deterministic RNG stream.
// Create deterministic/non-deterministic RNG stream.
// LINE NOTE: // Determinism matters for method-vs-method comparability in benchmarks.
// Determinism matters for method-vs-method comparability in benchmarks.
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

// LINE NOTE: // Derive reproducible child RNG streams from case/method labels so each
// Derive reproducible child RNG streams from case/method labels so each
// LINE NOTE: // benchmark component can be replayed exactly.
// benchmark component can be replayed exactly.
// LINE NOTE: std::uint32_t stable_stream_seed(
std::uint32_t stable_stream_seed(
    // LINE NOTE: const std::uint32_t base_seed,
    const std::uint32_t base_seed,
    // LINE NOTE: const std::string& case_id,
    const std::string& case_id,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const std::string& stream_label) {
    const std::string& stream_label) {
  // LINE NOTE: const std::size_t hashed = std::hash<std::string>{}(case_id + "::" + method + "::" + stream_label);
  const std::size_t hashed = std::hash<std::string>{}(case_id + "::" + method + "::" + stream_label);
  // LINE NOTE: return static_cast<std::uint32_t>(hashed ^ static_cast<std::size_t>(base_seed));
  return static_cast<std::uint32_t>(hashed ^ static_cast<std::size_t>(base_seed));
// LINE NOTE: }
}

// LINE NOTE: // Small timing helper used for all benchmark runtime reporting.
// Small timing helper used for all benchmark runtime reporting.
// LINE NOTE: double elapsed_seconds(
double elapsed_seconds(
    // LINE NOTE: const std::chrono::time_point<Clock>& start,
    const std::chrono::time_point<Clock>& start,
    // LINE NOTE: const std::chrono::time_point<Clock>& end) {
    const std::chrono::time_point<Clock>& end) {
  // LINE NOTE: return std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count();
  return std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count();
// LINE NOTE: }
}

// LINE NOTE: // Parse R list of scripted rolls into internal DiceRoll objects.
// Parse R list of scripted rolls into internal DiceRoll objects.
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

// LINE NOTE: // Convenience check for optional fields in list-like case objects.
// Convenience check for optional fields in list-like case objects.
// LINE NOTE: bool has_named_element(const Rcpp::List& x, const char* name) {
bool has_named_element(const Rcpp::List& x, const char* name) {
  // LINE NOTE: return x.containsElementNamed(name);
  return x.containsElementNamed(name);
// LINE NOTE: }
}

// LINE NOTE: // Default case IDs are deterministic and human-readable.
// Default case IDs are deterministic and human-readable.
// LINE NOTE: std::string default_case_id(const int index) {
std::string default_case_id(const int index) {
  // LINE NOTE: std::ostringstream oss;
  std::ostringstream oss;
  // LINE NOTE: oss << "case_" << index;
  oss << "case_" << index;
  // LINE NOTE: return oss.str();
  return oss.str();
// LINE NOTE: }
}

// LINE NOTE: // Parse optional case ID while enforcing scalar/non-missing semantics.
// Parse optional case ID while enforcing scalar/non-missing semantics.
// LINE NOTE: std::string parse_case_id(const Rcpp::List& case_list, const int index) {
std::string parse_case_id(const Rcpp::List& case_list, const int index) {
  // LINE NOTE: if (!has_named_element(case_list, "case_id")) {
  if (!has_named_element(case_list, "case_id")) {
    // LINE NOTE: return default_case_id(index);
    return default_case_id(index);
  // LINE NOTE: }
  }

  // LINE NOTE: SEXP raw = case_list["case_id"];
  SEXP raw = case_list["case_id"];
  // LINE NOTE: if (raw == R_NilValue) {
  if (raw == R_NilValue) {
    // LINE NOTE: return default_case_id(index);
    return default_case_id(index);
  // LINE NOTE: }
  }

  // LINE NOTE: if (TYPEOF(raw) != STRSXP) {
  if (TYPEOF(raw) != STRSXP) {
    // LINE NOTE: throw std::range_error("`case_id` must be a character scalar when supplied.");
    throw std::range_error("`case_id` must be a character scalar when supplied.");
  // LINE NOTE: }
  }

  // LINE NOTE: Rcpp::CharacterVector case_id(raw);
  Rcpp::CharacterVector case_id(raw);
  // LINE NOTE: if (case_id.size() != 1 || case_id[0] == NA_STRING) {
  if (case_id.size() != 1 || case_id[0] == NA_STRING) {
    // LINE NOTE: throw std::range_error("`case_id` must be a non-missing character scalar when supplied.");
    throw std::range_error("`case_id` must be a non-missing character scalar when supplied.");
  // LINE NOTE: }
  }

  // LINE NOTE: const std::string parsed = Rcpp::as<std::string>(case_id[0]);
  const std::string parsed = Rcpp::as<std::string>(case_id[0]);
  // LINE NOTE: if (parsed.empty()) {
  if (parsed.empty()) {
    // LINE NOTE: return default_case_id(index);
    return default_case_id(index);
  // LINE NOTE: }
  }

  // LINE NOTE: return parsed;
  return parsed;
// LINE NOTE: }
}

// LINE NOTE: // Convert chosen move sequence into 1-based index in legal move table.
// Convert chosen move sequence into 1-based index in legal move table.
// LINE NOTE: int chosen_index_in_legal_moves(
int chosen_index_in_legal_moves(
    // LINE NOTE: const backgammonr::MoveSequence& chosen,
    const backgammonr::MoveSequence& chosen,
    // LINE NOTE: const std::vector<backgammonr::MoveSequence>& legal_moves) {
    const std::vector<backgammonr::MoveSequence>& legal_moves) {
  // LINE NOTE: for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    // LINE NOTE: if (backgammonr::move_sequences_equal(chosen, legal_moves[i])) {
    if (backgammonr::move_sequences_equal(chosen, legal_moves[i])) {
      // LINE NOTE: return i + 1;
      return i + 1;
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: throw std::range_error("Internal error: chosen move was not found in the legal-move set.");
  throw std::range_error("Internal error: chosen move was not found in the legal-move set.");
// LINE NOTE: }
}

// LINE NOTE: // Method-selection helper used by move-evaluator benchmark.
// Method-selection helper used by move-evaluator benchmark.
// LINE NOTE: // The key behavior is RNG stream isolation:
// The key behavior is RNG stream isolation:
// LINE NOTE: // - each case/method/stream_label gets a stable child stream
// - each case/method/stream_label gets a stable child stream
// LINE NOTE: // - this avoids accidental cross-method coupling.
// - this avoids accidental cross-method coupling.
// LINE NOTE: std::optional<backgammonr::MoveSequence> choose_move_for_benchmark(
std::optional<backgammonr::MoveSequence> choose_move_for_benchmark(
    // LINE NOTE: const backgammonr::BoardState& board,
    const backgammonr::BoardState& board,
    // LINE NOTE: const std::vector<backgammonr::MoveSequence>& legal_moves,
    const std::vector<backgammonr::MoveSequence>& legal_moves,
    // LINE NOTE: const std::string& case_id,
    const std::string& case_id,
    // LINE NOTE: const std::string& method,
    const std::string& method,
    // LINE NOTE: const std::string& stream_label,
    const std::string& stream_label,
    // LINE NOTE: const backgammonr::RolloutConfig& rollout_config,
    const backgammonr::RolloutConfig& rollout_config,
    // LINE NOTE: const std::uint32_t base_seed) {
    const std::uint32_t base_seed) {
  // LINE NOTE: if (legal_moves.empty()) {
  if (legal_moves.empty()) {
    // LINE NOTE: return std::nullopt;
    return std::nullopt;
  // LINE NOTE: }
  }

  // LINE NOTE: std::mt19937* rng_ptr = nullptr;
  std::mt19937* rng_ptr = nullptr;
  // LINE NOTE: std::mt19937 child_rng;
  std::mt19937 child_rng;
  // LINE NOTE: if (backgammonr::selection_uses_randomness(method)) {
  if (backgammonr::selection_uses_randomness(method)) {
    // LINE NOTE: child_rng.seed(stable_stream_seed(base_seed, case_id, method, stream_label));
    child_rng.seed(stable_stream_seed(base_seed, case_id, method, stream_label));
    // LINE NOTE: rng_ptr = &child_rng;
    rng_ptr = &child_rng;
  // LINE NOTE: }
  }

  // LINE NOTE: return backgammonr::choose_move_sequence(board, legal_moves, method, rng_ptr, rollout_config);
  return backgammonr::choose_move_sequence(board, legal_moves, method, rng_ptr, rollout_config);
// LINE NOTE: }
}

// LINE NOTE: // Attach runtime-derived throughput columns to summary data frame.
// Attach runtime-derived throughput columns to summary data frame.
// LINE NOTE: Rcpp::DataFrame add_runtime_columns_to_summary(const Rcpp::DataFrame& summary, const double runtime_seconds, const int n_games) {
Rcpp::DataFrame add_runtime_columns_to_summary(const Rcpp::DataFrame& summary, const double runtime_seconds, const int n_games) {
  // LINE NOTE: Rcpp::List out(summary);
  Rcpp::List out(summary);
  // LINE NOTE: out.push_back(Rcpp::NumericVector::create(runtime_seconds), "runtime_seconds");
  out.push_back(Rcpp::NumericVector::create(runtime_seconds), "runtime_seconds");
  // LINE NOTE: out.push_back(
  out.push_back(
      // LINE NOTE: Rcpp::NumericVector::create(n_games > 0 ? runtime_seconds / static_cast<double>(n_games) : NA_REAL),
      Rcpp::NumericVector::create(n_games > 0 ? runtime_seconds / static_cast<double>(n_games) : NA_REAL),
      // LINE NOTE: "seconds_per_game");
      "seconds_per_game");
  // LINE NOTE: out.push_back(
  out.push_back(
      // LINE NOTE: Rcpp::NumericVector::create(runtime_seconds > 0.0 ? static_cast<double>(n_games) / runtime_seconds : NA_REAL),
      Rcpp::NumericVector::create(runtime_seconds > 0.0 ? static_cast<double>(n_games) / runtime_seconds : NA_REAL),
      // LINE NOTE: "games_per_second");
      "games_per_second");
  // LINE NOTE: out.attr("class") = summary.attr("class");
  out.attr("class") = summary.attr("class");
  // LINE NOTE: out.attr("row.names") = summary.attr("row.names");
  out.attr("row.names") = summary.attr("row.names");
  // LINE NOTE: return Rcpp::DataFrame(out);
  return Rcpp::DataFrame(out);
// LINE NOTE: }
}

// LINE NOTE: }  // namespace
}  // namespace

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: // Benchmark random-roll matchup end-to-end and keep wall-clock runtime.
// Benchmark random-roll matchup end-to-end and keep wall-clock runtime.
// LINE NOTE: MatchupBenchmarkResult benchmark_matchup_random(
MatchupBenchmarkResult benchmark_matchup_random(
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
  // LINE NOTE: const auto start = Clock::now();
  const auto start = Clock::now();
  // LINE NOTE: const MatchupSimulationResult simulation = simulate_matchup_random(
  const MatchupSimulationResult simulation = simulate_matchup_random(
      // LINE NOTE: initial_board,
      initial_board,
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
      // LINE NOTE: rollout_config);
      rollout_config);
  // LINE NOTE: const auto end = Clock::now();
  const auto end = Clock::now();

  // LINE NOTE: MatchupBenchmarkResult out;
  MatchupBenchmarkResult out;
  // LINE NOTE: out.simulation = simulation;
  out.simulation = simulation;
  // LINE NOTE: out.runtime_seconds = elapsed_seconds(start, end);
  out.runtime_seconds = elapsed_seconds(start, end);
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Benchmark scripted-roll matchup end-to-end and keep wall-clock runtime.
// Benchmark scripted-roll matchup end-to-end and keep wall-clock runtime.
// LINE NOTE: MatchupBenchmarkResult benchmark_matchup_with_rolls(
MatchupBenchmarkResult benchmark_matchup_with_rolls(
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
  // LINE NOTE: const auto start = Clock::now();
  const auto start = Clock::now();
  // LINE NOTE: const MatchupSimulationResult simulation = simulate_matchup_with_rolls(
  const MatchupSimulationResult simulation = simulate_matchup_with_rolls(
      // LINE NOTE: initial_board,
      initial_board,
      // LINE NOTE: rolls,
      rolls,
      // LINE NOTE: n_games,
      n_games,
      // LINE NOTE: max_turns,
      max_turns,
      // LINE NOTE: player1_selection,
      player1_selection,
      // LINE NOTE: player2_selection,
      player2_selection,
      // LINE NOTE: rng,
      rng,
      // LINE NOTE: rollout_config);
      rollout_config);
  // LINE NOTE: const auto end = Clock::now();
  const auto end = Clock::now();

  // LINE NOTE: MatchupBenchmarkResult out;
  MatchupBenchmarkResult out;
  // LINE NOTE: out.simulation = simulation;
  out.simulation = simulation;
  // LINE NOTE: out.runtime_seconds = elapsed_seconds(start, end);
  out.runtime_seconds = elapsed_seconds(start, end);
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Parse list of benchmark cases:
// Parse list of benchmark cases:
// LINE NOTE: // each case must provide (board, roll), and may provide case_id.
// each case must provide (board, roll), and may provide case_id.
// LINE NOTE: std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases) {
std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases) {
  // LINE NOTE: std::vector<MoveBenchmarkCase> out;
  std::vector<MoveBenchmarkCase> out;
  // LINE NOTE: out.reserve(cases.size());
  out.reserve(cases.size());

  // LINE NOTE: for (int i = 0; i < cases.size(); ++i) {
  for (int i = 0; i < cases.size(); ++i) {
    // LINE NOTE: SEXP case_sexp = cases[i];
    SEXP case_sexp = cases[i];
    // LINE NOTE: if (!Rf_isNewList(case_sexp)) {
    if (!Rf_isNewList(case_sexp)) {
      // LINE NOTE: std::ostringstream oss;
      std::ostringstream oss;
      // LINE NOTE: oss << "`cases[[" << (i + 1) << "]]` must be a list-like benchmark case.";
      oss << "`cases[[" << (i + 1) << "]]` must be a list-like benchmark case.";
      // LINE NOTE: throw std::range_error(oss.str());
      throw std::range_error(oss.str());
    // LINE NOTE: }
    }

    // LINE NOTE: const Rcpp::List case_list(case_sexp);
    const Rcpp::List case_list(case_sexp);
    // LINE NOTE: if (!has_named_element(case_list, "board")) {
    if (!has_named_element(case_list, "board")) {
      // LINE NOTE: throw std::range_error("Each benchmark case must contain a `board` field.");
      throw std::range_error("Each benchmark case must contain a `board` field.");
    // LINE NOTE: }
    }
    // LINE NOTE: if (!has_named_element(case_list, "roll")) {
    if (!has_named_element(case_list, "roll")) {
      // LINE NOTE: throw std::range_error("Each benchmark case must contain a `roll` field.");
      throw std::range_error("Each benchmark case must contain a `roll` field.");
    // LINE NOTE: }
    }

    // LINE NOTE: SEXP board_raw = case_list["board"];
    SEXP board_raw = case_list["board"];
    // LINE NOTE: SEXP roll_raw = case_list["roll"];
    SEXP roll_raw = case_list["roll"];
    // LINE NOTE: if (!Rf_isNewList(board_raw)) {
    if (!Rf_isNewList(board_raw)) {
      // LINE NOTE: throw std::range_error("Each `board` field in `cases` must be a board-like list.");
      throw std::range_error("Each `board` field in `cases` must be a board-like list.");
    // LINE NOTE: }
    }
    // LINE NOTE: if (!Rf_isNewList(roll_raw)) {
    if (!Rf_isNewList(roll_raw)) {
      // LINE NOTE: throw std::range_error("Each `roll` field in `cases` must be a roll-like list.");
      throw std::range_error("Each `roll` field in `cases` must be a roll-like list.");
    // LINE NOTE: }
    }

    // LINE NOTE: MoveBenchmarkCase parsed;
    MoveBenchmarkCase parsed;
    // LINE NOTE: parsed.case_id = parse_case_id(case_list, i + 1);
    parsed.case_id = parse_case_id(case_list, i + 1);
    // LINE NOTE: parsed.board = parse_board_list(Rcpp::List(board_raw));
    parsed.board = parse_board_list(Rcpp::List(board_raw));
    // LINE NOTE: parsed.roll = parse_roll_list(Rcpp::List(roll_raw));
    parsed.roll = parse_roll_list(Rcpp::List(roll_raw));
    // LINE NOTE: out.push_back(parsed);
    out.push_back(parsed);
  // LINE NOTE: }
  }

  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Core move-evaluator benchmark:
// Core move-evaluator benchmark:
// LINE NOTE: // for each case and each method, choose one move and compare to optional
// for each case and each method, choose one move and compare to optional
// LINE NOTE: // reference method choice on the same legal-action set.
// reference method choice on the same legal-action set.
// LINE NOTE: MoveBenchmarkResult benchmark_move_evaluators(
MoveBenchmarkResult benchmark_move_evaluators(
    // LINE NOTE: const std::vector<MoveBenchmarkCase>& cases,
    const std::vector<MoveBenchmarkCase>& cases,
    // LINE NOTE: const std::vector<std::string>& methods,
    const std::vector<std::string>& methods,
    // LINE NOTE: const std::optional<std::string>& reference_method,
    const std::optional<std::string>& reference_method,
    // LINE NOTE: const RolloutConfig& method_rollout_config,
    const RolloutConfig& method_rollout_config,
    // LINE NOTE: const RolloutConfig& reference_rollout_config,
    const RolloutConfig& reference_rollout_config,
    // LINE NOTE: std::mt19937& rng) {
    std::mt19937& rng) {
  // LINE NOTE: validate_rollout_config(method_rollout_config);
  validate_rollout_config(method_rollout_config);
  // LINE NOTE: validate_rollout_config(reference_rollout_config);
  validate_rollout_config(reference_rollout_config);

  // LINE NOTE: if (cases.empty()) {
  if (cases.empty()) {
    // LINE NOTE: throw std::range_error("`cases` must contain at least one benchmark case.");
    throw std::range_error("`cases` must contain at least one benchmark case.");
  // LINE NOTE: }
  }
  // LINE NOTE: if (methods.empty()) {
  if (methods.empty()) {
    // LINE NOTE: throw std::range_error("`methods` must contain at least one selection method.");
    throw std::range_error("`methods` must contain at least one selection method.");
  // LINE NOTE: }
  }

  // LINE NOTE: for (const std::string& method : methods) {
  for (const std::string& method : methods) {
    // LINE NOTE: validate_selection(method);
    validate_selection(method);
  // LINE NOTE: }
  }
  // LINE NOTE: if (reference_method.has_value()) {
  if (reference_method.has_value()) {
    // LINE NOTE: validate_selection(reference_method.value());
    validate_selection(reference_method.value());
  // LINE NOTE: }
  }

  // LINE NOTE: MoveBenchmarkResult out;
  MoveBenchmarkResult out;
  // LINE NOTE: out.methods = methods;
  out.methods = methods;
  // LINE NOTE: out.reference_method = reference_method;
  out.reference_method = reference_method;
  // LINE NOTE: out.rows.reserve(static_cast<int>(cases.size()) * static_cast<int>(methods.size()));
  out.rows.reserve(static_cast<int>(cases.size()) * static_cast<int>(methods.size()));
  // LINE NOTE: const std::uint32_t base_seed = static_cast<std::uint32_t>(rng());
  const std::uint32_t base_seed = static_cast<std::uint32_t>(rng());

  // LINE NOTE: for (const MoveBenchmarkCase& benchmark_case : cases) {
  for (const MoveBenchmarkCase& benchmark_case : cases) {
    // LINE NOTE: // Legal move set defines the action universe for this case.
    // Legal move set defines the action universe for this case.
    // LINE NOTE: const std::vector<MoveSequence> legal_moves = generate_legal_move_sequences(
    const std::vector<MoveSequence> legal_moves = generate_legal_move_sequences(
        // LINE NOTE: benchmark_case.board,
        benchmark_case.board,
        // LINE NOTE: benchmark_case.board.turn,
        benchmark_case.board.turn,
        // LINE NOTE: benchmark_case.roll);
        benchmark_case.roll);
    // LINE NOTE: const int n_legal_moves = static_cast<int>(legal_moves.size());
    const int n_legal_moves = static_cast<int>(legal_moves.size());

    // LINE NOTE: int reference_choice_index = NA_INTEGER;
    int reference_choice_index = NA_INTEGER;
    // LINE NOTE: if (reference_method.has_value() && n_legal_moves > 0) {
    if (reference_method.has_value() && n_legal_moves > 0) {
      // LINE NOTE: // Compute reference recommendation once per case (not per method row).
      // Compute reference recommendation once per case (not per method row).
      // LINE NOTE: const std::optional<MoveSequence> reference_choice = choose_move_for_benchmark(
      const std::optional<MoveSequence> reference_choice = choose_move_for_benchmark(
          // LINE NOTE: benchmark_case.board,
          benchmark_case.board,
          // LINE NOTE: legal_moves,
          legal_moves,
          // LINE NOTE: benchmark_case.case_id,
          benchmark_case.case_id,
          // LINE NOTE: reference_method.value(),
          reference_method.value(),
          // LINE NOTE: "reference",
          "reference",
          // LINE NOTE: reference_rollout_config,
          reference_rollout_config,
          // LINE NOTE: base_seed);
          base_seed);
      // LINE NOTE: if (reference_choice.has_value()) {
      if (reference_choice.has_value()) {
        // LINE NOTE: reference_choice_index = chosen_index_in_legal_moves(reference_choice.value(), legal_moves);
        reference_choice_index = chosen_index_in_legal_moves(reference_choice.value(), legal_moves);
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: for (const std::string& method : methods) {
    for (const std::string& method : methods) {
      // LINE NOTE: MoveBenchmarkRow row;
      MoveBenchmarkRow row;
      // LINE NOTE: row.case_id = benchmark_case.case_id;
      row.case_id = benchmark_case.case_id;
      // LINE NOTE: row.method = method;
      row.method = method;
      // LINE NOTE: row.n_legal_moves = n_legal_moves;
      row.n_legal_moves = n_legal_moves;
      // LINE NOTE: row.reference_choice_index = reference_choice_index;
      row.reference_choice_index = reference_choice_index;

      // LINE NOTE: if (n_legal_moves == 0) {
      if (n_legal_moves == 0) {
        // LINE NOTE: // Degenerate case: no legal decision to benchmark.
        // Degenerate case: no legal decision to benchmark.
        // LINE NOTE: row.chosen_index = 0;
        row.chosen_index = 0;
        // LINE NOTE: row.match_reference = NA_LOGICAL;
        row.match_reference = NA_LOGICAL;
        // LINE NOTE: row.runtime_seconds = 0.0;
        row.runtime_seconds = 0.0;
        // LINE NOTE: out.rows.push_back(row);
        out.rows.push_back(row);
        // LINE NOTE: continue;
        continue;
      // LINE NOTE: }
      }

      // LINE NOTE: const auto start = Clock::now();
      const auto start = Clock::now();
      // LINE NOTE: // Method under test chooses one move on this case.
      // Method under test chooses one move on this case.
      // LINE NOTE: const std::optional<MoveSequence> chosen = choose_move_for_benchmark(
      const std::optional<MoveSequence> chosen = choose_move_for_benchmark(
          // LINE NOTE: benchmark_case.board,
          benchmark_case.board,
          // LINE NOTE: legal_moves,
          legal_moves,
          // LINE NOTE: benchmark_case.case_id,
          benchmark_case.case_id,
          // LINE NOTE: method,
          method,
          // LINE NOTE: "method",
          "method",
          // LINE NOTE: method_rollout_config,
          method_rollout_config,
          // LINE NOTE: base_seed);
          base_seed);
      // LINE NOTE: const auto end = Clock::now();
      const auto end = Clock::now();

      // LINE NOTE: row.runtime_seconds = elapsed_seconds(start, end);
      row.runtime_seconds = elapsed_seconds(start, end);
      // LINE NOTE: row.chosen_index = chosen.has_value() ? chosen_index_in_legal_moves(chosen.value(), legal_moves) : 0;
      row.chosen_index = chosen.has_value() ? chosen_index_in_legal_moves(chosen.value(), legal_moves) : 0;

      // LINE NOTE: if (!reference_method.has_value() || n_legal_moves <= 1 || reference_choice_index == NA_INTEGER) {
      if (!reference_method.has_value() || n_legal_moves <= 1 || reference_choice_index == NA_INTEGER) {
        // LINE NOTE: // No comparable decision target, so match is undefined.
        // No comparable decision target, so match is undefined.
        // LINE NOTE: row.match_reference = NA_LOGICAL;
        row.match_reference = NA_LOGICAL;
      // LINE NOTE: } else {
      } else {
        // LINE NOTE: // Direct agreement indicator with reference recommendation.
        // Direct agreement indicator with reference recommendation.
        // LINE NOTE: row.match_reference = row.chosen_index == reference_choice_index ? TRUE : FALSE;
        row.match_reference = row.chosen_index == reference_choice_index ? TRUE : FALSE;
      // LINE NOTE: }
      }

      // LINE NOTE: out.rows.push_back(row);
      out.rows.push_back(row);
    // LINE NOTE: }
    }
  // LINE NOTE: }
  }

  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Convert matchup benchmark object to R list and append runtime metadata.
// Convert matchup benchmark object to R list and append runtime metadata.
// LINE NOTE: Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result) {
Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result) {
  // LINE NOTE: Rcpp::List out = matchup_simulation_result_to_list(result.simulation);
  Rcpp::List out = matchup_simulation_result_to_list(result.simulation);
  // LINE NOTE: const Rcpp::DataFrame summary(out["summary"]);
  const Rcpp::DataFrame summary(out["summary"]);
  // LINE NOTE: out["summary"] = add_runtime_columns_to_summary(summary, result.runtime_seconds, result.simulation.n_games);
  out["summary"] = add_runtime_columns_to_summary(summary, result.runtime_seconds, result.simulation.n_games);

  // LINE NOTE: Rcpp::List settings(out["settings"]);
  Rcpp::List settings(out["settings"]);
  // LINE NOTE: settings.push_back(Rcpp::NumericVector::create(result.runtime_seconds), "runtime_seconds");
  settings.push_back(Rcpp::NumericVector::create(result.runtime_seconds), "runtime_seconds");
  // LINE NOTE: out["settings"] = settings;
  out["settings"] = settings;
  // LINE NOTE: return out;
  return out;
// LINE NOTE: }
}

// LINE NOTE: // Convert row-level move benchmark records to R data frame.
// Convert row-level move benchmark records to R data frame.
// LINE NOTE: Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result) {
Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result) {
  // LINE NOTE: const int n = static_cast<int>(result.rows.size());
  const int n = static_cast<int>(result.rows.size());
  // LINE NOTE: Rcpp::CharacterVector case_id(n);
  Rcpp::CharacterVector case_id(n);
  // LINE NOTE: Rcpp::CharacterVector method(n);
  Rcpp::CharacterVector method(n);
  // LINE NOTE: Rcpp::IntegerVector n_legal_moves(n);
  Rcpp::IntegerVector n_legal_moves(n);
  // LINE NOTE: Rcpp::IntegerVector chosen_index(n);
  Rcpp::IntegerVector chosen_index(n);
  // LINE NOTE: Rcpp::IntegerVector reference_choice_index(n);
  Rcpp::IntegerVector reference_choice_index(n);
  // LINE NOTE: Rcpp::LogicalVector match_reference(n);
  Rcpp::LogicalVector match_reference(n);
  // LINE NOTE: Rcpp::NumericVector runtime_seconds(n);
  Rcpp::NumericVector runtime_seconds(n);

  // LINE NOTE: for (int i = 0; i < n; ++i) {
  for (int i = 0; i < n; ++i) {
    // LINE NOTE: case_id[i] = result.rows[i].case_id;
    case_id[i] = result.rows[i].case_id;
    // LINE NOTE: method[i] = result.rows[i].method;
    method[i] = result.rows[i].method;
    // LINE NOTE: n_legal_moves[i] = result.rows[i].n_legal_moves;
    n_legal_moves[i] = result.rows[i].n_legal_moves;
    // LINE NOTE: chosen_index[i] = result.rows[i].chosen_index;
    chosen_index[i] = result.rows[i].chosen_index;
    // LINE NOTE: reference_choice_index[i] = result.rows[i].reference_choice_index;
    reference_choice_index[i] = result.rows[i].reference_choice_index;
    // LINE NOTE: match_reference[i] = result.rows[i].match_reference;
    match_reference[i] = result.rows[i].match_reference;
    // LINE NOTE: runtime_seconds[i] = result.rows[i].runtime_seconds;
    runtime_seconds[i] = result.rows[i].runtime_seconds;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["case_id"] = case_id,
      Rcpp::_["case_id"] = case_id,
      // LINE NOTE: Rcpp::_["method"] = method,
      Rcpp::_["method"] = method,
      // LINE NOTE: Rcpp::_["n_legal_moves"] = n_legal_moves,
      Rcpp::_["n_legal_moves"] = n_legal_moves,
      // LINE NOTE: Rcpp::_["chosen_index"] = chosen_index,
      Rcpp::_["chosen_index"] = chosen_index,
      // LINE NOTE: Rcpp::_["reference_choice_index"] = reference_choice_index,
      Rcpp::_["reference_choice_index"] = reference_choice_index,
      // LINE NOTE: Rcpp::_["match_reference"] = match_reference,
      Rcpp::_["match_reference"] = match_reference,
      // LINE NOTE: Rcpp::_["runtime_seconds"] = runtime_seconds,
      Rcpp::_["runtime_seconds"] = runtime_seconds,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
}

// LINE NOTE: // Aggregate move benchmark rows by method into compact summary metrics.
// Aggregate move benchmark rows by method into compact summary metrics.
// LINE NOTE: Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result) {
Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result) {
  // LINE NOTE: const int n_methods = static_cast<int>(result.methods.size());
  const int n_methods = static_cast<int>(result.methods.size());
  // LINE NOTE: Rcpp::CharacterVector method(n_methods);
  Rcpp::CharacterVector method(n_methods);
  // LINE NOTE: Rcpp::IntegerVector n_cases(n_methods);
  Rcpp::IntegerVector n_cases(n_methods);
  // LINE NOTE: Rcpp::IntegerVector decision_cases(n_methods);
  Rcpp::IntegerVector decision_cases(n_methods);
  // LINE NOTE: Rcpp::NumericVector mean_n_legal_moves(n_methods);
  Rcpp::NumericVector mean_n_legal_moves(n_methods);
  // LINE NOTE: Rcpp::NumericVector total_runtime_seconds(n_methods);
  Rcpp::NumericVector total_runtime_seconds(n_methods);
  // LINE NOTE: Rcpp::NumericVector mean_runtime_seconds(n_methods);
  Rcpp::NumericVector mean_runtime_seconds(n_methods);
  // LINE NOTE: Rcpp::NumericVector best_move_match_rate(n_methods);
  Rcpp::NumericVector best_move_match_rate(n_methods);

  // LINE NOTE: for (int i = 0; i < n_methods; ++i) {
  for (int i = 0; i < n_methods; ++i) {
    // LINE NOTE: const std::string& current_method = result.methods[i];
    const std::string& current_method = result.methods[i];
    // LINE NOTE: method[i] = current_method;
    method[i] = current_method;

    // LINE NOTE: int rows_for_method = 0;
    int rows_for_method = 0;
    // LINE NOTE: int comparable_rows = 0;
    int comparable_rows = 0;
    // LINE NOTE: double total_legal_moves = 0.0;
    double total_legal_moves = 0.0;
    // LINE NOTE: double total_runtime = 0.0;
    double total_runtime = 0.0;
    // LINE NOTE: int reference_matches = 0;
    int reference_matches = 0;

    // LINE NOTE: for (const MoveBenchmarkRow& row : result.rows) {
    for (const MoveBenchmarkRow& row : result.rows) {
      // LINE NOTE: if (row.method != current_method) {
      if (row.method != current_method) {
        // LINE NOTE: continue;
        continue;
      // LINE NOTE: }
      }

      // LINE NOTE: ++rows_for_method;
      ++rows_for_method;
      // LINE NOTE: total_legal_moves += static_cast<double>(row.n_legal_moves);
      total_legal_moves += static_cast<double>(row.n_legal_moves);
      // LINE NOTE: total_runtime += row.runtime_seconds;
      total_runtime += row.runtime_seconds;

      // LINE NOTE: if (row.match_reference == TRUE || row.match_reference == FALSE) {
      if (row.match_reference == TRUE || row.match_reference == FALSE) {
        // LINE NOTE: // Only rows with defined match_reference contribute to agreement rate.
        // Only rows with defined match_reference contribute to agreement rate.
        // LINE NOTE: ++comparable_rows;
        ++comparable_rows;
        // LINE NOTE: if (row.match_reference == TRUE) {
        if (row.match_reference == TRUE) {
          // LINE NOTE: ++reference_matches;
          ++reference_matches;
        // LINE NOTE: }
        }
      // LINE NOTE: }
      }
    // LINE NOTE: }
    }

    // LINE NOTE: n_cases[i] = rows_for_method;
    n_cases[i] = rows_for_method;
    // LINE NOTE: decision_cases[i] = comparable_rows;
    decision_cases[i] = comparable_rows;
    // LINE NOTE: mean_n_legal_moves[i] = rows_for_method > 0 ? total_legal_moves / static_cast<double>(rows_for_method) : NA_REAL;
    mean_n_legal_moves[i] = rows_for_method > 0 ? total_legal_moves / static_cast<double>(rows_for_method) : NA_REAL;
    // LINE NOTE: total_runtime_seconds[i] = total_runtime;
    total_runtime_seconds[i] = total_runtime;
    // LINE NOTE: mean_runtime_seconds[i] = rows_for_method > 0 ? total_runtime / static_cast<double>(rows_for_method) : NA_REAL;
    mean_runtime_seconds[i] = rows_for_method > 0 ? total_runtime / static_cast<double>(rows_for_method) : NA_REAL;
    // LINE NOTE: best_move_match_rate[i] = comparable_rows > 0
    best_move_match_rate[i] = comparable_rows > 0
        // LINE NOTE: ? static_cast<double>(reference_matches) / static_cast<double>(comparable_rows)
        ? static_cast<double>(reference_matches) / static_cast<double>(comparable_rows)
        // LINE NOTE: : NA_REAL;
        : NA_REAL;
  // LINE NOTE: }
  }

  // LINE NOTE: return Rcpp::DataFrame::create(
  return Rcpp::DataFrame::create(
      // LINE NOTE: Rcpp::_["method"] = method,
      Rcpp::_["method"] = method,
      // LINE NOTE: Rcpp::_["n_cases"] = n_cases,
      Rcpp::_["n_cases"] = n_cases,
      // LINE NOTE: Rcpp::_["decision_cases"] = decision_cases,
      Rcpp::_["decision_cases"] = decision_cases,
      // LINE NOTE: Rcpp::_["mean_n_legal_moves"] = mean_n_legal_moves,
      Rcpp::_["mean_n_legal_moves"] = mean_n_legal_moves,
      // LINE NOTE: Rcpp::_["total_runtime_seconds"] = total_runtime_seconds,
      Rcpp::_["total_runtime_seconds"] = total_runtime_seconds,
      // LINE NOTE: Rcpp::_["mean_runtime_seconds"] = mean_runtime_seconds,
      Rcpp::_["mean_runtime_seconds"] = mean_runtime_seconds,
      // LINE NOTE: Rcpp::_["best_move_match_rate"] = best_move_match_rate,
      Rcpp::_["best_move_match_rate"] = best_move_match_rate,
      // LINE NOTE: Rcpp::_["stringsAsFactors"] = false);
      Rcpp::_["stringsAsFactors"] = false);
// LINE NOTE: }
}

// LINE NOTE: // Package row-level + summary-level benchmark outputs into one R list.
// Package row-level + summary-level benchmark outputs into one R list.
// LINE NOTE: Rcpp::List move_benchmark_result_to_list(
Rcpp::List move_benchmark_result_to_list(
    // LINE NOTE: const MoveBenchmarkResult& result,
    const MoveBenchmarkResult& result,
    // LINE NOTE: const RolloutConfig& method_rollout_config,
    const RolloutConfig& method_rollout_config,
    // LINE NOTE: const RolloutConfig& reference_rollout_config) {
    const RolloutConfig& reference_rollout_config) {
  // LINE NOTE: return Rcpp::List::create(
  return Rcpp::List::create(
      // LINE NOTE: Rcpp::_["results"] = move_benchmark_rows_to_data_frame(result),
      Rcpp::_["results"] = move_benchmark_rows_to_data_frame(result),
      // LINE NOTE: Rcpp::_["summary"] = move_benchmark_summary_to_data_frame(result),
      Rcpp::_["summary"] = move_benchmark_summary_to_data_frame(result),
      // LINE NOTE: Rcpp::_["settings"] = Rcpp::List::create(
      Rcpp::_["settings"] = Rcpp::List::create(
          // LINE NOTE: Rcpp::_["methods"] = Rcpp::wrap(result.methods),
          Rcpp::_["methods"] = Rcpp::wrap(result.methods),
          // LINE NOTE: Rcpp::_["reference_method"] = result.reference_method.has_value()
          Rcpp::_["reference_method"] = result.reference_method.has_value()
              // LINE NOTE: ? Rcpp::CharacterVector::create(result.reference_method.value())
              ? Rcpp::CharacterVector::create(result.reference_method.value())
              // LINE NOTE: : Rcpp::CharacterVector::create(NA_STRING),
              : Rcpp::CharacterVector::create(NA_STRING),
          // LINE NOTE: Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(method_rollout_config.budget),
          Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(method_rollout_config.budget),
          // LINE NOTE: Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(method_rollout_config.policy),
          Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(method_rollout_config.policy),
          // LINE NOTE: Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(method_rollout_config.max_turns),
          Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(method_rollout_config.max_turns),
          // LINE NOTE: Rcpp::_["reference_rollout_budget"] = Rcpp::IntegerVector::create(reference_rollout_config.budget),
          Rcpp::_["reference_rollout_budget"] = Rcpp::IntegerVector::create(reference_rollout_config.budget),
          // LINE NOTE: Rcpp::_["reference_rollout_policy"] = Rcpp::CharacterVector::create(reference_rollout_config.policy),
          Rcpp::_["reference_rollout_policy"] = Rcpp::CharacterVector::create(reference_rollout_config.policy),
          // LINE NOTE: Rcpp::_["reference_max_rollout_turns"] = Rcpp::IntegerVector::create(reference_rollout_config.max_turns)));
          Rcpp::_["reference_max_rollout_turns"] = Rcpp::IntegerVector::create(reference_rollout_config.max_turns)));
// LINE NOTE: }
}

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_benchmark_matchup_random(
Rcpp::List bg_cpp_benchmark_matchup_random(
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
  // LINE NOTE: // Parse input state and initialize one RNG stream for this benchmark call.
  // Parse input state and initialize one RNG stream for this benchmark call.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);
  // LINE NOTE: const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  // LINE NOTE: return backgammonr::matchup_benchmark_result_to_list(
  return backgammonr::matchup_benchmark_result_to_list(
      // LINE NOTE: backgammonr::benchmark_matchup_random(
      backgammonr::benchmark_matchup_random(
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
// LINE NOTE: Rcpp::List bg_cpp_benchmark_matchup_scripted(
Rcpp::List bg_cpp_benchmark_matchup_scripted(
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
  // LINE NOTE: // Parse state + scripted dice sequence.
  // Parse state + scripted dice sequence.
  // LINE NOTE: const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  // LINE NOTE: const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  // LINE NOTE: const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  // LINE NOTE: std::mt19937 rng;
  std::mt19937 rng;
  // LINE NOTE: std::mt19937* rng_ptr = nullptr;
  std::mt19937* rng_ptr = nullptr;
  // LINE NOTE: // Only allocate RNG when at least one player policy is stochastic.
  // Only allocate RNG when at least one player policy is stochastic.
  // LINE NOTE: if (backgammonr::selection_uses_randomness(player1_selection) ||
  if (backgammonr::selection_uses_randomness(player1_selection) ||
      // LINE NOTE: backgammonr::selection_uses_randomness(player2_selection)) {
      backgammonr::selection_uses_randomness(player2_selection)) {
    // LINE NOTE: rng = init_rng(seed, use_seed);
    rng = init_rng(seed, use_seed);
    // LINE NOTE: rng_ptr = &rng;
    rng_ptr = &rng;
  // LINE NOTE: }
  }

  // LINE NOTE: return backgammonr::matchup_benchmark_result_to_list(
  return backgammonr::matchup_benchmark_result_to_list(
      // LINE NOTE: backgammonr::benchmark_matchup_with_rolls(
      backgammonr::benchmark_matchup_with_rolls(
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
          // LINE NOTE: rollout_config));
          rollout_config));
// LINE NOTE: }
}

// LINE NOTE: // [[Rcpp::export]]
// [[Rcpp::export]]
// LINE NOTE: Rcpp::List bg_cpp_benchmark_move_evaluators(
Rcpp::List bg_cpp_benchmark_move_evaluators(
    // LINE NOTE: const Rcpp::List& cases,
    const Rcpp::List& cases,
    // LINE NOTE: const std::vector<std::string>& methods,
    const std::vector<std::string>& methods,
    // LINE NOTE: const std::string& reference_method,
    const std::string& reference_method,
    // LINE NOTE: const int rollout_budget,
    const int rollout_budget,
    // LINE NOTE: const std::string& rollout_policy,
    const std::string& rollout_policy,
    // LINE NOTE: const int max_rollout_turns,
    const int max_rollout_turns,
    // LINE NOTE: const int reference_rollout_budget,
    const int reference_rollout_budget,
    // LINE NOTE: const std::string& reference_rollout_policy,
    const std::string& reference_rollout_policy,
    // LINE NOTE: const int reference_max_rollout_turns,
    const int reference_max_rollout_turns,
    // LINE NOTE: const int seed,
    const int seed,
    // LINE NOTE: const bool use_seed) {
    const bool use_seed) {
  // LINE NOTE: // Parse case set and configs once; benchmark core consumes typed structures.
  // Parse case set and configs once; benchmark core consumes typed structures.
  // LINE NOTE: const std::vector<backgammonr::MoveBenchmarkCase> parsed_cases = backgammonr::parse_move_benchmark_cases(cases);
  const std::vector<backgammonr::MoveBenchmarkCase> parsed_cases = backgammonr::parse_move_benchmark_cases(cases);
  // LINE NOTE: const backgammonr::RolloutConfig method_rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig method_rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  // LINE NOTE: const backgammonr::RolloutConfig reference_rollout_config{
  const backgammonr::RolloutConfig reference_rollout_config{
      // LINE NOTE: reference_rollout_budget,
      reference_rollout_budget,
      // LINE NOTE: reference_rollout_policy,
      reference_rollout_policy,
      // LINE NOTE: reference_max_rollout_turns};
      reference_max_rollout_turns};
  // LINE NOTE: std::mt19937 rng = init_rng(seed, use_seed);
  std::mt19937 rng = init_rng(seed, use_seed);

  // LINE NOTE: const std::optional<std::string> parsed_reference_method =
  const std::optional<std::string> parsed_reference_method =
      // LINE NOTE: reference_method.empty() ? std::nullopt : std::optional<std::string>(reference_method);
      reference_method.empty() ? std::nullopt : std::optional<std::string>(reference_method);

  // LINE NOTE: // Run benchmark and return row/summary/settings bundle for R analysis.
  // Run benchmark and return row/summary/settings bundle for R analysis.
  // LINE NOTE: return backgammonr::move_benchmark_result_to_list(
  return backgammonr::move_benchmark_result_to_list(
      // LINE NOTE: backgammonr::benchmark_move_evaluators(
      backgammonr::benchmark_move_evaluators(
          // LINE NOTE: parsed_cases,
          parsed_cases,
          // LINE NOTE: methods,
          methods,
          // LINE NOTE: parsed_reference_method,
          parsed_reference_method,
          // LINE NOTE: method_rollout_config,
          method_rollout_config,
          // LINE NOTE: reference_rollout_config,
          reference_rollout_config,
          // LINE NOTE: rng),
          rng),
      // LINE NOTE: method_rollout_config,
      method_rollout_config,
      // LINE NOTE: reference_rollout_config);
      reference_rollout_config);
// LINE NOTE: }
}
