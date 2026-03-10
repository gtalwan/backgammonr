#include "bg_benchmark.h"
// This translation unit contains benchmark-oriented orchestration for the
// rollout/statistical APIs. The game engine produces stochastic outcomes;
// this file standardizes how we compare methods on shared cases and collect
// reproducible runtime + decision-quality summaries.

#include <Rcpp.h>

#include <chrono>
#include <cstdint>
#include <functional>
#include <optional>
#include <random>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include "bg_game.h"
#include "bg_move.h"
#include "bg_movegen.h"

namespace {

using Clock = std::chrono::steady_clock;

// Function: init_rng
// Purpose: Build RNG stream for benchmark wrappers.
// Called by: all exported benchmark entry points in this file.
// Notes: Determinism matters for method-vs-method reproducibility.
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
}

// Function: stable_stream_seed
// Purpose: Derive deterministic child stream seeds from case/method labels.
// Called by: choose_move_for_benchmark().
std::uint32_t stable_stream_seed(
    const std::uint32_t base_seed,
    const std::string& case_id,
    const std::string& method,
    const std::string& stream_label) {
  const std::size_t hashed = std::hash<std::string>{}(case_id + "::" + method + "::" + stream_label);
  return static_cast<std::uint32_t>(hashed ^ static_cast<std::size_t>(base_seed));
}

// Function: elapsed_seconds
// Purpose: Return elapsed wall-clock seconds for benchmark timing.
// Called by: benchmark_matchup_random(), benchmark_matchup_with_rolls(),
// benchmark_move_evaluators().
double elapsed_seconds(
    const std::chrono::time_point<Clock>& start,
    const std::chrono::time_point<Clock>& end) {
  return std::chrono::duration_cast<std::chrono::duration<double>>(end - start).count();
}

// Function: parse_roll_vector
// Purpose: Parse R list of scripted rolls into DiceRoll vector.
// Called by: bg_cpp_benchmark_matchup_scripted().
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

// Function: has_named_element
// Purpose: Convenience helper for optional case-list fields.
// Called by: parse_case_id(), parse_move_benchmark_cases().
bool has_named_element(const Rcpp::List& x, const char* name) {
  return x.containsElementNamed(name);
}

// Function: default_case_id
// Purpose: Generate deterministic fallback case ID when none is supplied.
// Called by: parse_case_id().
std::string default_case_id(const int index) {
  std::ostringstream oss;
  oss << "case_" << index;
  return oss.str();
}

// Function: parse_case_id
// Purpose: Parse optional user-supplied case_id with strict validation.
// Called by: parse_move_benchmark_cases().
std::string parse_case_id(const Rcpp::List& case_list, const int index) {
  if (!has_named_element(case_list, "case_id")) {
    return default_case_id(index);
  }

  SEXP raw = case_list["case_id"];
  if (raw == R_NilValue) {
    return default_case_id(index);
  }

  if (TYPEOF(raw) != STRSXP) {
    throw std::range_error("`case_id` must be a character scalar when supplied.");
  }

  Rcpp::CharacterVector case_id(raw);
  if (case_id.size() != 1 || case_id[0] == NA_STRING) {
    throw std::range_error("`case_id` must be a non-missing character scalar when supplied.");
  }

  const std::string parsed = Rcpp::as<std::string>(case_id[0]);
  if (parsed.empty()) {
    return default_case_id(index);
  }

  return parsed;
}

// Function: chosen_index_in_legal_moves
// Purpose: Convert chosen MoveSequence to 1-based index in legal move vector.
// Called by: benchmark_move_evaluators().
int chosen_index_in_legal_moves(
    const backgammonr::MoveSequence& chosen,
    const std::vector<backgammonr::MoveSequence>& legal_moves) {
  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    if (backgammonr::move_sequences_equal(chosen, legal_moves[i])) {
      return i + 1;
    }
  }

  throw std::range_error("Internal error: chosen move was not found in the legal-move set.");
}

// Function: choose_move_for_benchmark
// Purpose: Execute one method decision on one benchmark case.
// Called by: benchmark_move_evaluators().
// Notes: Uses case/method-specific child RNG to avoid cross-method coupling.
std::optional<backgammonr::MoveSequence> choose_move_for_benchmark(
    const backgammonr::BoardState& board,
    const std::vector<backgammonr::MoveSequence>& legal_moves,
    const std::string& case_id,
    const std::string& method,
    const std::string& stream_label,
    const backgammonr::RolloutConfig& rollout_config,
    const std::uint32_t base_seed) {
  if (legal_moves.empty()) {
    return std::nullopt;
  }

  std::mt19937* rng_ptr = nullptr;
  std::mt19937 child_rng;
  if (backgammonr::selection_uses_randomness(method)) {
    child_rng.seed(stable_stream_seed(base_seed, case_id, method, stream_label));
    rng_ptr = &child_rng;
  }

  return backgammonr::choose_move_sequence(board, legal_moves, method, rng_ptr, rollout_config);
}

// Function: add_runtime_columns_to_summary
// Purpose: Append runtime/throughput columns to matchup summary table.
// Called by: matchup_benchmark_result_to_list().
Rcpp::DataFrame add_runtime_columns_to_summary(const Rcpp::DataFrame& summary, const double runtime_seconds, const int n_games) {
  Rcpp::List out(summary);
  out.push_back(Rcpp::NumericVector::create(runtime_seconds), "runtime_seconds");
  out.push_back(
      Rcpp::NumericVector::create(n_games > 0 ? runtime_seconds / static_cast<double>(n_games) : NA_REAL),
      "seconds_per_game");
  out.push_back(
      Rcpp::NumericVector::create(runtime_seconds > 0.0 ? static_cast<double>(n_games) / runtime_seconds : NA_REAL),
      "games_per_second");
  out.attr("class") = summary.attr("class");
  out.attr("row.names") = summary.attr("row.names");
  return Rcpp::DataFrame(out);
}

}  // namespace

namespace backgammonr {

// Function: benchmark_matchup_random
// Purpose: Time and run random-dice matchup simulation.
// Called by: bg_cpp_benchmark_matchup_random().
MatchupBenchmarkResult benchmark_matchup_random(
    const BoardState& initial_board,
    const int n_games,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config) {
  const auto start = Clock::now();
  const MatchupSimulationResult simulation = simulate_matchup_random(
      initial_board,
      n_games,
      max_turns,
      rng,
      player1_selection,
      player2_selection,
      rollout_config);
  const auto end = Clock::now();

  MatchupBenchmarkResult out;
  out.simulation = simulation;
  out.runtime_seconds = elapsed_seconds(start, end);
  return out;
}

// Function: benchmark_matchup_with_rolls
// Purpose: Time and run scripted-dice matchup simulation.
// Called by: bg_cpp_benchmark_matchup_scripted().
MatchupBenchmarkResult benchmark_matchup_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    const int n_games,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  const auto start = Clock::now();
  const MatchupSimulationResult simulation = simulate_matchup_with_rolls(
      initial_board,
      rolls,
      n_games,
      max_turns,
      player1_selection,
      player2_selection,
      rng,
      rollout_config);
  const auto end = Clock::now();

  MatchupBenchmarkResult out;
  out.simulation = simulation;
  out.runtime_seconds = elapsed_seconds(start, end);
  return out;
}

// Function: parse_move_benchmark_cases
// Purpose: Parse and validate benchmark case list from R.
// Called by: bg_cpp_benchmark_move_evaluators().
std::vector<MoveBenchmarkCase> parse_move_benchmark_cases(const Rcpp::List& cases) {
  std::vector<MoveBenchmarkCase> out;
  out.reserve(cases.size());

  for (int i = 0; i < cases.size(); ++i) {
    SEXP case_sexp = cases[i];
    if (!Rf_isNewList(case_sexp)) {
      std::ostringstream oss;
      oss << "`cases[[" << (i + 1) << "]]` must be a list-like benchmark case.";
      throw std::range_error(oss.str());
    }

    const Rcpp::List case_list(case_sexp);
    if (!has_named_element(case_list, "board")) {
      throw std::range_error("Each benchmark case must contain a `board` field.");
    }
    if (!has_named_element(case_list, "roll")) {
      throw std::range_error("Each benchmark case must contain a `roll` field.");
    }

    SEXP board_raw = case_list["board"];
    SEXP roll_raw = case_list["roll"];
    if (!Rf_isNewList(board_raw)) {
      throw std::range_error("Each `board` field in `cases` must be a board-like list.");
    }
    if (!Rf_isNewList(roll_raw)) {
      throw std::range_error("Each `roll` field in `cases` must be a roll-like list.");
    }

    MoveBenchmarkCase parsed;
    parsed.case_id = parse_case_id(case_list, i + 1);
    parsed.board = parse_board_list(Rcpp::List(board_raw));
    parsed.roll = parse_roll_list(Rcpp::List(roll_raw));
    out.push_back(parsed);
  }

  return out;
}

// Function: benchmark_move_evaluators
// Purpose: Core move-choice benchmark across cases and methods.
// Called by: bg_cpp_benchmark_move_evaluators().
// Notes: Optional reference method is evaluated once per case and reused across
// method rows to keep agreement comparisons fair.
MoveBenchmarkResult benchmark_move_evaluators(
    const std::vector<MoveBenchmarkCase>& cases,
    const std::vector<std::string>& methods,
    const std::optional<std::string>& reference_method,
    const RolloutConfig& method_rollout_config,
    const RolloutConfig& reference_rollout_config,
    std::mt19937& rng) {
  validate_rollout_config(method_rollout_config);
  validate_rollout_config(reference_rollout_config);

  if (cases.empty()) {
    throw std::range_error("`cases` must contain at least one benchmark case.");
  }
  if (methods.empty()) {
    throw std::range_error("`methods` must contain at least one selection method.");
  }

  for (const std::string& method : methods) {
    validate_selection(method);
  }
  if (reference_method.has_value()) {
    validate_selection(reference_method.value());
  }

  MoveBenchmarkResult out;
  out.methods = methods;
  out.reference_method = reference_method;
  out.rows.reserve(static_cast<int>(cases.size()) * static_cast<int>(methods.size()));
  const std::uint32_t base_seed = static_cast<std::uint32_t>(rng());

  for (const MoveBenchmarkCase& benchmark_case : cases) {
    // For this case, all methods see the exact same legal action set.
    // That keeps the benchmark focused on allocation/selection behavior rather
    // than differences in action availability.
    // Legal move set defines the action universe for this case.
    const std::vector<MoveSequence> legal_moves = generate_legal_move_sequences(
        benchmark_case.board,
        benchmark_case.board.turn,
        benchmark_case.roll);
    const int n_legal_moves = static_cast<int>(legal_moves.size());

    int reference_choice_index = NA_INTEGER;
    if (reference_method.has_value() && n_legal_moves > 0) {
      // We intentionally compute this once here so every method row below is
      // compared against the same reference decision.
      // Compute reference recommendation once per case (not per method row).
      const std::optional<MoveSequence> reference_choice = choose_move_for_benchmark(
          benchmark_case.board,
          legal_moves,
          benchmark_case.case_id,
          reference_method.value(),
          "reference",
          reference_rollout_config,
          base_seed);
      if (reference_choice.has_value()) {
        reference_choice_index = chosen_index_in_legal_moves(reference_choice.value(), legal_moves);
      }
    }

    for (const std::string& method : methods) {
      MoveBenchmarkRow row;
      row.case_id = benchmark_case.case_id;
      row.method = method;
      row.n_legal_moves = n_legal_moves;
      row.reference_choice_index = reference_choice_index;

      if (n_legal_moves == 0) {
        // Degenerate case: no legal decision to benchmark.
        row.chosen_index = 0;
        row.match_reference = NA_LOGICAL;
        row.runtime_seconds = 0.0;
        out.rows.push_back(row);
        continue;
      }

      const auto start = Clock::now();
      // Runtime is measured around only the method-under-test decision call.
      // This makes per-method timing numbers interpretable.
      // Method under test chooses one move on this case.
      const std::optional<MoveSequence> chosen = choose_move_for_benchmark(
          benchmark_case.board,
          legal_moves,
          benchmark_case.case_id,
          method,
          "method",
          method_rollout_config,
          base_seed);
      const auto end = Clock::now();

      row.runtime_seconds = elapsed_seconds(start, end);
      row.chosen_index = chosen.has_value() ? chosen_index_in_legal_moves(chosen.value(), legal_moves) : 0;

      if (!reference_method.has_value() || n_legal_moves <= 1 || reference_choice_index == NA_INTEGER) {
        // Agreement metric is undefined if there is no valid reference target
        // or no meaningful choice set to compare.
        // No comparable decision target, so match is undefined.
        row.match_reference = NA_LOGICAL;
      } else {
        // Direct agreement indicator with reference recommendation.
        row.match_reference = row.chosen_index == reference_choice_index ? TRUE : FALSE;
      }

      out.rows.push_back(row);
    }
  }

  return out;
}

// Function: matchup_benchmark_result_to_list
// Purpose: Convert matchup benchmark result to R list and append runtime fields.
// Called by: bg_cpp_benchmark_matchup_random(),
// bg_cpp_benchmark_matchup_scripted().
Rcpp::List matchup_benchmark_result_to_list(const MatchupBenchmarkResult& result) {
  Rcpp::List out = matchup_simulation_result_to_list(result.simulation);
  const Rcpp::DataFrame summary(out["summary"]);
  out["summary"] = add_runtime_columns_to_summary(summary, result.runtime_seconds, result.simulation.n_games);

  Rcpp::List settings(out["settings"]);
  settings.push_back(Rcpp::NumericVector::create(result.runtime_seconds), "runtime_seconds");
  out["settings"] = settings;
  return out;
}

// Function: move_benchmark_rows_to_data_frame
// Purpose: Convert row-level move benchmark records to R data frame.
// Called by: move_benchmark_result_to_list().
Rcpp::DataFrame move_benchmark_rows_to_data_frame(const MoveBenchmarkResult& result) {
  const int n = static_cast<int>(result.rows.size());
  Rcpp::CharacterVector case_id(n);
  Rcpp::CharacterVector method(n);
  Rcpp::IntegerVector n_legal_moves(n);
  Rcpp::IntegerVector chosen_index(n);
  Rcpp::IntegerVector reference_choice_index(n);
  Rcpp::LogicalVector match_reference(n);
  Rcpp::NumericVector runtime_seconds(n);

  for (int i = 0; i < n; ++i) {
    case_id[i] = result.rows[i].case_id;
    method[i] = result.rows[i].method;
    n_legal_moves[i] = result.rows[i].n_legal_moves;
    chosen_index[i] = result.rows[i].chosen_index;
    reference_choice_index[i] = result.rows[i].reference_choice_index;
    match_reference[i] = result.rows[i].match_reference;
    runtime_seconds[i] = result.rows[i].runtime_seconds;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["case_id"] = case_id,
      Rcpp::_["method"] = method,
      Rcpp::_["n_legal_moves"] = n_legal_moves,
      Rcpp::_["chosen_index"] = chosen_index,
      Rcpp::_["reference_choice_index"] = reference_choice_index,
      Rcpp::_["match_reference"] = match_reference,
      Rcpp::_["runtime_seconds"] = runtime_seconds,
      Rcpp::_["stringsAsFactors"] = false);
}

// Function: move_benchmark_summary_to_data_frame
// Purpose: Aggregate move benchmark rows by method into summary metrics.
// Called by: move_benchmark_result_to_list().
// Notes: Inner loop intentionally scans all rows to produce method-level
// totals, runtimes, and reference-agreement rates.
Rcpp::DataFrame move_benchmark_summary_to_data_frame(const MoveBenchmarkResult& result) {
  const int n_methods = static_cast<int>(result.methods.size());
  Rcpp::CharacterVector method(n_methods);
  Rcpp::IntegerVector n_cases(n_methods);
  Rcpp::IntegerVector decision_cases(n_methods);
  Rcpp::NumericVector mean_n_legal_moves(n_methods);
  Rcpp::NumericVector total_runtime_seconds(n_methods);
  Rcpp::NumericVector mean_runtime_seconds(n_methods);
  Rcpp::NumericVector best_move_match_rate(n_methods);

  for (int i = 0; i < n_methods; ++i) {
    const std::string& current_method = result.methods[i];
    method[i] = current_method;

    int rows_for_method = 0;
    int comparable_rows = 0;
    double total_legal_moves = 0.0;
    double total_runtime = 0.0;
    int reference_matches = 0;

    for (const MoveBenchmarkRow& row : result.rows) {
      if (row.method != current_method) {
        continue;
      }

      ++rows_for_method;
      total_legal_moves += static_cast<double>(row.n_legal_moves);
      total_runtime += row.runtime_seconds;

      if (row.match_reference == TRUE || row.match_reference == FALSE) {
        // Only rows with defined match_reference contribute to agreement rate.
        ++comparable_rows;
        if (row.match_reference == TRUE) {
          ++reference_matches;
        }
      }
    }

    n_cases[i] = rows_for_method;
    decision_cases[i] = comparable_rows;
    mean_n_legal_moves[i] = rows_for_method > 0 ? total_legal_moves / static_cast<double>(rows_for_method) : NA_REAL;
    total_runtime_seconds[i] = total_runtime;
    mean_runtime_seconds[i] = rows_for_method > 0 ? total_runtime / static_cast<double>(rows_for_method) : NA_REAL;
    best_move_match_rate[i] = comparable_rows > 0
        ? static_cast<double>(reference_matches) / static_cast<double>(comparable_rows)
        : NA_REAL;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["method"] = method,
      Rcpp::_["n_cases"] = n_cases,
      Rcpp::_["decision_cases"] = decision_cases,
      Rcpp::_["mean_n_legal_moves"] = mean_n_legal_moves,
      Rcpp::_["total_runtime_seconds"] = total_runtime_seconds,
      Rcpp::_["mean_runtime_seconds"] = mean_runtime_seconds,
      Rcpp::_["best_move_match_rate"] = best_move_match_rate,
      Rcpp::_["stringsAsFactors"] = false);
}

// Function: move_benchmark_result_to_list
// Purpose: Bundle row-level and summary-level move benchmark outputs for R.
// Called by: bg_cpp_benchmark_move_evaluators().
Rcpp::List move_benchmark_result_to_list(
    const MoveBenchmarkResult& result,
    const RolloutConfig& method_rollout_config,
    const RolloutConfig& reference_rollout_config) {
  return Rcpp::List::create(
      Rcpp::_["results"] = move_benchmark_rows_to_data_frame(result),
      Rcpp::_["summary"] = move_benchmark_summary_to_data_frame(result),
      Rcpp::_["settings"] = Rcpp::List::create(
          Rcpp::_["methods"] = Rcpp::wrap(result.methods),
          Rcpp::_["reference_method"] = result.reference_method.has_value()
              ? Rcpp::CharacterVector::create(result.reference_method.value())
              : Rcpp::CharacterVector::create(NA_STRING),
          Rcpp::_["rollout_budget"] = Rcpp::IntegerVector::create(method_rollout_config.budget),
          Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(method_rollout_config.policy),
          Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(method_rollout_config.max_turns),
          Rcpp::_["reference_rollout_budget"] = Rcpp::IntegerVector::create(reference_rollout_config.budget),
          Rcpp::_["reference_rollout_policy"] = Rcpp::CharacterVector::create(reference_rollout_config.policy),
          Rcpp::_["reference_max_rollout_turns"] = Rcpp::IntegerVector::create(reference_rollout_config.max_turns)));
}

}  // namespace backgammonr

// [[Rcpp::export]]
// Function: bg_cpp_benchmark_matchup_random
// Purpose: Rcpp entry point for random-dice matchup benchmarking.
// Called by: R benchmark wrapper in R/benchmarking.R.
Rcpp::List bg_cpp_benchmark_matchup_random(
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
  // Parse input state and initialize one RNG stream for this benchmark call.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::matchup_benchmark_result_to_list(
      backgammonr::benchmark_matchup_random(
          parsed_board,
          n_games,
          max_turns,
          rng,
          player1_selection,
          player2_selection,
          rollout_config));
}

// [[Rcpp::export]]
// Function: bg_cpp_benchmark_matchup_scripted
// Purpose: Rcpp entry point for scripted-dice matchup benchmarking.
// Called by: R benchmark wrapper in R/benchmarking.R.
Rcpp::List bg_cpp_benchmark_matchup_scripted(
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
  // Parse state + scripted dice sequence.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  // Only allocate RNG when at least one player policy is stochastic.
  if (backgammonr::selection_uses_randomness(player1_selection) ||
      backgammonr::selection_uses_randomness(player2_selection)) {
    rng = init_rng(seed, use_seed);
    rng_ptr = &rng;
  }

  return backgammonr::matchup_benchmark_result_to_list(
      backgammonr::benchmark_matchup_with_rolls(
          parsed_board,
          parsed_rolls,
          n_games,
          max_turns,
          player1_selection,
          player2_selection,
          rng_ptr,
          rollout_config));
}

// [[Rcpp::export]]
// Function: bg_cpp_benchmark_move_evaluators
// Purpose: Rcpp entry point for case-by-case move benchmark across methods.
// Called by: benchmark_move_evaluators() R wrapper in R/benchmarking.R.
Rcpp::List bg_cpp_benchmark_move_evaluators(
    const Rcpp::List& cases,
    const std::vector<std::string>& methods,
    const std::string& reference_method,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int reference_rollout_budget,
    const std::string& reference_rollout_policy,
    const int reference_max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse case set and configs once; benchmark core consumes typed structures.
  const std::vector<backgammonr::MoveBenchmarkCase> parsed_cases = backgammonr::parse_move_benchmark_cases(cases);
  const backgammonr::RolloutConfig method_rollout_config{rollout_budget, rollout_policy, max_rollout_turns};
  const backgammonr::RolloutConfig reference_rollout_config{
      reference_rollout_budget,
      reference_rollout_policy,
      reference_max_rollout_turns};
  std::mt19937 rng = init_rng(seed, use_seed);

  const std::optional<std::string> parsed_reference_method =
      reference_method.empty() ? std::nullopt : std::optional<std::string>(reference_method);

  // Run benchmark and return row/summary/settings bundle for R analysis.
  return backgammonr::move_benchmark_result_to_list(
      backgammonr::benchmark_move_evaluators(
          parsed_cases,
          methods,
          parsed_reference_method,
          method_rollout_config,
          reference_rollout_config,
          rng),
      method_rollout_config,
      reference_rollout_config);
}
