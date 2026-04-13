// Rcpp entry points for allocation evaluation and profiling.
//
// These wrappers are intentionally thin: they parse R objects once, build one
// RolloutConfig, and then hand control to the shared native allocation engine.

#include "alloc_core.h"

#include <chrono>
#include <stdexcept>
#include <vector>

#include "alloc_trace.h"
#include "bg_movegen.h"
#include "bg_rng.h"

// [[Rcpp::export]]
Rcpp::List bg_cpp_allocation_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& method,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const double unresolved_value,
    const int initial_allocations,
    const double ucb_exploration,
    const double prior_alpha,
    const double prior_beta,
    const std::string& dice_mode,
    const bool crn,
    const bool fast_diagnostics,
    const int seed,
    const bool use_seed) {
  // The stable evaluation entry point for fixed-budget method comparison
  // without per-step trace output.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{
      total_budget,
      rollout_policy,
      max_rollout_turns,
      ucb_exploration,
      prior_alpha,
      prior_beta,
      initial_allocations,
      unresolved_value,
      dice_mode,
      crn,
      seed,
      use_seed,
      fast_diagnostics};
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          method,
          config,
          rng);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_index = summaries[best_summary_index].candidate_index;

  return Rcpp::List::create(
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          backgammonr::canonicalize_allocation_method(method)),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_allocation_evaluate_trace(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& method,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const double unresolved_value,
    const int initial_allocations,
    const double ucb_exploration,
    const double prior_alpha,
    const double prior_beta,
    const std::string& dice_mode,
    const bool crn,
    const bool fast_diagnostics,
    const int trace_every,
    const int seed,
    const bool use_seed) {
  // Trace mode reuses the same engine as the plain evaluator and only adds
  // checkpoint snapshots of the internal allocation state.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{
      total_budget,
      rollout_policy,
      max_rollout_turns,
      ucb_exploration,
      prior_alpha,
      prior_beta,
      initial_allocations,
      unresolved_value,
      dice_mode,
      crn,
      seed,
      use_seed,
      fast_diagnostics};
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);
  std::vector<backgammonr::allocation::AllocationTraceRow> trace_rows;
  trace_rows.reserve(static_cast<std::size_t>(std::max(total_budget, 0)) *
      static_cast<std::size_t>(std::max(static_cast<int>(parsed_moves.size()), 1)));

  const std::vector<backgammonr::ActionEvaluationSummary> summaries =
      backgammonr::allocation::evaluate_with_optional_trace(
          parsed_board,
          parsed_moves,
          method,
          config,
          rng,
          trace_every,
          &trace_rows);
  const int best_summary_index = backgammonr::best_candidate_index(summaries);
  const int best_index = summaries[best_summary_index].candidate_index;

  return Rcpp::List::create(
      Rcpp::_["results"] = backgammonr::action_evaluation_summaries_to_data_frame(summaries),
      Rcpp::_["trace"] = backgammonr::allocation::allocation_trace_rows_to_data_frame(trace_rows),
      Rcpp::_["recommended_index"] = Rcpp::IntegerVector::create(best_index),
      Rcpp::_["method"] = Rcpp::CharacterVector::create(
          backgammonr::canonicalize_allocation_method(method)),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_profile_rollout_runtime(
    const Rcpp::List& board,
    const Rcpp::List& roll,
    const int legal_reps,
    const int apply_reps,
    const int one_rollout_reps,
    const int total_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // This profiler decomposes one local decision into move generation, move
  // application, one-rollout, and batched-rollout costs so R-side studies can
  // reason about throughput bottlenecks without rewriting the engine.
  if (legal_reps < 1) {
    throw std::range_error("`legal_reps` must be at least 1.");
  }
  if (apply_reps < 1) {
    throw std::range_error("`apply_reps` must be at least 1.");
  }
  if (one_rollout_reps < 1) {
    throw std::range_error("`one_rollout_reps` must be at least 1.");
  }
  if (total_budget < 1) {
    throw std::range_error("`total_budget` must be at least 1.");
  }

  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  auto tic = std::chrono::steady_clock::now();
  std::vector<backgammonr::MoveSequence> legal_moves;
  for (int i = 0; i < legal_reps; ++i) {
    legal_moves = backgammonr::generate_legal_move_sequences(
        parsed_board, parsed_board.turn, parsed_roll);
  }
  auto toc = std::chrono::steady_clock::now();
  const double legal_seconds = std::chrono::duration<double>(toc - tic).count();

  if (legal_moves.empty()) {
    return Rcpp::List::create(
        Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(0),
        Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
        Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL),
        Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(NA_REAL));
  }

  const backgammonr::MoveSequence first_move = legal_moves.front();

  tic = std::chrono::steady_clock::now();
  for (int i = 0; i < apply_reps; ++i) {
    (void) backgammonr::allocation::apply_sequence_without_full_validation(parsed_board, first_move);
  }
  toc = std::chrono::steady_clock::now();
  const double apply_seconds = std::chrono::duration<double>(toc - tic).count();

  const std::vector<backgammonr::MoveSequence> singleton_moves{first_move};
  const backgammonr::RolloutConfig single_rollout_config{
      1,
      rollout_policy,
      max_rollout_turns};

  tic = std::chrono::steady_clock::now();
  for (int i = 0; i < one_rollout_reps; ++i) {
    (void) backgammonr::evaluate_move_sequences_with_allocation(
        parsed_board,
        singleton_moves,
        "equal",
        single_rollout_config,
        rng);
  }
  toc = std::chrono::steady_clock::now();
  const double one_rollout_seconds = std::chrono::duration<double>(toc - tic).count();

  const backgammonr::RolloutConfig batch_config{
      total_budget,
      rollout_policy,
      max_rollout_turns};
  tic = std::chrono::steady_clock::now();
  (void) backgammonr::evaluate_move_sequences_with_allocation(
      parsed_board,
      legal_moves,
      "equal",
      batch_config,
      rng);
  toc = std::chrono::steady_clock::now();
  const double batched_seconds = std::chrono::duration<double>(toc - tic).count();

  return Rcpp::List::create(
      Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(static_cast<int>(legal_moves.size())),
      Rcpp::_["legal_generation_seconds"] = Rcpp::NumericVector::create(legal_seconds),
      Rcpp::_["move_application_seconds"] = Rcpp::NumericVector::create(apply_seconds),
      Rcpp::_["one_rollout_seconds"] = Rcpp::NumericVector::create(one_rollout_seconds),
      Rcpp::_["batched_rollout_seconds"] = Rcpp::NumericVector::create(batched_seconds),
      Rcpp::_["legal_reps"] = Rcpp::IntegerVector::create(legal_reps),
      Rcpp::_["apply_reps"] = Rcpp::IntegerVector::create(apply_reps),
      Rcpp::_["one_rollout_reps"] = Rcpp::IntegerVector::create(one_rollout_reps),
      Rcpp::_["total_budget"] = Rcpp::IntegerVector::create(total_budget),
      Rcpp::_["rollout_policy"] = Rcpp::CharacterVector::create(rollout_policy),
      Rcpp::_["max_rollout_turns"] = Rcpp::IntegerVector::create(max_rollout_turns));
}
