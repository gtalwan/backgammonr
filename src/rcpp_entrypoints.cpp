// Rcpp entry points for the statistical/native layer.
//
// This file is intentionally a thin bridge between the R package surface and
// the native statistical helpers. It does not implement allocation or
// posterior logic itself. Instead it:
// - parses R objects once into native structs;
// - assembles one configuration object per call;
// - delegates to alloc_core / posterior_core; and
// - converts the compact native outputs back into R-friendly data frames.
//
// Keeping the entry points in one file makes the native layout easier to read:
// users can now look in one place for "what does R actually call?"

#include "alloc_core.h"
#include "posterior_core.h"

#include <chrono>
#include <stdexcept>
#include <vector>

#include "alloc_trace.h"
#include "bg_movegen.h"
#include "bg_rng.h"
#include "posterior_policy.h"

// ---------------------------------------------------------------------------
// Allocation-engine entry points
// ---------------------------------------------------------------------------

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
  // Stable evaluation entry point used by the fast scalar allocation layer.
  // R has already chosen the canonical method label and rollout configuration;
  // this wrapper only marshals data and delegates to the shared engine.
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
  // Trace mode reuses the same allocation engine and only adds checkpoint
  // snapshots. This keeps "evaluation" and "evaluation with trace" on the same
  // simulation path.
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
  // Runtime profiling is separated from ordinary evaluation so the R layer can
  // benchmark engine components without modifying study code.
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

// ---------------------------------------------------------------------------
// Explicit-posterior entry points
// ---------------------------------------------------------------------------

// [[Rcpp::export]]
int bg_cpp_posterior_equal_choice(
    const Rcpp::IntegerVector& active_idx,
    const int spent) {
  return backgammonr::posterior_policy::choose_posterior_equal_candidate(
      active_idx,
      spent);
}

// [[Rcpp::export]]
int bg_cpp_posterior_thompson_choice(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean) {
  return backgammonr::posterior_policy::choose_posterior_thompson_candidate(
      draw_mat,
      allocation_count,
      posterior_mean);
}

// [[Rcpp::export]]
int bg_cpp_posterior_top_two_choice(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const double ttts_beta) {
  Rcpp::RNGScope scope;
  return backgammonr::posterior_policy::choose_posterior_ttts_candidate(
      draw_mat,
      allocation_count,
      ttts_beta);
}

// [[Rcpp::export]]
int bg_cpp_posterior_multi_sample_choice(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& allocation_count,
    const Rcpp::NumericVector& posterior_mean) {
  return backgammonr::posterior_policy::choose_posterior_multi_sample_candidate(
      draw_mat,
      allocation_count,
      posterior_mean);
}

// [[Rcpp::export]]
int bg_cpp_posterior_forced_choice(
    const Rcpp::NumericVector& allocation_count,
    const int spent,
    const int forced_every,
    const int forced_min_allocations) {
  return backgammonr::posterior_policy::choose_posterior_forced_candidate(
      allocation_count,
      spent,
      forced_every,
      forced_min_allocations);
}

// [[Rcpp::export]]
Rcpp::LogicalVector bg_cpp_posterior_update_active_set(
    const Rcpp::LogicalVector& active,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& posterior_sd,
    const Rcpp::NumericVector& allocation_count,
    const int min_allocations,
    const int keep_top,
    const double margin) {
  return backgammonr::posterior_policy::update_posterior_active_set(
      active,
      posterior_mean,
      posterior_sd,
      allocation_count,
      min_allocations,
      keep_top,
      margin);
}

// [[Rcpp::export]]
int bg_cpp_posterior_top_k_choice(
    const Rcpp::NumericMatrix& draw_mat,
    const Rcpp::NumericVector& posterior_mean,
    const Rcpp::NumericVector& allocation_count,
    const int focus_top_k) {
  return backgammonr::posterior_policy::choose_posterior_top_k_candidate(
      draw_mat,
      posterior_mean,
      allocation_count,
      focus_top_k);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix bg_cpp_posterior_sample_values(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& posterior_prior,
    const int draws) {
  // Return the raw posterior draw matrix only. The R layer decides whether the
  // draws are used for Thompson selection, probability-best summaries, or
  // posterior adequacy checks.
  Rcpp::RNGScope scope;
  const std::vector<backgammonr::posterior::ActionStats> stats =
      backgammonr::posterior::materialize_stats(
          allocation_count,
          wins,
          losses,
          single_loss,
          gammon_loss,
          backgammon_loss,
          unresolved,
          single_win,
          gammon_win,
          backgammon_win,
          reward_sum,
          reward_sum_sq);
  return backgammonr::posterior::posterior_draw_matrix(
      stats,
      reward_model,
      posterior_model,
      unresolved_value,
      posterior_prior,
      draws);
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_posterior_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& wins,
    const Rcpp::IntegerVector& losses,
    const Rcpp::IntegerVector& single_loss,
    const Rcpp::IntegerVector& gammon_loss,
    const Rcpp::IntegerVector& backgammon_loss,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::IntegerVector& single_win,
    const Rcpp::IntegerVector& gammon_win,
    const Rcpp::IntegerVector& backgammon_win,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const std::string& reward_model,
    const std::string& posterior_model,
    const double unresolved_value,
    const Rcpp::List& posterior_prior,
    const int draws) {
  if (draws < 2) {
    Rcpp::stop("`draws` must be at least 2 for posterior summaries.");
  }

  Rcpp::RNGScope scope;
  const std::vector<backgammonr::posterior::ActionStats> stats =
      backgammonr::posterior::materialize_stats(
          allocation_count,
          wins,
          losses,
          single_loss,
          gammon_loss,
          backgammon_loss,
          unresolved,
          single_win,
          gammon_win,
          backgammon_win,
          reward_sum,
          reward_sum_sq);
  const Rcpp::NumericMatrix draw_matrix = backgammonr::posterior::posterior_draw_matrix(
      stats,
      reward_model,
      posterior_model,
      unresolved_value,
      posterior_prior,
      draws);

  Rcpp::NumericVector prob_best;
  Rcpp::NumericVector expected_regret;
  backgammonr::posterior::posterior_draw_metrics(draw_matrix, prob_best, expected_regret);

  const int n_actions = static_cast<int>(stats.size());
  Rcpp::NumericVector estimate(n_actions);
  Rcpp::NumericVector posterior_sd(n_actions);
  Rcpp::NumericVector lower_95(n_actions);
  Rcpp::NumericVector upper_95(n_actions);
  Rcpp::NumericVector alpha(n_actions, NA_REAL);
  Rcpp::NumericVector beta(n_actions, NA_REAL);

  for (int action = 0; action < n_actions; ++action) {
    const backgammonr::posterior::PosteriorSummaryRow row =
        backgammonr::posterior::summarize_action(
            stats[static_cast<std::size_t>(action)],
            reward_model,
            posterior_model,
            unresolved_value,
            posterior_prior,
            draw_matrix,
            action,
            prob_best,
            expected_regret);
    estimate[action] = row.estimate;
    posterior_sd[action] = row.posterior_sd;
    lower_95[action] = row.lower_95;
    upper_95[action] = row.upper_95;
    alpha[action] = row.alpha;
    beta[action] = row.beta;
  }

  return Rcpp::DataFrame::create(
      Rcpp::Named("estimate") = estimate,
      Rcpp::Named("posterior_sd") = posterior_sd,
      Rcpp::Named("lower_95") = lower_95,
      Rcpp::Named("upper_95") = upper_95,
      Rcpp::Named("model_relative_prob_best") = prob_best,
      Rcpp::Named("model_relative_expected_regret") = expected_regret,
      Rcpp::Named("alpha") = alpha,
      Rcpp::Named("beta") = beta);
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_reference_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    const double prior_alpha,
    const double prior_beta) {
  // Reference summaries are empirical Monte Carlo summaries, not posterior
  // summaries. They live here because they reuse the same compact sufficient
  // statistics.
  return backgammonr::posterior::reference_summary(
      allocation_count,
      unresolved,
      reward_sum,
      reward_sum_sq,
      prior_alpha,
      prior_beta);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_eval_path_metrics(
    const Rcpp::IntegerVector& checkpoint,
    const Rcpp::NumericVector& runtime_seconds,
    const Rcpp::LogicalVector& top1_match,
    const Rcpp::LogicalVector& epsilon_optimal,
    const Rcpp::NumericVector& simple_regret,
    const Rcpp::NumericVector& recommended_prob_best) {
  return backgammonr::posterior::eval_path_metrics(
      checkpoint,
      runtime_seconds,
      top1_match,
      epsilon_optimal,
      simple_regret,
      recommended_prob_best);
}

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_calibration_summary(
    const Rcpp::NumericVector& predicted_prob,
    const Rcpp::NumericVector& observed_top1,
    const int bins) {
  return backgammonr::posterior::calibration_summary(predicted_prob, observed_top1, bins);
}
