// Rcpp entry points for explicit posterior kernels and diagnostics.
//
// Each entry point materializes one canonical ActionStats vector from R-side
// sufficient statistics and then delegates to the shared posterior layer.

#include "posterior_core.h"

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
  // Return only the posterior draw matrix; policy logic in R decides how to
  // consume those draws.
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
  // This summary path is the native bridge used by the explicit-posterior TS
  // engine and the posterior-adequacy diagnostics.
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
  // Reference summaries stay separate from posterior summaries because they are
  // empirical Monte Carlo summaries of rollout rewards, not posterior objects.
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
  // Path metrics compress one checkpoint trajectory into "first correct", AUC,
  // and Brier-style diagnostics.
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
  // Calibration binning is kept in C++ so repeated study summaries do not
  // spend time reimplementing the same loop in R.
  return backgammonr::posterior::calibration_summary(predicted_prob, observed_top1, bins);
}
