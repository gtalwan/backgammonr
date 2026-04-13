#ifndef BACKGAMMONR_BG_POSTERIOR_CORE_H
#define BACKGAMMONR_BG_POSTERIOR_CORE_H

// Shared posterior-kernel types and declarations.
//
// The research-facing R layer routes all explicit posterior work through these
// kernels. This header keeps shared sufficient-stat handling and summary
// contracts in one place while family-specific math lives in separate
// translation units.

#include <Rcpp.h>

#include <string>
#include <vector>

namespace backgammonr {
namespace posterior {

struct ActionStats {
  // One action's sufficient statistics accumulated from rollout outcomes. The
  // same struct feeds every posterior family so the R layer can switch models
  // without changing the native data contract.
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int single_loss{0};
  int gammon_loss{0};
  int backgammon_loss{0};
  int unresolved{0};
  int single_win{0};
  int gammon_win{0};
  int backgammon_win{0};
  double reward_sum{0.0};
  double reward_sum_sq{0.0};
};

struct PosteriorSummaryRow {
  // Unified summary row returned regardless of posterior family. Some fields
  // (for example alpha/beta) are only meaningful for subsets of models.
  double estimate{NA_REAL};
  double posterior_sd{NA_REAL};
  double lower_95{NA_REAL};
  double upper_95{NA_REAL};
  double prob_best{NA_REAL};
  double expected_regret{NA_REAL};
  double alpha{NA_REAL};
  double beta{NA_REAL};
};

double clamp_unit_interval(double x);
double safe_sample_variance(const ActionStats& stats);
double safe_empirical_mean(const ActionStats& stats, double fallback);
double list_number_or_default(const Rcpp::List& x, const char* name, double fallback);
Rcpp::NumericVector list_numeric_or_default(
    const Rcpp::List& x,
    const char* name,
    const Rcpp::NumericVector& fallback);

std::vector<ActionStats> materialize_stats(
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
    const Rcpp::NumericVector& reward_sum_sq);

Rcpp::NumericVector scored_category_counts(const ActionStats& stats);
void effective_binary_counts(
    const ActionStats& stats,
    double unresolved_value,
    double& success,
    double& failure);
void normal_inverse_gamma_posterior(
    const ActionStats& stats,
    const Rcpp::List& prior,
    double& mean_n,
    double& kappa_n,
    double& shape_n,
    double& scale_n);
double dirichlet_linear_variance(
    const Rcpp::NumericVector& alpha,
    const Rcpp::NumericVector& payoff);
Rcpp::NumericVector categorical_alpha_prior(const Rcpp::List& prior);
Rcpp::NumericVector categorical_payoff_map(
    const Rcpp::List& prior,
    double unresolved_value);
Rcpp::NumericVector posterior_categorical_alpha(
    const ActionStats& stats,
    const Rcpp::List& prior);

std::vector<int> ordered_index(const Rcpp::IntegerVector& x);
double trapezoid_area_sorted(
    const Rcpp::IntegerVector& x,
    const Rcpp::NumericVector& y,
    const std::vector<int>& ord);

double sample_beta_family_value(
    const ActionStats& stats,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& prior);
PosteriorSummaryRow summarize_beta_family(
    const ActionStats& stats,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& prior);

double sample_dirichlet_value(
    const ActionStats& stats,
    double unresolved_value,
    const Rcpp::List& prior);
PosteriorSummaryRow summarize_dirichlet_family(
    const ActionStats& stats,
    double unresolved_value,
    const Rcpp::List& prior);

double sample_scalar_model_value(
    const ActionStats& stats,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& prior);
PosteriorSummaryRow summarize_scalar_model(
    const ActionStats& stats,
    const std::string& posterior_model,
    const Rcpp::List& prior);

double sample_bootstrap_value(
    const ActionStats& stats,
    const std::string& reward_model,
    double unresolved_value,
    const Rcpp::List& prior);

double sample_posterior_value(
    const ActionStats& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& prior);

Rcpp::NumericMatrix posterior_draw_matrix(
    const std::vector<ActionStats>& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& posterior_prior,
    int draws);

void posterior_draw_metrics(
    const Rcpp::NumericMatrix& draw_matrix,
    Rcpp::NumericVector& prob_best,
    Rcpp::NumericVector& expected_regret);

PosteriorSummaryRow summarize_action(
    const ActionStats& stats,
    const std::string& reward_model,
    const std::string& posterior_model,
    double unresolved_value,
    const Rcpp::List& prior,
    const Rcpp::NumericMatrix& draw_matrix,
    int action_index,
    const Rcpp::NumericVector& prob_best,
    const Rcpp::NumericVector& expected_regret);

Rcpp::DataFrame reference_summary(
    const Rcpp::IntegerVector& allocation_count,
    const Rcpp::IntegerVector& unresolved,
    const Rcpp::NumericVector& reward_sum,
    const Rcpp::NumericVector& reward_sum_sq,
    double prior_alpha,
    double prior_beta);

Rcpp::List eval_path_metrics(
    const Rcpp::IntegerVector& checkpoint,
    const Rcpp::NumericVector& runtime_seconds,
    const Rcpp::LogicalVector& top1_match,
    const Rcpp::LogicalVector& epsilon_optimal,
    const Rcpp::NumericVector& simple_regret,
    const Rcpp::NumericVector& recommended_prob_best);

Rcpp::DataFrame calibration_summary(
    const Rcpp::NumericVector& predicted_prob,
    const Rcpp::NumericVector& observed_top1,
    int bins);

}  // namespace posterior
}  // namespace backgammonr

#endif
