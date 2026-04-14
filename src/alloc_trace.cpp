// Allocation trace snapshots and export helpers.
//
// The trace layer exists for one reason: the package wants readable,
// checkpoint-by-checkpoint pictures of what the allocator is doing without
// duplicating the rollout loop. The core engine simulates and updates; this
// file only:
// - asks the core to refresh posterior/empirical summaries at a checkpoint;
// - records which candidate was just sampled and which candidate currently
//   leads; and
// - converts the compact native trace rows into the data-frame schema used by
//   the R plotting and diagnostics layer.

#include "alloc_trace.h"

#include "alloc_core.h"

namespace backgammonr {
namespace allocation {

int current_leader_index(const std::vector<ActionEvaluationSummary>& summaries) {
  // "Leader" means the action with the largest current posterior mean
  // estimate, with deterministic tie-breaking toward the more-sampled action.
  // This is not the same thing as the last sampled action.
  if (summaries.empty()) {
    return NA_INTEGER;
  }

  int leader = 0;
  for (int i = 1; i < static_cast<int>(summaries.size()); ++i) {
    if (summaries[i].estimate > summaries[leader].estimate + kTieTolerance) {
      leader = i;
      continue;
    }

    if (std::fabs(summaries[i].estimate - summaries[leader].estimate) <= kTieTolerance &&
        summaries[i].allocation_count > summaries[leader].allocation_count) {
      leader = i;
    }
  }

  return leader;
}

void append_trace_snapshot(
    std::vector<AllocationTraceRow>& trace_rows,
    std::vector<ActionEvaluationSummary>& summaries,
    const AllocationPolicy policy,
    const RolloutConfig& config,
    const int checkpoint,
    const int selected_candidate) {
  // Always refresh the shared summary fields first so the trace row reflects
  // the allocator state *after* the newly completed rollout has been absorbed.
  refresh_summary_fields(summaries, policy, config, checkpoint);
  const int leader_pos = current_leader_index(summaries);
  const int leader_index = leader_pos == NA_INTEGER
      ? NA_INTEGER
      : summaries[leader_pos].candidate_index;

  for (const ActionEvaluationSummary& summary : summaries) {
    // Emit one row per candidate so the R side can later reconstruct
    // candidate-by-checkpoint panels without needing any more native logic.
    AllocationTraceRow row;
    row.checkpoint = checkpoint;
    row.selected_candidate = selected_candidate;
    row.leader_index = leader_index;
    row.candidate_index = summary.candidate_index;
    row.allocation_count = summary.allocation_count;
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    row.empirical_value = summary.empirical_value;
    row.alpha = summary.alpha;
    row.beta = summary.beta;
    row.estimate = summary.estimate;
    row.posterior_sd = summary.posterior_sd;
    row.lower_95 = summary.lower_95;
    row.upper_95 = summary.upper_95;
    row.selection_score = summary.selection_score;
    trace_rows.push_back(row);
  }
}

Rcpp::DataFrame allocation_trace_rows_to_data_frame(
    const std::vector<AllocationTraceRow>& trace_rows) {
  // The trace export is intentionally column-oriented because the R layer uses
  // the result directly for tidy plots, checkpoint tables, and seed-level
  // diagnostics.
  const int n = static_cast<int>(trace_rows.size());
  Rcpp::IntegerVector checkpoint(n);
  Rcpp::IntegerVector selected_candidate(n);
  Rcpp::IntegerVector leader_index(n);
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector empirical_value(n);
  Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector beta(n);
  Rcpp::NumericVector estimate(n);
  Rcpp::NumericVector posterior_sd(n);
  Rcpp::NumericVector lower_95(n);
  Rcpp::NumericVector upper_95(n);
  Rcpp::NumericVector selection_score(n);

  for (int i = 0; i < n; ++i) {
    checkpoint[i] = trace_rows[i].checkpoint;
    selected_candidate[i] = trace_rows[i].selected_candidate;
    leader_index[i] = trace_rows[i].leader_index;
    candidate_index[i] = trace_rows[i].candidate_index;
    allocation_count[i] = trace_rows[i].allocation_count;
    wins[i] = trace_rows[i].wins;
    losses[i] = trace_rows[i].losses;
    unresolved[i] = trace_rows[i].unresolved;
    empirical_value[i] = trace_rows[i].empirical_value;
    alpha[i] = trace_rows[i].alpha;
    beta[i] = trace_rows[i].beta;
    estimate[i] = trace_rows[i].estimate;
    posterior_sd[i] = trace_rows[i].posterior_sd;
    lower_95[i] = trace_rows[i].lower_95;
    upper_95[i] = trace_rows[i].upper_95;
    selection_score[i] = trace_rows[i].selection_score;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["checkpoint"] = checkpoint,
      Rcpp::_["selected_candidate"] = selected_candidate,
      Rcpp::_["leader_index"] = leader_index,
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["empirical_value"] = empirical_value,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta,
      Rcpp::_["estimate"] = estimate,
      Rcpp::_["posterior_sd"] = posterior_sd,
      Rcpp::_["lower_95"] = lower_95,
      Rcpp::_["upper_95"] = upper_95,
      Rcpp::_["selection_score"] = selection_score,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace allocation
}  // namespace backgammonr
