#ifndef BACKGAMMONR_BG_ALLOC_TRACE_H
#define BACKGAMMONR_BG_ALLOC_TRACE_H

// Allocation-trace declarations.
//
// The trace layer is intentionally separate from the rollout core so the
// public package can request checkpoint diagnostics without forking the
// underlying simulation loop.

#include <Rcpp.h>

#include <vector>

#include "alloc_types.h"
#include "alloc_interface.h"

namespace backgammonr {
namespace allocation {

// Identify the current empirical/posterior leader for checkpoint reporting.
int current_leader_index(const std::vector<ActionEvaluationSummary>& summaries);

// Append one checkpoint snapshot across every candidate.
void append_trace_snapshot(
    std::vector<AllocationTraceRow>& trace_rows,
    std::vector<ActionEvaluationSummary>& summaries,
    AllocationPolicy policy,
    const RolloutConfig& config,
    int checkpoint,
    int selected_candidate);

// Convert compact native trace rows into the data-frame schema consumed by the
// R-side diagnostics and plotting front doors.
Rcpp::DataFrame allocation_trace_rows_to_data_frame(
    const std::vector<AllocationTraceRow>& trace_rows);

}  // namespace allocation
}  // namespace backgammonr

#endif
