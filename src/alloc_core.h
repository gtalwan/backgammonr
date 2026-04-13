#ifndef BACKGAMMONR_BG_ALLOC_CORE_H
#define BACKGAMMONR_BG_ALLOC_CORE_H

// Shared allocation-engine declarations.
//
// The fixed-budget scalar allocation engine has one rollout loop and several
// policy selectors. This header groups the shared helpers that keep those
// pieces consistent:
// - root-move collapsing and rollout execution;
// - policy parsing and candidate selection;
// - posterior/Beta summary refreshes; and
// - the one internal evaluation entry point used by the public wrappers.

#include <Rcpp.h>

#include <cstdint>
#include <random>
#include <string>
#include <vector>

#include "alloc_types.h"
#include "alloc_interface.h"

namespace backgammonr {

struct TurnResult;

namespace allocation {

// Root-move execution and rollout helpers.
BoardState apply_sequence_without_full_validation(
    const BoardState& board,
    const MoveSequence& sequence);

void play_random_turn_lightweight(
    BoardState& board,
    const DiceRoll& roll,
    std::mt19937& rng);

double outcome_reward(
    RolloutOutcome outcome,
    const RolloutConfig& config);

RolloutOutcome outcome_from_turn_result(
    const TurnResult& turn_result,
    int acting_player);

RolloutOutcome single_rollout_outcome(
    const BoardState& board_after,
    int acting_player,
    const RolloutConfig& config,
    std::mt19937& rng,
    const ForcedRollSchedule& forced_rolls);

// Summary updates for the Beta-style scalar allocation engine.
void update_summary(
    ActionEvaluationSummary& summary,
    RolloutOutcome outcome,
    const RolloutConfig& config);

double sample_beta_distribution(double alpha, double beta, std::mt19937& rng);

// Policy routing and tie-breaking helpers.
AllocationPolicy parse_allocation_policy(const std::string& canonical_method);
bool score_beats_incumbent(
    double score,
    int allocation_count,
    double incumbent_score,
    int incumbent_allocation_count);
std::uint32_t stable_rollout_seed(
    std::uint32_t base_seed,
    int sample_index,
    int salt);
const std::vector<DiceRoll>& unique_unordered_rolls();
ForcedRollSchedule scheduled_forced_rolls(
    const std::string& dice_mode,
    int sample_index,
    int offset);

// Candidate collapsing converts multiple legal sequences that reach the same
// successor board into one rollout target plus a multiplicity count.
BoardStateKey board_state_key(const BoardState& board);
std::vector<CollapsedCandidate> collapse_equivalent_candidates(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves);

// Posterior diagnostics are exported with the scalar engine's final action
// table so TS/TTTS traces can report means, intervals, and prob-best values.
void compute_posterior_diagnostics(
    std::vector<ActionEvaluationSummary>& summaries,
    std::mt19937& rng);
void refresh_summary_fields(
    std::vector<ActionEvaluationSummary>& summaries,
    AllocationPolicy policy,
    const RolloutConfig& config,
    int total_allocations);
void finalize_summaries(
    std::vector<ActionEvaluationSummary>& summaries,
    AllocationPolicy policy,
    const RolloutConfig& config,
    std::mt19937& rng);

int choose_equal_candidate(int n_candidates, int step);
int choose_greedy_candidate(
    const std::vector<ActionEvaluationSummary>& summaries);
int choose_ucb_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    int step,
    const RolloutConfig& config);
int choose_ocba_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    int step);
int choose_thompson_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    std::mt19937& rng);
int choose_ttts_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    const RolloutConfig& config,
    std::mt19937& rng);

// Shared policy dispatch used by both traced and non-traced evaluation paths.
int choose_next_candidate(
    const std::vector<ActionEvaluationSummary>& summaries,
    AllocationPolicy policy,
    int step,
    const RolloutConfig& config,
    std::mt19937& rng);

// The one native rollout loop for fixed-budget method comparison. Public Rcpp
// entry points either call it directly or request additional trace rows.
std::vector<ActionEvaluationSummary> evaluate_with_optional_trace(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& method,
    const RolloutConfig& config,
    std::mt19937& rng,
    int trace_every,
    std::vector<AllocationTraceRow>* trace_rows);

}  // namespace allocation
}  // namespace backgammonr

#endif
