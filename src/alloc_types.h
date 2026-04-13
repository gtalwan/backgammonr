#ifndef BACKGAMMONR_BG_ALLOC_TYPES_H
#define BACKGAMMONR_BG_ALLOC_TYPES_H

// Internal allocation-engine types.
//
// This header collects the small native data structures that every allocation
// module needs to agree on:
// - collapsed root-move representations;
// - the policy labels used by the shared fixed-budget engine; and
// - trace rows emitted for R-side diagnostics.
//
// These types stay internal because they describe implementation details of
// the rollout allocator rather than the stable package API.

#include <Rcpp.h>

#include <array>
#include <cstddef>

#include "bg_board.h"
#include "bg_dice.h"

namespace backgammonr {
namespace allocation {

enum class RolloutOutcome {
  // Coarse outcome classes used by the scalar/Beta allocation engine. Richer
  // scored-outcome tallies live in ActionEvaluationSummary itself.
  kWin,
  kLoss,
  kUnresolved
};

enum class AllocationPolicy {
  // The fixed-budget policies supported by the shared native engine.
  kEqual,
  kGreedy,
  kUcb,
  kThompson,
  kTtts,
  kOcba
};

inline constexpr double kTieTolerance = 1e-12;
inline constexpr int kPosteriorDiagnosticDraws = 512;

struct CollapsedCandidate {
  // Several legal move sequences can reach the same successor board. The
  // allocation engine only needs to simulate that successor once, but it still
  // tracks which original representative index and multiplicity it came from.
  BoardState board_after{};
  int representative_index{0};
  int acting_player{1};
  int n_equivalent{1};
};

struct AllocationTraceRow {
  // One trace row records the allocator's state for one candidate at one
  // checkpoint. Together these rows reconstruct the checkpoint-by-candidate
  // panel returned to R for allocation-flow plots and diagnostics.
  int checkpoint{0};
  int selected_candidate{NA_INTEGER};
  int leader_index{NA_INTEGER};
  int candidate_index{0};
  int allocation_count{0};
  int wins{0};
  int losses{0};
  int unresolved{0};
  double empirical_value{NA_REAL};
  double alpha{1.0};
  double beta{1.0};
  double estimate{0.5};
  double posterior_sd{0.0};
  double lower_95{0.0};
  double upper_95{1.0};
  double selection_score{0.5};
};

struct ForcedRollSchedule {
  // Optional synchronized dice prefix used by CRN/stratified-dice modes.
  std::array<DiceRoll, 2> rolls{};
  int n_rolls{0};
};

struct BoardStateKey {
  // Compact hashable representation of a root successor board. Used only to
  // collapse equivalent legal sequences before rollout work starts.
  std::array<int, kNumPoints> points{};
  std::array<int, kNumPlayers> bar{};
  std::array<int, kNumPlayers> off{};
  int turn{1};

  bool operator==(const BoardStateKey& other) const {
    return turn == other.turn &&
        points == other.points &&
        bar == other.bar &&
        off == other.off;
  }
};

struct BoardStateKeyHash {
  std::size_t operator()(const BoardStateKey& key) const {
    std::size_t h = static_cast<std::size_t>(key.turn * 1315423911U);

    for (const int value : key.points) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }
    for (const int value : key.bar) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }
    for (const int value : key.off) {
      h ^= static_cast<std::size_t>(value + 0x9e3779b9U + (h << 6) + (h >> 2));
    }

    return h;
  }
};

}  // namespace allocation
}  // namespace backgammonr

#endif
