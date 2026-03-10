#ifndef BACKGAMMONR_BG_ROLLOUT_H
#define BACKGAMMONR_BG_ROLLOUT_H

#include <Rcpp.h>
#include <random>
#include <string>
#include <vector>

#include "bg_board.h"
#include "bg_move.h"

namespace backgammonr {

inline constexpr int kDefaultRolloutBudget = 16;
inline constexpr int kDefaultRolloutMaxTurns = 1000;
inline constexpr double kDefaultUcbExploration = 1.0;
inline constexpr double kDefaultPriorAlpha = 1.0;
inline constexpr double kDefaultPriorBeta = 1.0;
inline constexpr int kDefaultInitialAllocations = 1;
inline constexpr double kDefaultUnresolvedValue = 0.5;
inline constexpr const char* kDefaultDiceMode = "iid";
inline constexpr bool kDefaultFastDiagnostics = false;

struct RolloutConfig {
  int budget{kDefaultRolloutBudget};
  std::string policy{"random"};
  int max_turns{kDefaultRolloutMaxTurns};
  double ucb_exploration{kDefaultUcbExploration};
  double prior_alpha{kDefaultPriorAlpha};
  double prior_beta{kDefaultPriorBeta};
  int initial_allocations{kDefaultInitialAllocations};
  double unresolved_value{kDefaultUnresolvedValue};
  std::string dice_mode{kDefaultDiceMode};
  bool crn{false};
  int crn_seed{0};
  bool use_crn_seed{false};
  bool fast_diagnostics{kDefaultFastDiagnostics};
};

struct RolloutMoveSummary {
  int candidate_index{0};
  int wins{0};
  int losses{0};
  int unresolved{0};
  double win_rate{0.0};
};

bool selection_uses_randomness(const std::string& selection);
void validate_selection(const std::string& selection);
bool is_supported_rollout_policy(const std::string& policy);
void validate_rollout_config(const RolloutConfig& config);
std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng);
MoveSequence choose_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng);
Rcpp::DataFrame rollout_move_summaries_to_data_frame(
    const std::vector<RolloutMoveSummary>& summaries);

}  // namespace backgammonr

#endif
