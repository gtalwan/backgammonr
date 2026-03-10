// LINE NOTE: #ifndef BACKGAMMONR_BG_ROLLOUT_H
#ifndef BACKGAMMONR_BG_ROLLOUT_H
// LINE NOTE: #define BACKGAMMONR_BG_ROLLOUT_H
#define BACKGAMMONR_BG_ROLLOUT_H

// LINE NOTE: #include <Rcpp.h>
#include <Rcpp.h>
// LINE NOTE: #include <random>
#include <random>
// LINE NOTE: #include <string>
#include <string>
// LINE NOTE: #include <vector>
#include <vector>

// LINE NOTE: #include "bg_board.h"
#include "bg_board.h"
// LINE NOTE: #include "bg_move.h"
#include "bg_move.h"

// LINE NOTE: namespace backgammonr {
namespace backgammonr {

// LINE NOTE: inline constexpr int kDefaultRolloutBudget = 16;
inline constexpr int kDefaultRolloutBudget = 16;
// LINE NOTE: inline constexpr int kDefaultRolloutMaxTurns = 1000;
inline constexpr int kDefaultRolloutMaxTurns = 1000;
// LINE NOTE: inline constexpr double kDefaultUcbExploration = 1.0;
inline constexpr double kDefaultUcbExploration = 1.0;
// LINE NOTE: inline constexpr double kDefaultPriorAlpha = 1.0;
inline constexpr double kDefaultPriorAlpha = 1.0;
// LINE NOTE: inline constexpr double kDefaultPriorBeta = 1.0;
inline constexpr double kDefaultPriorBeta = 1.0;
// LINE NOTE: inline constexpr int kDefaultInitialAllocations = 1;
inline constexpr int kDefaultInitialAllocations = 1;
// LINE NOTE: inline constexpr double kDefaultUnresolvedValue = 0.5;
inline constexpr double kDefaultUnresolvedValue = 0.5;
// LINE NOTE: inline constexpr const char* kDefaultDiceMode = "iid";
inline constexpr const char* kDefaultDiceMode = "iid";
// LINE NOTE: inline constexpr bool kDefaultFastDiagnostics = false;
inline constexpr bool kDefaultFastDiagnostics = false;

// LINE NOTE: struct RolloutConfig {
struct RolloutConfig {
// LINE NOTE:   int budget{kDefaultRolloutBudget};
  int budget{kDefaultRolloutBudget};
// LINE NOTE:   std::string policy{"random"};
  std::string policy{"random"};
// LINE NOTE:   int max_turns{kDefaultRolloutMaxTurns};
  int max_turns{kDefaultRolloutMaxTurns};
// LINE NOTE:   double ucb_exploration{kDefaultUcbExploration};
  double ucb_exploration{kDefaultUcbExploration};
// LINE NOTE:   double prior_alpha{kDefaultPriorAlpha};
  double prior_alpha{kDefaultPriorAlpha};
// LINE NOTE:   double prior_beta{kDefaultPriorBeta};
  double prior_beta{kDefaultPriorBeta};
// LINE NOTE:   int initial_allocations{kDefaultInitialAllocations};
  int initial_allocations{kDefaultInitialAllocations};
// LINE NOTE:   double unresolved_value{kDefaultUnresolvedValue};
  double unresolved_value{kDefaultUnresolvedValue};
// LINE NOTE:   std::string dice_mode{kDefaultDiceMode};
  std::string dice_mode{kDefaultDiceMode};
// LINE NOTE:   bool crn{false};
  bool crn{false};
// LINE NOTE:   int crn_seed{0};
  int crn_seed{0};
// LINE NOTE:   bool use_crn_seed{false};
  bool use_crn_seed{false};
// LINE NOTE:   bool fast_diagnostics{kDefaultFastDiagnostics};
  bool fast_diagnostics{kDefaultFastDiagnostics};
// LINE NOTE: };
};

// LINE NOTE: struct RolloutMoveSummary {
struct RolloutMoveSummary {
// LINE NOTE:   int candidate_index{0};
  int candidate_index{0};
// LINE NOTE:   int wins{0};
  int wins{0};
// LINE NOTE:   int losses{0};
  int losses{0};
// LINE NOTE:   int unresolved{0};
  int unresolved{0};
// LINE NOTE:   double win_rate{0.0};
  double win_rate{0.0};
// LINE NOTE: };
};

// LINE NOTE: bool selection_uses_randomness(const std::string& selection);
bool selection_uses_randomness(const std::string& selection);
// LINE NOTE: void validate_selection(const std::string& selection);
void validate_selection(const std::string& selection);
// LINE NOTE: bool is_supported_rollout_policy(const std::string& policy);
bool is_supported_rollout_policy(const std::string& policy);
// LINE NOTE: void validate_rollout_config(const RolloutConfig& config);
void validate_rollout_config(const RolloutConfig& config);
// LINE NOTE: std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);
// LINE NOTE: MoveSequence choose_rollout_move_sequence(
MoveSequence choose_rollout_move_sequence(
// LINE NOTE:     const BoardState& board,
    const BoardState& board,
// LINE NOTE:     const std::vector<MoveSequence>& legal_moves,
    const std::vector<MoveSequence>& legal_moves,
// LINE NOTE:     const RolloutConfig& config,
    const RolloutConfig& config,
// LINE NOTE:     std::mt19937& rng);
    std::mt19937& rng);
// LINE NOTE: Rcpp::DataFrame rollout_move_summaries_to_data_frame(
Rcpp::DataFrame rollout_move_summaries_to_data_frame(
// LINE NOTE:     const std::vector<RolloutMoveSummary>& summaries);
    const std::vector<RolloutMoveSummary>& summaries);

// LINE NOTE: }  // namespace backgammonr
}  // namespace backgammonr

// LINE NOTE: #endif
#endif
