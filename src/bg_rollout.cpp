#include "bg_rollout.h"

#include <cstdint>
#include <random>
#include <stdexcept>
#include <vector>

#include "bg_allocation.h"

namespace {

std::mt19937 init_rng(const int seed, const bool use_seed) {
  std::mt19937 rng;

  if (use_seed) {
    if (seed < 0) {
      throw std::range_error("`seed` must be nonnegative when supplied.");
    }
    rng.seed(static_cast<std::uint32_t>(seed));
  } else {
    std::random_device rd;
    rng.seed(rd());
  }

  return rng;
}

}  // namespace

namespace backgammonr {

bool selection_uses_randomness(const std::string& selection) {
  validate_selection(selection);
  return selection == "random" ||
      selection == "rollout" ||
      selection == "equal_rollout" ||
      selection == "greedy_rollout" ||
      selection == "ucb_rollout" ||
      selection == "thompson_rollout" ||
      selection == "ocba_rollout" ||
      selection == "ttts_rollout";
}

void validate_selection(const std::string& selection) {
  if (selection != "first" &&
      selection != "random" &&
      selection != "aggressive" &&
      selection != "defensive" &&
      selection != "rollout" &&
      selection != "equal_rollout" &&
      selection != "greedy_rollout" &&
      selection != "ucb_rollout" &&
      selection != "thompson_rollout" &&
      selection != "ocba_rollout" &&
      selection != "ttts_rollout") {
    throw std::range_error(
        "`selection` must be one of \"first\", \"random\", \"aggressive\", \"defensive\", \"rollout\", \"equal_rollout\", \"greedy_rollout\", \"ucb_rollout\", \"thompson_rollout\", \"ttts_rollout\", or \"ocba_rollout\".");
  }
}

bool is_supported_rollout_policy(const std::string& policy) {
  return policy == "random" || policy == "aggressive" || policy == "defensive";
}

bool is_supported_dice_mode(const std::string& dice_mode) {
  return dice_mode == "iid" ||
      dice_mode == "stratified_first_roll" ||
      dice_mode == "stratified_first_two_rolls";
}

void validate_rollout_config(const RolloutConfig& config) {
  if (config.budget < 1) {
    throw std::range_error("`rollout_budget` must be at least 1.");
  }

  if (!is_supported_rollout_policy(config.policy)) {
    throw std::range_error(
        "`rollout_policy` must be one of \"random\", \"aggressive\", or \"defensive\".");
  }

  if (config.max_turns < 0) {
    throw std::range_error("`max_rollout_turns` must be nonnegative.");
  }

  if (config.ucb_exploration < 0.0) {
    throw std::range_error("`ucb_exploration` must be nonnegative.");
  }

  if (config.prior_alpha <= 0.0 || config.prior_beta <= 0.0) {
    throw std::range_error("`prior_alpha` and `prior_beta` must be positive.");
  }

  if (config.initial_allocations < 0) {
    throw std::range_error("`initial_allocations` must be nonnegative.");
  }

  if (config.unresolved_value < 0.0 || config.unresolved_value > 1.0) {
    throw std::range_error("`unresolved_value` must lie between 0 and 1.");
  }

  if (!is_supported_dice_mode(config.dice_mode)) {
    throw std::range_error(
        "`dice_mode` must be one of \"iid\", \"stratified_first_roll\", or \"stratified_first_two_rolls\".");
  }
}

std::vector<RolloutMoveSummary> evaluate_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, "equal", config, rng);

  std::vector<RolloutMoveSummary> out;
  out.reserve(summaries.size());

  for (const ActionEvaluationSummary& summary : summaries) {
    RolloutMoveSummary row;
    row.candidate_index = summary.candidate_index;
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    row.win_rate = Rcpp::NumericVector::is_na(summary.empirical_value)
        ? summary.estimate
        : summary.empirical_value;
    out.push_back(row);
  }

  return out;
}

MoveSequence choose_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  return choose_move_sequence_with_allocation(board, legal_moves, "equal", config, rng);
}

Rcpp::DataFrame rollout_move_summaries_to_data_frame(
    const std::vector<RolloutMoveSummary>& summaries) {
  const int n = static_cast<int>(summaries.size());
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector win_rate(n);

  for (int i = 0; i < n; ++i) {
    candidate_index[i] = summaries[i].candidate_index;
    wins[i] = summaries[i].wins;
    losses[i] = summaries[i].losses;
    unresolved[i] = summaries[i].unresolved;
    win_rate[i] = summaries[i].win_rate;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["win_rate"] = win_rate,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::action_evaluation_summaries_to_data_frame(
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "equal",
          config,
          rng));
}
