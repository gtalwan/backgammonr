// Combined engine player/helper kernels.
//
// This translation unit collects random, heuristic, and Thompson-rollout
// move-choice helpers without changing their public API.

// -----------------------------------------------------------------------------
// Source: bg_random_player.cpp
// -----------------------------------------------------------------------------
// Random move-choice kernel used by the game engine.
#include "bg_random_player.h"

#include <cstdint>
#include <random>
#include <stdexcept>

namespace {

// Random-selection helpers share one RNG initializer so exported entry points
// get the same seed semantics as the game engine.
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

MoveSequence choose_random_move_sequence(const std::vector<MoveSequence>& legal_moves, std::mt19937& rng) {
  // Uniformly sample one legal move sequence from the generated move set.
  if (legal_moves.empty()) {
    throw std::range_error("Cannot choose a move from an empty legal-move set.");
  }

  std::uniform_int_distribution<int> dist(0, static_cast<int>(legal_moves.size()) - 1);
  return legal_moves[dist(rng)];
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::List bg_cpp_random_move_choice(const Rcpp::List& legal_moves, const int seed, const bool use_seed) {
  // Exported helper used by the R random-player wrapper.
  const std::vector<backgammonr::MoveSequence> parsed_moves = backgammonr::parse_move_sequence_vector(legal_moves);
  std::mt19937 rng = init_rng(seed, use_seed);
  return backgammonr::move_sequence_to_list(backgammonr::choose_random_move_sequence(parsed_moves, rng));
}

// -----------------------------------------------------------------------------
// Source: bg_heuristic.cpp
// -----------------------------------------------------------------------------
// Heuristic board scoring, feature extraction, and move-choice kernels.
#include "bg_heuristic.h"

#include <limits>
#include <sstream>
#include <stdexcept>

#include "bg_game.h"
#include "bg_rules.h"

namespace {

// Heuristic kernels use explicit player validation because many helper
// functions are also called from exported entry points.
void validate_player(const int player) {
  if (player != 1 && player != -1) {
    throw std::range_error("`player` must be either 1L or -1L.");
  }
}

void validate_heuristic_selection(const std::string& selection) {
  // The heuristic layer intentionally exposes only two hand-crafted scoring
  // systems so the game engine can treat them as named strategies.
  if (selection != "aggressive" && selection != "defensive") {
    throw std::range_error("`selection` must be either \"aggressive\" or \"defensive\".");
  }
}

int pip_distance_to_off(const int player, const int point) {
  // Convert a board coordinate into remaining pip distance for one player.
  validate_player(player);

  if (point < 1 || point > backgammonr::kNumPoints) {
    throw std::range_error("`point` must be between 1 and 24.");
  }

  if (player == 1) {
    return point;
  }

  return backgammonr::kOffPosition - point;
}

bool point_has_player_contact(const backgammonr::BoardState& board, const int player, const int point) {
  // Contact means an opposing checker still lies ahead in the race, so the
  // checker can still be hit or can hit later.
  validate_player(player);

  if (player == 1) {
    for (int opponent_point = 1; opponent_point < point; ++opponent_point) {
      if (backgammonr::opponent_checker_count_on_point(board, player, opponent_point) > 0) {
        return true;
      }
    }
    return false;
  }

  for (int opponent_point = point + 1; opponent_point <= backgammonr::kNumPoints; ++opponent_point) {
    if (backgammonr::opponent_checker_count_on_point(board, player, opponent_point) > 0) {
      return true;
    }
  }

  return false;
}

bool blot_is_directly_hittable_by_die(
    const backgammonr::BoardState& board,
    const int attacker,
    const int target_point,
    const int die) {
  // Check direct one-die hits only; the heuristic summaries intentionally keep
  // this measure lightweight.
  validate_player(attacker);

  const int defender = -attacker;
  if (backgammonr::player_checker_count_on_point(board, defender, target_point) != 1) {
    return false;
  }

  if (backgammonr::player_has_bar_checkers(board, attacker)) {
    return backgammonr::bar_entry_point(attacker, die) == target_point;
  }

  const int source_point = attacker == 1 ? target_point + die : target_point - die;
  if (source_point < 1 || source_point > backgammonr::kNumPoints) {
    return false;
  }

  return backgammonr::player_checker_count_on_point(board, attacker, source_point) > 0;
}

int count_direct_shot_dice_against_blots(const backgammonr::BoardState& board, const int attacker) {
  // Count how many distinct die faces immediately hit an opposing blot.
  validate_player(attacker);

  int count = 0;
  const int defender = -attacker;

  for (int target_point = 1; target_point <= backgammonr::kNumPoints; ++target_point) {
    if (backgammonr::player_checker_count_on_point(board, defender, target_point) != 1) {
      continue;
    }

    for (int die = backgammonr::kMinDieValue; die <= backgammonr::kMaxDieValue; ++die) {
      if (blot_is_directly_hittable_by_die(board, attacker, target_point, die)) {
        ++count;
      }
    }
  }

  return count;
}

backgammonr::BoardState apply_sequence_without_full_validation(
    const backgammonr::BoardState& board,
    const backgammonr::MoveSequence& sequence) {
  // The heuristic scorer evaluates legal candidates only, so it can skip the
  // slower legality checks when constructing successor states.
  backgammonr::BoardState out = board;
  for (const backgammonr::MoveStep& step : sequence.steps) {
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  }
  out.turn = -sequence.player;
  return out;
}

}  // namespace

namespace backgammonr {

BoardFeatures extract_board_features(const BoardState& board, const int player) {
  // Compute one compact feature bundle reused by both heuristic scorers and
  // by the R-facing board-feature API.
  validate_player(player);

  BoardFeatures features;
  const int own_index = player_index(player);
  const int opponent_index_value = opponent_index(player);

  features.own_bar = board.bar[own_index];
  features.opponent_bar = board.bar[opponent_index_value];
  features.own_off = board.off[own_index];
  features.opponent_off = board.off[opponent_index_value];
  features.own_direct_hit_opportunities = count_direct_shot_dice_against_blots(board, player);
  features.own_direct_hit_risk = count_direct_shot_dice_against_blots(board, -player);
  features.own_pip_count = features.own_bar * 25;

  for (int point = 1; point <= kNumPoints; ++point) {
    const int own_count = player_checker_count_on_point(board, player, point);
    const int opp_count = opponent_checker_count_on_point(board, player, point);

    if (own_count == 1) {
      ++features.own_blots;
    }

    if (opp_count == 1) {
      ++features.opponent_blots;
    }

    if (own_count >= 2) {
      ++features.own_made_points;
      if (is_home_point(player, point)) {
        ++features.own_home_made_points;
      }
    }

    if (own_count > 0 && point_has_player_contact(board, player, point)) {
      features.own_contact_checkers += own_count;
    }

    features.own_pip_count += own_count * pip_distance_to_off(player, point);
  }

  return features;
}

double aggressive_board_score(const BoardState& board, const int player) {
  // Aggressive scoring values contact, hitting opportunities, and opponent bar
  // pressure more heavily than pure racing progress.
  const BoardFeatures f = extract_board_features(board, player);

  return
      120.0 * static_cast<double>(f.opponent_bar) +
      12.0 * static_cast<double>(f.own_direct_hit_opportunities) +
      8.0 * static_cast<double>(f.own_made_points) +
      3.0 * static_cast<double>(f.own_home_made_points) +
      2.0 * static_cast<double>(f.own_contact_checkers) +
      5.0 * static_cast<double>(f.own_off) -
      15.0 * static_cast<double>(f.own_bar) -
      3.0 * static_cast<double>(f.own_blots) -
      1.5 * static_cast<double>(f.own_direct_hit_risk) -
      0.05 * static_cast<double>(f.own_pip_count);
}

double defensive_board_score(const BoardState& board, const int player) {
  // Defensive scoring prioritizes making points, reducing blots, and limiting
  // immediate shot risk.
  const BoardFeatures f = extract_board_features(board, player);

  return
      10.0 * static_cast<double>(f.own_made_points) +
      6.0 * static_cast<double>(f.own_home_made_points) +
      4.0 * static_cast<double>(f.own_off) +
      2.0 * static_cast<double>(f.opponent_bar) -
      20.0 * static_cast<double>(f.own_bar) -
      15.0 * static_cast<double>(f.own_blots) -
      20.0 * static_cast<double>(f.own_direct_hit_risk) -
      3.0 * static_cast<double>(f.own_contact_checkers) -
      0.10 * static_cast<double>(f.own_pip_count);
}

double heuristic_board_score(const BoardState& board, const int player, const std::string& selection) {
  // Dispatch named heuristic families through one entry point.
  validate_heuristic_selection(selection);

  if (selection == "aggressive") {
    return aggressive_board_score(board, player);
  }

  return defensive_board_score(board, player);
}

MoveSequence choose_best_heuristic_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& selection) {
  // Score each legal successor board and return the highest-scoring move.
  validate_heuristic_selection(selection);

  if (legal_moves.empty()) {
    throw std::range_error("Cannot choose a move from an empty legal-move set.");
  }

  double best_score = -std::numeric_limits<double>::infinity();
  int best_index = -1;

  for (int i = 0; i < static_cast<int>(legal_moves.size()); ++i) {
    const MoveSequence& candidate = legal_moves[i];
    const BoardState board_after = apply_sequence_without_full_validation(board, candidate);
    const double score = heuristic_board_score(board_after, candidate.player, selection);

    if (best_index < 0 || score > best_score) {
      best_score = score;
      best_index = i;
    }
  }

  return legal_moves[best_index];
}

Rcpp::List board_features_to_list(const BoardFeatures& features) {
  // Convert the compact feature struct into a named list for R.
  return Rcpp::List::create(
    Rcpp::_["own_bar"] = Rcpp::IntegerVector::create(features.own_bar),
    Rcpp::_["opponent_bar"] = Rcpp::IntegerVector::create(features.opponent_bar),
    Rcpp::_["own_off"] = Rcpp::IntegerVector::create(features.own_off),
    Rcpp::_["opponent_off"] = Rcpp::IntegerVector::create(features.opponent_off),
    Rcpp::_["own_blots"] = Rcpp::IntegerVector::create(features.own_blots),
    Rcpp::_["opponent_blots"] = Rcpp::IntegerVector::create(features.opponent_blots),
    Rcpp::_["own_made_points"] = Rcpp::IntegerVector::create(features.own_made_points),
    Rcpp::_["own_home_made_points"] = Rcpp::IntegerVector::create(features.own_home_made_points),
    Rcpp::_["own_direct_hit_opportunities"] = Rcpp::IntegerVector::create(features.own_direct_hit_opportunities),
    Rcpp::_["own_direct_hit_risk"] = Rcpp::IntegerVector::create(features.own_direct_hit_risk),
    Rcpp::_["own_contact_checkers"] = Rcpp::IntegerVector::create(features.own_contact_checkers),
    Rcpp::_["own_pip_count"] = Rcpp::IntegerVector::create(features.own_pip_count)
  );
}

}  // namespace backgammonr

// [[Rcpp::export]]
double bg_cpp_heuristic_board_score(
    const Rcpp::List& board,
    const int player,
    const std::string& selection) {
  // Exported scalar score for one board/player pair.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  return backgammonr::heuristic_board_score(parsed_board, player, selection);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_heuristic_board_features(
    const Rcpp::List& board,
    const int player) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  return backgammonr::board_features_to_list(backgammonr::extract_board_features(parsed_board, player));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_heuristic_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const std::string& selection) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves = backgammonr::parse_move_sequence_vector(legal_moves);
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_best_heuristic_move_sequence(parsed_board, parsed_moves, selection));
}

// -----------------------------------------------------------------------------
// Source: bg_thompson_rollout.cpp
// -----------------------------------------------------------------------------
// Thompson-rollout move evaluation and move-choice kernels.
#include "bg_thompson_rollout.h"

#include <stdexcept>
#include <vector>

#include "alloc_interface.h"
#include "bg_rng.h"

// -----------------------------------------------------------------------------
// bg_thompson_rollout.cpp
//
// Thompson-specific rollout wrappers.
//
// Why this file exists:
// - R users often want a dedicated "Thompson rollout" entry point.
// - Internally, we still route to the common allocation engine in
//   bg_allocation.cpp to avoid duplicated logic.
// - Here, we only fix method = "thompson" and shape the returned fields.
// -----------------------------------------------------------------------------

namespace backgammonr {

// Function: evaluate_thompson_rollout_move_sequences
// Purpose: Evaluate legal moves with Thompson allocation and return compact
// Thompson-focused summaries.
// Called by: Thompson-specific R wrappers and unit tests.
// Notes: Delegates to shared allocation engine to avoid duplicated logic.
std::vector<ThompsonRolloutMoveSummary> evaluate_thompson_rollout_move_sequences(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Step 1: run shared allocator with method fixed to "thompson".
  // Step 2: map generic summaries into a Thompson-specific result struct.
  const std::vector<ActionEvaluationSummary> summaries =
      evaluate_move_sequences_with_allocation(board, legal_moves, "thompson", config, rng);

  std::vector<ThompsonRolloutMoveSummary> out;
  out.reserve(summaries.size());

  for (const ActionEvaluationSummary& summary : summaries) {
    // Keep this mapping explicit so each output field is easy to audit against
    // the generic `ActionEvaluationSummary` source.
    ThompsonRolloutMoveSummary row;
    // Preserve candidate identity.
    row.candidate_index = summary.candidate_index;
    // Keep allocation intensity for interpretability.
    row.allocation_count = summary.allocation_count;
    // Keep raw outcomes for diagnostics.
    row.wins = summary.wins;
    row.losses = summary.losses;
    row.unresolved = summary.unresolved;
    // Keep posterior sufficient statistics (Beta alpha/beta).
    row.alpha = summary.alpha;
    row.beta = summary.beta;
    // Posterior mean is the Thompson value estimate shown to users.
    row.posterior_mean = summary.estimate;
    // Empirical win rate gives a direct frequentist-style view.
    row.empirical_win_rate = summary.empirical_value;
    out.push_back(row);
  }

  return out;
}

// Function: choose_thompson_rollout_move_sequence
// Purpose: Choose one legal move under Thompson allocation.
// Called by: Thompson R choice wrapper and benchmark paths.
// Notes: Shares tie-breaking and posterior logic with generic allocator.
MoveSequence choose_thompson_rollout_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const RolloutConfig& config,
    std::mt19937& rng) {
  // Hard-code method = "thompson" and defer core selection to shared chooser.
  return choose_move_sequence_with_allocation(board, legal_moves, "thompson", config, rng);
}

// Function: thompson_rollout_move_summaries_to_data_frame
// Purpose: Convert Thompson summary structs into a stable R data-frame schema.
// Called by: Thompson evaluate wrappers and R-facing output helpers.
// Notes: Keeps column names and ordering stable for docs/examples/tests.
Rcpp::DataFrame thompson_rollout_move_summaries_to_data_frame(
    const std::vector<ThompsonRolloutMoveSummary>& summaries) {
  const int n = static_cast<int>(summaries.size());
  Rcpp::IntegerVector candidate_index(n);
  Rcpp::IntegerVector allocation_count(n);
  Rcpp::IntegerVector wins(n);
  Rcpp::IntegerVector losses(n);
  Rcpp::IntegerVector unresolved(n);
  Rcpp::NumericVector alpha(n);
  Rcpp::NumericVector beta(n);
  Rcpp::NumericVector posterior_mean(n);
  Rcpp::NumericVector empirical_win_rate(n);

  for (int i = 0; i < n; ++i) {
    candidate_index[i] = summaries[i].candidate_index;
    allocation_count[i] = summaries[i].allocation_count;
    wins[i] = summaries[i].wins;
    losses[i] = summaries[i].losses;
    unresolved[i] = summaries[i].unresolved;
    alpha[i] = summaries[i].alpha;
    beta[i] = summaries[i].beta;
    posterior_mean[i] = summaries[i].posterior_mean;
    empirical_win_rate[i] = summaries[i].empirical_win_rate;
  }

  return Rcpp::DataFrame::create(
      Rcpp::_["candidate_index"] = candidate_index,
      Rcpp::_["allocation_count"] = allocation_count,
      Rcpp::_["wins"] = wins,
      Rcpp::_["losses"] = losses,
      Rcpp::_["unresolved"] = unresolved,
      Rcpp::_["alpha"] = alpha,
      Rcpp::_["beta"] = beta,
      Rcpp::_["posterior_mean"] = posterior_mean,
      Rcpp::_["empirical_win_rate"] = empirical_win_rate,
      Rcpp::_["stringsAsFactors"] = false);
}

}  // namespace backgammonr

// Function: bg_cpp_thompson_rollout_move_evaluate
// Purpose: Rcpp entry point for Thompson move evaluation (table output).
// Called by: R function `evaluate_actions_thompson()` through RcppExports.
// Notes: Returns standardized action-evaluation table used across methods.
// [[Rcpp::export]]
Rcpp::DataFrame bg_cpp_thompson_rollout_move_evaluate(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R lists/scalars into C++ engine structures.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build local RNG stream.
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  // Return the full standardized evaluation table for consistency with
  // evaluate_actions_thompson() R-side wrappers.
  return backgammonr::action_evaluation_summaries_to_data_frame(
      backgammonr::evaluate_move_sequences_with_allocation(
          parsed_board,
          parsed_moves,
          "thompson",
          config,
          rng));
}

// Function: bg_cpp_thompson_rollout_move_choice
// Purpose: Rcpp entry point for one Thompson-selected move.
// Called by: R function `choose_action_thompson()` through RcppExports.
// Notes: Uses same parsing/config pattern as evaluate wrapper for consistency.
// [[Rcpp::export]]
Rcpp::List bg_cpp_thompson_rollout_move_choice(
    const Rcpp::List& board,
    const Rcpp::List& legal_moves,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Parse R lists/scalars into C++ engine structures.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::MoveSequence> parsed_moves =
      backgammonr::parse_move_sequence_vector(legal_moves);
  const backgammonr::RolloutConfig config{rollout_budget, rollout_policy, max_rollout_turns};
  // Build local RNG stream.
  std::mt19937 rng = backgammonr::init_rng(seed, use_seed);

  // Compute Thompson-selected move and convert back to R representation.
  return backgammonr::move_sequence_to_list(
      backgammonr::choose_move_sequence_with_allocation(
          parsed_board,
          parsed_moves,
          "thompson",
          config,
          rng));
}

