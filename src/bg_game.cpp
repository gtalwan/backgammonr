// Turn- and game-level simulation kernels.
#include "bg_game.h"

#include <cstdint>
#include <random>
#include <sstream>
#include <stdexcept>

#include "bg_allocation.h"
#include "bg_heuristic.h"
#include "bg_movegen.h"
#include "bg_random_player.h"
#include "bg_rollout.h"
#include "bg_rules.h"
#include "bg_thompson_rollout.h"

namespace {

// The game layer validates players and RNG-sensitive selections at the outer
// boundary, then delegates to faster unchecked helpers internally.
void validate_player(const int player) {
  if (player != 1 && player != -1) {
    throw std::range_error("`player` must be either 1L or -1L.");
  }
}

bool selection_uses_randomness(const std::string& selection) {
  // Mirror the public selection registry so exported entry points know whether
  // to initialize an RNG.
  return backgammonr::selection_uses_randomness(selection);
}

bool player_has_checker_in_home_board(
    const backgammonr::BoardState& board,
    const int player,
    const int home_owner) {
  // Used for gammon/backgammon classification once the game ends.
  for (int point = 1; point <= backgammonr::kNumPoints; ++point) {
    if (!backgammonr::is_home_point(home_owner, point)) {
      continue;
    }
    if (backgammonr::player_checker_count_on_point(board, player, point) > 0) {
      return true;
    }
  }
  return false;
}

void validate_max_turns(const int max_turns) {
  // Negative limits are always invalid; zero is allowed for explicit
  // unresolved/truncated rollouts.
  if (max_turns < 0) {
    throw std::range_error("`max_turns` must be nonnegative.");
  }
}

std::mt19937 init_rng(const int seed, const bool use_seed) {
  // Match RNG semantics across exported random-turn and random-game helpers.
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

std::vector<backgammonr::DiceRoll> parse_roll_vector(const Rcpp::List& rolls) {
  // Parse a scripted roll sequence for deterministic game playback.
  std::vector<backgammonr::DiceRoll> out;
  out.reserve(rolls.size());

  for (int i = 0; i < rolls.size(); ++i) {
    SEXP roll_sexp = rolls[i];
    if (!Rf_isNewList(roll_sexp)) {
      std::ostringstream oss;
      oss << "`rolls[[" << (i + 1) << "]]` must be a roll-like list.";
      throw std::range_error(oss.str());
    }

    out.push_back(backgammonr::parse_roll_list(Rcpp::List(roll_sexp)));
  }

  return out;
}

const std::string& selection_for_player_unchecked(
    const int player,
    const std::string& player1_selection,
    const std::string& player2_selection) {
  // Cheap player-to-policy routing used inside game loops.
  return player == 1 ? player1_selection : player2_selection;
}

std::string shared_or_mixed_label(
    const std::string& player1_selection,
    const std::string& player2_selection) {
  // Label same-policy matchups with the policy name and mixed matchups with a
  // neutral tag for downstream summaries.
  if (player1_selection == player2_selection) {
    return player1_selection;
  }

  return "mixed";
}

backgammonr::BoardState apply_sequence_without_full_validation(
    const backgammonr::BoardState& board,
    const backgammonr::MoveSequence& sequence) {
  // Legal sequences from the move generator can be applied directly inside the
  // turn/game engine without re-validating each step.
  backgammonr::BoardState out = board;

  for (const backgammonr::MoveStep& step : sequence.steps) {
    backgammonr::apply_move_step_unchecked_inplace(out, sequence.player, step);
  }
  out.turn = -sequence.player;
  return out;
}

}  // namespace

namespace backgammonr {

bool board_is_terminal(const BoardState& board) {
  // A game ends as soon as either side has borne off all 15 checkers.
  return board.off[0] == kCheckersPerPlayer || board.off[1] == kCheckersPerPlayer;
}

int board_winner(const BoardState& board) {
  // Return +1, -1, or 0 for non-terminal boards.
  if (board.off[0] == kCheckersPerPlayer && board.off[1] == kCheckersPerPlayer) {
    throw std::range_error("Invalid terminal board: both players cannot have borne off all checkers.");
  }

  if (board.off[0] == kCheckersPerPlayer) {
    return 1;
  }

  if (board.off[1] == kCheckersPerPlayer) {
    return -1;
  }

  return 0;
}

TerminalScoreClass terminal_score_class(const BoardState& board, const int perspective_player) {
  // Convert a terminal board into single/gammon/backgammon classes from one
  // player's perspective.
  validate_player(perspective_player);

  if (!board_is_terminal(board)) {
    return TerminalScoreClass::kUnresolved;
  }

  const int winner = board_winner(board);
  const int loser = -winner;
  const bool perspective_wins = winner == perspective_player;
  const int loser_index = player_index(loser);
  const bool loser_bore_off_any = board.off[loser_index] > 0;

  if (loser_bore_off_any) {
    return perspective_wins ? TerminalScoreClass::kSingleWin : TerminalScoreClass::kSingleLoss;
  }

  const bool loser_on_bar = board.bar[loser_index] > 0;
  const bool loser_in_winner_home = player_has_checker_in_home_board(board, loser, winner);
  const bool backgammon = loser_on_bar || loser_in_winner_home;

  if (perspective_wins) {
    return backgammon ? TerminalScoreClass::kBackgammonWin : TerminalScoreClass::kGammonWin;
  }

  return backgammon ? TerminalScoreClass::kBackgammonLoss : TerminalScoreClass::kGammonLoss;
}

std::string terminal_score_class_label(const TerminalScoreClass score_class) {
  // Public string labels shared by the rollout and truth layers.
  switch (score_class) {
    case TerminalScoreClass::kSingleLoss:
      return "single_loss";
    case TerminalScoreClass::kGammonLoss:
      return "gammon_loss";
    case TerminalScoreClass::kBackgammonLoss:
      return "backgammon_loss";
    case TerminalScoreClass::kUnresolved:
      return "unresolved";
    case TerminalScoreClass::kSingleWin:
      return "single_win";
    case TerminalScoreClass::kGammonWin:
      return "gammon_win";
    case TerminalScoreClass::kBackgammonWin:
      return "backgammon_win";
  }

  throw std::range_error("Unsupported terminal score class.");
}

BoardState apply_move_sequence_to_board(const BoardState& board, const MoveSequence& sequence) {
  // Checked full-turn application used by the exported move-application API.
  validate_player(sequence.player);

  if (board_is_terminal(board)) {
    throw std::range_error("Cannot apply a move sequence to a terminal board.");
  }

  if (board.turn != sequence.player) {
    throw std::range_error("`board$turn` must match the acting player in `move_sequence`.");
  }

  if (!sequence.roll.has_value()) {
    throw std::range_error("`move_sequence` must include a `roll` field to verify full-turn legality.");
  }

  if (sequence.steps.empty()) {
    throw std::range_error("`move_sequence` must contain at least one step.");
  }

  const std::vector<MoveSequence> legal_moves =
      generate_legal_move_sequences(board, sequence.player, sequence.roll.value());

  bool found_match = false;
  MoveSequence canonical_match;
  for (const MoveSequence& legal_move : legal_moves) {
    if (move_sequences_equal(legal_move, sequence)) {
      canonical_match = legal_move;
      found_match = true;
      break;
    }
  }

  if (!found_match) {
    throw std::range_error("`move_sequence` is not legal for the supplied board and roll.");
  }

  return apply_sequence_without_full_validation(board, canonical_match);
}

MoveSequence choose_move_sequence(
    const BoardState& board,
    const std::vector<MoveSequence>& legal_moves,
    const std::string& selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // Route one legal-move set through the named selection policy.
  validate_selection(selection);

  if (legal_moves.empty()) {
    throw std::range_error("Cannot choose a move from an empty legal-move set.");
  }

  if (selection == "first" || legal_moves.size() == 1U) {
    return legal_moves.front();
  }

  if (selection == "aggressive" || selection == "defensive") {
    return choose_best_heuristic_move_sequence(board, legal_moves, selection);
  }

  if (selection == "rollout" ||
      selection == "equal_rollout" ||
      selection == "greedy_rollout" ||
      selection == "ucb_rollout" ||
      selection == "thompson_rollout" ||
      selection == "ttts_rollout" ||
      selection == "ocba_rollout") {
    if (rng == nullptr) {
      throw std::range_error("Rollout-family move selection requires an RNG.");
    }

    return choose_move_sequence_with_allocation(board, legal_moves, selection, rollout_config, *rng);
  }

  if (rng == nullptr) {
    throw std::range_error("Random move selection requires an RNG.");
  }

  return choose_random_move_sequence(legal_moves, *rng);
}

TurnResult play_turn_with_roll(
    const BoardState& board,
    const DiceRoll& roll,
    const std::string& selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // Core turn kernel: generate legal moves, choose one under the requested
  // policy, then advance the board by one full turn.
  validate_selection(selection);

  if (board_is_terminal(board)) {
    throw std::range_error("Cannot play a turn from a terminal board.");
  }

  validate_player(board.turn);

  TurnResult result;
  result.board_before = board;
  result.player = board.turn;
  result.roll = roll;
  result.selection = selection;
  result.legal_moves = generate_legal_move_sequences(board, result.player, roll);

  if (result.legal_moves.empty()) {
    result.turn_passed = true;
    result.chosen_move = std::nullopt;
    result.board_after = board;
    result.board_after.turn = -result.player;
  } else {
    result.turn_passed = false;
    result.chosen_move = choose_move_sequence(board, result.legal_moves, selection, rng, rollout_config);
    result.board_after = apply_sequence_without_full_validation(board, result.chosen_move.value());
  }

  result.game_over = board_is_terminal(result.board_after);
  result.winner = board_winner(result.board_after);
  return result;
}

TurnResult play_turn_random(const BoardState& board, std::mt19937& rng, const std::string& selection, const RolloutConfig& rollout_config) {
  // Convenience wrapper for one turn with a fresh IID roll.
  return play_turn_with_roll(board, roll_dice(rng), selection, &rng, rollout_config);
}

GameResult play_game_random(
    const BoardState& initial_board,
    const int max_turns,
    std::mt19937& rng,
    const std::string& selection,
    const RolloutConfig& rollout_config) {
  // Same-policy matchup convenience wrapper.
  return play_game_random_matchup(initial_board, max_turns, rng, selection, selection, rollout_config);
}

GameResult play_game_with_rolls(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    const int max_turns,
    const std::string& selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // Same-policy scripted-roll convenience wrapper.
  return play_game_with_rolls_matchup(initial_board, rolls, max_turns, selection, selection, rng, rollout_config);
}

GameResult play_game_random_matchup(
    const BoardState& initial_board,
    const int max_turns,
    std::mt19937& rng,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const RolloutConfig& rollout_config) {
  // Simulate a full game under IID rolls and possibly different player
  // selection policies.
  validate_selection(player1_selection);
  validate_selection(player2_selection);
  validate_max_turns(max_turns);

  GameResult result;
  result.initial_board = initial_board;
  result.final_board = initial_board;
  result.selection = shared_or_mixed_label(player1_selection, player2_selection);
  result.player1_selection = player1_selection;
  result.player2_selection = player2_selection;
  result.used_scripted_rolls = false;

  if (board_is_terminal(initial_board)) {
    result.game_over = true;
    result.winner = board_winner(initial_board);
    return result;
  }

  BoardState current = initial_board;
  for (int turn_index = 0; turn_index < max_turns; ++turn_index) {
    const std::string& turn_selection =
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    TurnResult turn_result = play_turn_random(current, rng, turn_selection, rollout_config);
    result.turns.push_back(turn_result);
    current = turn_result.board_after;

    if (board_is_terminal(current)) {
      break;
    }
  }

  result.final_board = current;
  result.n_turns = static_cast<int>(result.turns.size());
  result.game_over = board_is_terminal(current);
  result.winner = board_winner(current);
  result.turn_limit_reached = !result.game_over && result.n_turns == max_turns;
  return result;
}

GameResult play_game_with_rolls_matchup(
    const BoardState& initial_board,
    const std::vector<DiceRoll>& rolls,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    std::mt19937* rng,
    const RolloutConfig& rollout_config) {
  // Simulate a full game against a scripted roll sequence, optionally with RNG
  // only for random/rollout-based move choice.
  validate_selection(player1_selection);
  validate_selection(player2_selection);
  validate_max_turns(max_turns);

  GameResult result;
  result.initial_board = initial_board;
  result.final_board = initial_board;
  result.selection = shared_or_mixed_label(player1_selection, player2_selection);
  result.player1_selection = player1_selection;
  result.player2_selection = player2_selection;
  result.used_scripted_rolls = true;

  if (board_is_terminal(initial_board)) {
    result.game_over = true;
    result.winner = board_winner(initial_board);
    return result;
  }

  BoardState current = initial_board;
  int turn_index = 0;
  for (; turn_index < max_turns; ++turn_index) {
    if (turn_index >= static_cast<int>(rolls.size())) {
      result.roll_sequence_exhausted = true;
      break;
    }

    const std::string& turn_selection =
        selection_for_player_unchecked(current.turn, player1_selection, player2_selection);
    TurnResult turn_result = play_turn_with_roll(current, rolls[turn_index], turn_selection, rng, rollout_config);
    result.turns.push_back(turn_result);
    current = turn_result.board_after;

    if (board_is_terminal(current)) {
      break;
    }
  }

  result.final_board = current;
  result.n_turns = static_cast<int>(result.turns.size());
  result.game_over = board_is_terminal(current);
  result.winner = board_winner(current);
  result.turn_limit_reached = !result.game_over && !result.roll_sequence_exhausted && result.n_turns == max_turns;
  return result;
}

Rcpp::List turn_result_to_list(const TurnResult& result) {
  // Normalize one turn result for the R-facing API and diagnostics.
  Rcpp::List legal_moves(result.legal_moves.size());
  for (int i = 0; i < static_cast<int>(result.legal_moves.size()); ++i) {
    legal_moves[i] = move_sequence_to_list(result.legal_moves[i]);
  }

  SEXP chosen_move = R_NilValue;
  if (result.chosen_move.has_value()) {
    chosen_move = move_sequence_to_list(result.chosen_move.value());
  }

  return Rcpp::List::create(
    Rcpp::_["board_before"] = board_to_list(result.board_before),
    Rcpp::_["board_after"] = board_to_list(result.board_after),
    Rcpp::_["player"] = Rcpp::IntegerVector::create(result.player),
    Rcpp::_["roll"] = roll_to_list(result.roll),
    Rcpp::_["legal_moves"] = legal_moves,
    Rcpp::_["n_legal_moves"] = Rcpp::IntegerVector::create(static_cast<int>(result.legal_moves.size())),
    Rcpp::_["chosen_move"] = chosen_move,
    Rcpp::_["selection"] = Rcpp::CharacterVector::create(result.selection),
    Rcpp::_["turn_passed"] = Rcpp::LogicalVector::create(result.turn_passed),
    Rcpp::_["game_over"] = Rcpp::LogicalVector::create(result.game_over),
    Rcpp::_["winner"] = Rcpp::IntegerVector::create(result.winner)
  );
}

Rcpp::List game_result_to_list(const GameResult& result) {
  // Normalize a whole simulated game for the R-facing API.
  Rcpp::List turns(result.turns.size());
  for (int i = 0; i < static_cast<int>(result.turns.size()); ++i) {
    turns[i] = turn_result_to_list(result.turns[i]);
  }

  return Rcpp::List::create(
    Rcpp::_["initial_board"] = board_to_list(result.initial_board),
    Rcpp::_["final_board"] = board_to_list(result.final_board),
    Rcpp::_["turns"] = turns,
    Rcpp::_["game_over"] = Rcpp::LogicalVector::create(result.game_over),
    Rcpp::_["winner"] = Rcpp::IntegerVector::create(result.winner),
    Rcpp::_["n_turns"] = Rcpp::IntegerVector::create(result.n_turns),
    Rcpp::_["turn_limit_reached"] = Rcpp::LogicalVector::create(result.turn_limit_reached),
    Rcpp::_["used_scripted_rolls"] = Rcpp::LogicalVector::create(result.used_scripted_rolls),
    Rcpp::_["roll_sequence_exhausted"] = Rcpp::LogicalVector::create(result.roll_sequence_exhausted),
    Rcpp::_["selection"] = Rcpp::CharacterVector::create(result.selection),
    Rcpp::_["player1_selection"] = Rcpp::CharacterVector::create(result.player1_selection),
    Rcpp::_["player2_selection"] = Rcpp::CharacterVector::create(result.player2_selection)
  );
}

}  // namespace backgammonr

// [[Rcpp::export]]
std::string bg_cpp_terminal_score_class(const Rcpp::List& board, const int perspective_player) {
  // Exported terminal-score classifier used by rollout summarizers.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  return backgammonr::terminal_score_class_label(
    backgammonr::terminal_score_class(parsed_board, perspective_player)
  );
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_apply_move_sequence(const Rcpp::List& board, const Rcpp::List& move_sequence) {
  // Exported checked move application entry point.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::MoveSequence parsed_sequence = backgammonr::parse_move_sequence_list(move_sequence);
  return backgammonr::board_to_list(backgammonr::apply_move_sequence_to_board(parsed_board, parsed_sequence));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_turn_with_roll(
    const Rcpp::List& board,
    const Rcpp::List& roll,
    const std::string& selection,
    const int seed,
    const bool use_seed) {
  // Exported single-turn simulator with an explicit root roll.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  if (selection_uses_randomness(selection)) {
    rng = init_rng(seed, use_seed);
    rng_ptr = &rng;
  }

  return backgammonr::turn_result_to_list(
      backgammonr::play_turn_with_roll(parsed_board, parsed_roll, selection, rng_ptr, backgammonr::RolloutConfig()));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_turn_random(
    const Rcpp::List& board,
    const std::string& selection,
    const int seed,
    const bool use_seed) {
  // Exported single-turn simulator with a fresh IID roll.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::turn_result_to_list(backgammonr::play_turn_random(parsed_board, rng, selection, backgammonr::RolloutConfig()));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_random(
    const Rcpp::List& board,
    const int max_turns,
    const std::string& selection,
    const int seed,
    const bool use_seed) {
  // Exported same-policy random-roll game simulator.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::game_result_to_list(
      backgammonr::play_game_random(parsed_board, max_turns, rng, selection, backgammonr::RolloutConfig()));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_scripted(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int max_turns,
    const std::string& selection,
    const int seed,
    const bool use_seed) {
  // Exported same-policy scripted-roll game simulator.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  if (selection_uses_randomness(selection)) {
    rng = init_rng(seed, use_seed);
    rng_ptr = &rng;
  }

  return backgammonr::game_result_to_list(
      backgammonr::play_game_with_rolls(parsed_board, parsed_rolls, max_turns, selection, rng_ptr, backgammonr::RolloutConfig()));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_matchup_random(
    const Rcpp::List& board,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // Exported two-policy matchup simulator under IID rolls.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);

  return backgammonr::game_result_to_list(
      backgammonr::play_game_random_matchup(parsed_board, max_turns, rng, player1_selection, player2_selection, backgammonr::RolloutConfig()));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_matchup_scripted(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int seed,
    const bool use_seed) {
  // Exported two-policy matchup simulator under scripted rolls.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);

  std::mt19937 rng;
  std::mt19937* rng_ptr = nullptr;
  if (selection_uses_randomness(player1_selection) || selection_uses_randomness(player2_selection)) {
    rng = init_rng(seed, use_seed);
    rng_ptr = &rng;
  }

  return backgammonr::game_result_to_list(
      backgammonr::play_game_with_rolls_matchup(
          parsed_board,
          parsed_rolls,
          max_turns,
          player1_selection,
          player2_selection,
          rng_ptr,
          backgammonr::RolloutConfig()));
}


// [[Rcpp::export]]
Rcpp::List bg_cpp_play_turn_with_roll_rollout(
    const Rcpp::List& board,
    const Rcpp::List& roll,
    const std::string& selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  // Exported turn simulator that evaluates rollout-family move selectors.
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::turn_result_to_list(
      backgammonr::play_turn_with_roll(parsed_board, parsed_roll, selection, &rng, rollout_config));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_turn_random_rollout(
    const Rcpp::List& board,
    const std::string& selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::turn_result_to_list(
      backgammonr::play_turn_random(parsed_board, rng, selection, rollout_config));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_random_rollout(
    const Rcpp::List& board,
    const int max_turns,
    const std::string& selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::game_result_to_list(
      backgammonr::play_game_random(parsed_board, max_turns, rng, selection, rollout_config));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_scripted_rollout(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int max_turns,
    const std::string& selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::game_result_to_list(
      backgammonr::play_game_with_rolls(parsed_board, parsed_rolls, max_turns, selection, &rng, rollout_config));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_matchup_random_rollout(
    const Rcpp::List& board,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::game_result_to_list(
      backgammonr::play_game_random_matchup(
          parsed_board,
          max_turns,
          rng,
          player1_selection,
          player2_selection,
          rollout_config));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_play_game_matchup_scripted_rollout(
    const Rcpp::List& board,
    const Rcpp::List& rolls,
    const int max_turns,
    const std::string& player1_selection,
    const std::string& player2_selection,
    const int rollout_budget,
    const std::string& rollout_policy,
    const int max_rollout_turns,
    const int seed,
    const bool use_seed) {
  const backgammonr::BoardState parsed_board = backgammonr::parse_board_list(board);
  const std::vector<backgammonr::DiceRoll> parsed_rolls = parse_roll_vector(rolls);
  std::mt19937 rng = init_rng(seed, use_seed);
  const backgammonr::RolloutConfig rollout_config{rollout_budget, rollout_policy, max_rollout_turns};

  return backgammonr::game_result_to_list(
      backgammonr::play_game_with_rolls_matchup(
          parsed_board,
          parsed_rolls,
          max_turns,
          player1_selection,
          player2_selection,
          &rng,
          rollout_config));
}
