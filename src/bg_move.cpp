// Move-step and move-sequence construction kernels.
#include "bg_move.h"

#include <array>
#include <sstream>
#include <stdexcept>

namespace {

// Move parsing mirrors the public R list contract exactly, so the C++ kernels
// can reject malformed steps and sequences before simulation starts.
bool is_integer_vector_sexp(const SEXP x) {
  return TYPEOF(x) == INTSXP;
}

bool is_logical_vector_sexp(const SEXP x) {
  // `hit` is stored as an explicit logical scalar in the R representation.
  return TYPEOF(x) == LGLSXP;
}

// Collect parser diagnostics across nested step/sequence structures.
std::string collapse_messages(const std::vector<std::string>& messages) {
  std::ostringstream oss;

  for (std::size_t i = 0; i < messages.size(); ++i) {
    if (i > 0) {
      oss << "\n";
    }
    oss << messages[i];
  }

  return oss.str();
}

bool has_named_element(const Rcpp::List& x, const char* name) {
  // Named-field checks keep error messages specific to the offending field.
  return x.containsElementNamed(name);
}

int get_scalar_int_checked(
    const Rcpp::List& x,
    const char* name,
    std::vector<std::string>& messages) {
  // Extract a required integer scalar while appending parse diagnostics to the
  // shared message vector.
  if (!has_named_element(x, name)) {
    messages.push_back(std::string("Missing required field `") + name + "`.");
    return 0;
  }

  SEXP raw = x[name];
  if (!is_integer_vector_sexp(raw)) {
    messages.push_back(std::string("`") + name + "` must be stored as an integer scalar.");
    return 0;
  }

  Rcpp::IntegerVector values(raw);
  if (values.size() != 1) {
    messages.push_back(std::string("`") + name + "` must have length 1.");
    return 0;
  }

  if (values[0] == NA_INTEGER) {
    messages.push_back(std::string("`") + name + "` cannot be `NA`.");
    return 0;
  }

  return values[0];
}

bool get_scalar_bool_checked(
    const Rcpp::List& x,
    const char* name,
    std::vector<std::string>& messages) {
  // Extract the logical `hit` flag with the same non-throwing validation style
  // used by the integer fields.
  if (!has_named_element(x, name)) {
    messages.push_back(std::string("Missing required field `") + name + "`.");
    return false;
  }

  SEXP raw = x[name];
  if (!is_logical_vector_sexp(raw)) {
    messages.push_back(std::string("`") + name + "` must be stored as a logical scalar.");
    return false;
  }

  Rcpp::LogicalVector values(raw);
  if (values.size() != 1) {
    messages.push_back(std::string("`") + name + "` must have length 1.");
    return false;
  }

  if (values[0] == NA_LOGICAL) {
    messages.push_back(std::string("`") + name + "` cannot be `NA`.");
    return false;
  }

  return static_cast<bool>(values[0]);
}

std::array<int, backgammonr::kMaxDieValue + 1> count_dice(const std::vector<int>& dice) {
  // Count repeated die values so move-sequence construction can verify that a
  // roll supplies enough copies of each die.
  std::array<int, backgammonr::kMaxDieValue + 1> counts{};

  for (const int die : dice) {
    if (die >= backgammonr::kMinDieValue && die <= backgammonr::kMaxDieValue) {
      ++counts[die];
    }
  }

  return counts;
}

}  // namespace

namespace backgammonr {

MoveStep make_move_step(const int from, const int to, const int die, const bool hit) {
  // Canonical checked constructor for one atomic move step.
  std::vector<std::string> messages;

  if (from < kBarPosition || from > kNumPoints) {
    std::ostringstream oss;
    oss << "`from` must be between " << kBarPosition << " and " << kNumPoints << ".";
    messages.push_back(oss.str());
  }

  if (to < 1 || to > kOffPosition) {
    std::ostringstream oss;
    oss << "`to` must be between 1 and " << kOffPosition << ".";
    messages.push_back(oss.str());
  }

  if (from == to) {
    messages.push_back("`from` and `to` must differ.");
  }

  if (die < kMinDieValue || die > kMaxDieValue) {
    std::ostringstream oss;
    oss << "`die` must be between " << kMinDieValue << " and " << kMaxDieValue << ".";
    messages.push_back(oss.str());
  }

  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  MoveStep step;
  step.from = from;
  step.to = to;
  step.die = die;
  step.hit = hit;
  return step;
}

Rcpp::List move_step_to_list(const MoveStep& step) {
  // Convert the compact MoveStep struct into the public R list shape.
  return Rcpp::List::create(
    Rcpp::_["from"] = Rcpp::IntegerVector::create(step.from),
    Rcpp::_["to"] = Rcpp::IntegerVector::create(step.to),
    Rcpp::_["die"] = Rcpp::IntegerVector::create(step.die),
    Rcpp::_["hit"] = Rcpp::LogicalVector::create(step.hit)
  );
}

std::vector<std::string> validate_move_step_list(const Rcpp::List& step) {
  // Validate a step object without throwing so callers can accumulate nested
  // diagnostics for complete move-sequence objects.
  std::vector<std::string> messages;
  const int from = get_scalar_int_checked(step, "from", messages);
  const int to = get_scalar_int_checked(step, "to", messages);
  const int die = get_scalar_int_checked(step, "die", messages);
  const bool hit = get_scalar_bool_checked(step, "hit", messages);
  (void) hit;

  if (!messages.empty()) {
    return messages;
  }

  try {
    make_move_step(from, to, die, hit);
  } catch (const std::range_error& e) {
    std::istringstream iss(e.what());
    std::string line;
    while (std::getline(iss, line)) {
      if (!line.empty()) {
        messages.push_back(line);
      }
    }
  }

  return messages;
}

MoveStep parse_move_step_list(const Rcpp::List& step) {
  // Parse one validated move-step list into the engine struct.
  const std::vector<std::string> messages = validate_move_step_list(step);
  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  const Rcpp::IntegerVector from = step["from"];
  const Rcpp::IntegerVector to = step["to"];
  const Rcpp::IntegerVector die = step["die"];
  const Rcpp::LogicalVector hit = step["hit"];

  return make_move_step(from[0], to[0], die[0], static_cast<bool>(hit[0]));
}

std::vector<MoveStep> parse_move_step_vector(const Rcpp::List& steps) {
  // Parse a list of move-step objects, preserving index-specific error labels.
  std::vector<std::string> messages;
  std::vector<MoveStep> out;
  out.reserve(steps.size());

  for (int i = 0; i < steps.size(); ++i) {
    SEXP step_sexp = steps[i];
    if (!Rf_isNewList(step_sexp)) {
      std::ostringstream oss;
      oss << "`steps[[" << (i + 1) << "]]` must be a list-like move step.";
      messages.push_back(oss.str());
      continue;
    }

    Rcpp::List step(step_sexp);
    const std::vector<std::string> step_messages = validate_move_step_list(step);

    if (!step_messages.empty()) {
      for (const std::string& msg : step_messages) {
        std::ostringstream oss;
        oss << "In `steps[[" << (i + 1) << "]]`: " << msg;
        messages.push_back(oss.str());
      }
      continue;
    }

    out.push_back(parse_move_step_list(step));
  }

  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  return out;
}

MoveSequence make_move_sequence(
    const int player,
    const std::vector<MoveStep>& steps,
    const std::optional<DiceRoll>& roll) {
  // Canonical checked constructor for one full-turn move sequence.
  std::vector<std::string> messages;

  if (player != 1 && player != -1) {
    messages.push_back("`player` must be either 1L or -1L.");
  }

  if (steps.empty()) {
    messages.push_back("`steps` must contain at least one move step.");
  }

  for (std::size_t i = 0; i < steps.size(); ++i) {
    try {
      make_move_step(steps[i].from, steps[i].to, steps[i].die, steps[i].hit);
    } catch (const std::range_error& e) {
      std::istringstream iss(e.what());
      std::string line;
      while (std::getline(iss, line)) {
        if (!line.empty()) {
          std::ostringstream oss;
          oss << "In step " << (i + 1) << ": " << line;
          messages.push_back(oss.str());
        }
      }
    }
  }

  if (roll.has_value()) {
    const Rcpp::IntegerVector expanded = expanded_roll(roll.value());
    std::vector<int> expanded_std(expanded.begin(), expanded.end());
    const std::array<int, kMaxDieValue + 1> available_counts = count_dice(expanded_std);

    std::vector<int> step_dice;
    step_dice.reserve(steps.size());
    for (const MoveStep& step : steps) {
      step_dice.push_back(step.die);
    }
    const std::array<int, kMaxDieValue + 1> used_counts = count_dice(step_dice);

    for (int die = kMinDieValue; die <= kMaxDieValue; ++die) {
      if (used_counts[die] > available_counts[die]) {
        std::ostringstream oss;
        oss << "Die value " << die << " is used " << used_counts[die]
            << " times but only " << available_counts[die]
            << " occurrence(s) are available in the supplied roll.";
        messages.push_back(oss.str());
      }
    }
  }

  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  MoveSequence sequence;
  sequence.player = player;
  sequence.steps = steps;
  sequence.roll = roll;
  return sequence;
}

std::vector<std::string> validate_move_sequence_list(const Rcpp::List& sequence) {
  // Validate the outer sequence object and then delegate step/roll validation
  // to the lower-level parsers above.
  std::vector<std::string> messages;

  const int player = get_scalar_int_checked(sequence, "player", messages);

  if (!has_named_element(sequence, "steps")) {
    messages.push_back("Missing required field `steps`.");
    return messages;
  }

  SEXP steps_raw = sequence["steps"];
  if (!Rf_isNewList(steps_raw)) {
    messages.push_back("`steps` must be a list of move-step objects.");
    return messages;
  }

  std::optional<DiceRoll> roll = std::nullopt;
  if (has_named_element(sequence, "roll")) {
    SEXP roll_raw = sequence["roll"];
    if (roll_raw != R_NilValue) {
      if (!Rf_isNewList(roll_raw)) {
        messages.push_back("`roll` must be a roll-like list when supplied.");
        return messages;
      }

      try {
        roll = parse_roll_list(Rcpp::List(roll_raw));
      } catch (const std::range_error& e) {
        std::istringstream iss(e.what());
        std::string line;
        while (std::getline(iss, line)) {
          if (!line.empty()) {
            std::ostringstream oss;
            oss << "In `roll`: " << line;
            messages.push_back(oss.str());
          }
        }
        return messages;
      }
    }
  }

  try {
    const std::vector<MoveStep> parsed_steps = parse_move_step_vector(Rcpp::List(steps_raw));
    (void) make_move_sequence(player, parsed_steps, roll);
  } catch (const std::range_error& e) {
    std::istringstream iss(e.what());
    std::string line;
    while (std::getline(iss, line)) {
      if (!line.empty()) {
        messages.push_back(line);
      }
    }
  }

  return messages;
}

MoveSequence parse_move_sequence_list(const Rcpp::List& sequence) {
  // Parse one validated move-sequence list into the engine struct.
  const std::vector<std::string> messages = validate_move_sequence_list(sequence);
  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  const Rcpp::IntegerVector player = sequence["player"];
  const std::vector<MoveStep> steps = parse_move_step_vector(sequence["steps"]);

  std::optional<DiceRoll> roll = std::nullopt;
  if (has_named_element(sequence, "roll")) {
    SEXP roll_raw = sequence["roll"];
    if (roll_raw != R_NilValue) {
      roll = parse_roll_list(Rcpp::List(roll_raw));
    }
  }

  return make_move_sequence(player[0], steps, roll);
}

std::vector<MoveSequence> parse_move_sequence_vector(const Rcpp::List& sequences) {
  // Parse a whole legal-move list while preserving per-element diagnostics.
  std::vector<std::string> messages;
  std::vector<MoveSequence> out;
  out.reserve(sequences.size());

  for (int i = 0; i < sequences.size(); ++i) {
    SEXP sequence_sexp = sequences[i];
    if (!Rf_isNewList(sequence_sexp)) {
      std::ostringstream oss;
      oss << "`legal_moves[[" << (i + 1) << "]]` must be a move-sequence-like list.";
      messages.push_back(oss.str());
      continue;
    }

    try {
      out.push_back(parse_move_sequence_list(Rcpp::List(sequence_sexp)));
    } catch (const std::range_error& e) {
      std::istringstream iss(e.what());
      std::string line;
      while (std::getline(iss, line)) {
        if (!line.empty()) {
          std::ostringstream oss;
          oss << "In `legal_moves[[" << (i + 1) << "]]`: " << line;
          messages.push_back(oss.str());
        }
      }
    }
  }

  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  return out;
}

bool move_sequences_equal(const MoveSequence& lhs, const MoveSequence& rhs) {
  // Equality is structural: player, optional roll, and each step must match.
  if (lhs.player != rhs.player) {
    return false;
  }

  if (lhs.roll.has_value() != rhs.roll.has_value()) {
    return false;
  }

  if (lhs.roll.has_value()) {
    if (lhs.roll.value().dice[0] != rhs.roll.value().dice[0] ||
        lhs.roll.value().dice[1] != rhs.roll.value().dice[1]) {
      return false;
    }
  }

  if (lhs.steps.size() != rhs.steps.size()) {
    return false;
  }

  for (std::size_t i = 0; i < lhs.steps.size(); ++i) {
    if (lhs.steps[i].from != rhs.steps[i].from ||
        lhs.steps[i].to != rhs.steps[i].to ||
        lhs.steps[i].die != rhs.steps[i].die ||
        lhs.steps[i].hit != rhs.steps[i].hit) {
      return false;
    }
  }

  return true;
}

Rcpp::List move_sequence_to_list(const MoveSequence& sequence) {
  // Convert a full move sequence into the normalized R representation used
  // throughout the package.
  Rcpp::List step_list(sequence.steps.size());
  Rcpp::IntegerVector dice_used(sequence.steps.size());

  for (int i = 0; i < static_cast<int>(sequence.steps.size()); ++i) {
    step_list[i] = move_step_to_list(sequence.steps[i]);
    dice_used[i] = sequence.steps[i].die;
  }

  SEXP roll = R_NilValue;
  if (sequence.roll.has_value()) {
    roll = roll_to_list(sequence.roll.value());
  }

  return Rcpp::List::create(
    Rcpp::_["player"] = Rcpp::IntegerVector::create(sequence.player),
    Rcpp::_["roll"] = roll,
    Rcpp::_["steps"] = step_list,
    Rcpp::_["dice_used"] = dice_used,
    Rcpp::_["n_steps"] = Rcpp::IntegerVector::create(static_cast<int>(sequence.steps.size()))
  );
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::List bg_cpp_move_step_create(const int from, const int to, const int die, const bool hit = false) {
  // Exported checked constructor for one move step.
  return backgammonr::move_step_to_list(backgammonr::make_move_step(from, to, die, hit));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_move_sequence_create(const int player, const Rcpp::List& steps) {
  // Exported constructor for a move sequence without an attached roll.
  const std::vector<backgammonr::MoveStep> parsed_steps = backgammonr::parse_move_step_vector(steps);
  const backgammonr::MoveSequence sequence = backgammonr::make_move_sequence(player, parsed_steps, std::nullopt);
  return backgammonr::move_sequence_to_list(sequence);
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_move_sequence_create_with_roll(
    const int player,
    const Rcpp::List& steps,
    const Rcpp::List& roll) {
  // Exported constructor that binds a full-turn roll to the move sequence.
  const std::vector<backgammonr::MoveStep> parsed_steps = backgammonr::parse_move_step_vector(steps);
  const backgammonr::DiceRoll parsed_roll = backgammonr::parse_roll_list(roll);
  const backgammonr::MoveSequence sequence = backgammonr::make_move_sequence(player, parsed_steps, parsed_roll);
  return backgammonr::move_sequence_to_list(sequence);
}
