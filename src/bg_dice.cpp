// Dice and roll generation kernels.
#include "bg_dice.h"

#include <cstdint>
#include <sstream>
#include <stdexcept>

namespace {

// Roll objects are simple named lists, so validation begins with explicit
// field-presence checks.
bool has_named_element(const Rcpp::List& x, const char* name) {
  return x.containsElementNamed(name);
}

// Dice are always carried as integer vectors in the public R representation.
bool is_integer_vector_sexp(const SEXP x) {
  return TYPEOF(x) == INTSXP;
}

// Collapse multi-issue validation into one readable R error message.
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

Rcpp::IntegerVector get_dice_checked(
    const Rcpp::List& roll,
    std::vector<std::string>& messages) {
  // Parse the two dice without throwing so validate_roll_list() can keep one
  // consistent validation pathway.
  if (!has_named_element(roll, "dice")) {
    messages.push_back("Missing required field `dice`.");
    return Rcpp::IntegerVector();
  }

  SEXP raw = roll["dice"];
  if (!is_integer_vector_sexp(raw)) {
    messages.push_back("`dice` must be stored as an integer vector.");
    return Rcpp::IntegerVector();
  }

  Rcpp::IntegerVector dice(raw);
  if (dice.size() != backgammonr::kNumDice) {
    std::ostringstream oss;
    oss << "`dice` must have length " << backgammonr::kNumDice << ".";
    messages.push_back(oss.str());
    return Rcpp::IntegerVector();
  }

  for (int i = 0; i < dice.size(); ++i) {
    if (dice[i] == NA_INTEGER) {
      messages.push_back("`dice` cannot contain `NA` values.");
      return Rcpp::IntegerVector();
    }
  }

  return dice;
}

}  // namespace

namespace backgammonr {

DiceRoll make_roll(const int die1, const int die2) {
  // Canonical roll constructor with strict 1..6 bounds.
  if (die1 < kMinDieValue || die1 > kMaxDieValue) {
    throw std::range_error("`die1` must be between 1 and 6.");
  }

  if (die2 < kMinDieValue || die2 > kMaxDieValue) {
    throw std::range_error("`die2` must be between 1 and 6.");
  }

  DiceRoll roll;
  roll.dice[0] = die1;
  roll.dice[1] = die2;
  return roll;
}

DiceRoll roll_dice(std::mt19937& rng) {
  // IID dice generator used by the game and rollout engines.
  std::uniform_int_distribution<int> dist(kMinDieValue, kMaxDieValue);
  return make_roll(dist(rng), dist(rng));
}

Rcpp::IntegerVector expanded_roll(const DiceRoll& roll) {
  // Doubles expand to four pips because one physical roll yields four legal
  // move steps in backgammon.
  const int expanded_size = roll.is_double() ? kDoubleExpandedDice : kNumDice;
  Rcpp::IntegerVector expanded(expanded_size);

  if (roll.is_double()) {
    for (int i = 0; i < expanded_size; ++i) {
      expanded[i] = roll.dice[0];
    }
  } else {
    expanded[0] = roll.dice[0];
    expanded[1] = roll.dice[1];
  }

  return expanded;
}

Rcpp::List roll_to_list(const DiceRoll& roll) {
  // Normalize to the public list shape with both compact and expanded forms.
  Rcpp::IntegerVector dice(kNumDice);
  dice[0] = roll.dice[0];
  dice[1] = roll.dice[1];

  Rcpp::LogicalVector is_double(1);
  is_double[0] = roll.is_double();

  return Rcpp::List::create(
    Rcpp::_["dice"] = dice,
    Rcpp::_["is_double"] = is_double,
    Rcpp::_["expanded"] = expanded_roll(roll)
  );
}

std::vector<std::string> validate_roll_list(const Rcpp::List& roll) {
  // Roll validation is intentionally strict because downstream move generation
  // assumes a complete and legal dice pair.
  std::vector<std::string> messages;
  const Rcpp::IntegerVector dice = get_dice_checked(roll, messages);

  if (!messages.empty()) {
    return messages;
  }

  for (int i = 0; i < dice.size(); ++i) {
    if (dice[i] < kMinDieValue || dice[i] > kMaxDieValue) {
      std::ostringstream oss;
      oss << "Die values must be between " << kMinDieValue
          << " and " << kMaxDieValue << ".";
      messages.push_back(oss.str());
      return messages;
    }
  }

  return messages;
}

DiceRoll parse_roll_list(const Rcpp::List& roll) {
  // Convert the validated roll list into the fixed DiceRoll struct.
  const std::vector<std::string> messages = validate_roll_list(roll);
  if (!messages.empty()) {
    throw std::range_error(collapse_messages(messages));
  }

  const Rcpp::IntegerVector dice = roll["dice"];
  return make_roll(dice[0], dice[1]);
}

}  // namespace backgammonr

// [[Rcpp::export]]
Rcpp::List bg_cpp_roll_create(const int die1, const int die2) {
  // Exported constructor for one explicit roll.
  return backgammonr::roll_to_list(backgammonr::make_roll(die1, die2));
}

// [[Rcpp::export]]
Rcpp::List bg_cpp_roll_dice(const int n, const int seed, const bool use_seed) {
  // Exported random roll generator used by the R helper layer and tests.
  if (n < 1) {
    throw std::range_error("`n` must be at least 1.");
  }

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

  Rcpp::List out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = backgammonr::roll_to_list(backgammonr::roll_dice(rng));
  }

  return out;
}
