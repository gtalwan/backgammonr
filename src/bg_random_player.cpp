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
