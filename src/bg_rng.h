#ifndef BACKGAMMONR_BG_RNG_H
#define BACKGAMMONR_BG_RNG_H

#include <cstdint>
#include <random>
#include <stdexcept>

namespace backgammonr {

// Build a deterministic RNG when `use_seed` is requested, otherwise seed from
// system entropy. Keeping this inline avoids repeating the same validation and
// seeding boilerplate across multiple translation units.
inline std::mt19937 init_rng(const int seed, const bool use_seed) {
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

}  // namespace backgammonr

#endif
