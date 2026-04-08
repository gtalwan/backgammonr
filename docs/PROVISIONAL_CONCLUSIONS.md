# Provisional Conclusions

This note records the current research-minded conclusions that the package can
already support, even before the final high-budget study battery is complete.

## What We Can Already Say

- The package's scientific object is clear: rollout-model move value under
  random continuation play, not expert backgammon truth.
- Opening states are heterogeneous. Even the current pilot artifacts show that
  some doubles and some non-doubles are effectively near-tie states under the
  rollout model.
- Ranking recovery is worth central emphasis. Top-1 agreement alone hides too
  much of what matters in small-gap states.
- Canonical TS looks directionally promising on non-double openings in the
  current pilot comparison.
- TTTS looks most promising as a targeted response to ambiguity near the top of
  the ranking, not as a universal replacement for canonical TS.
- UCB is a useful optimism-based contrast, but it should be interpreted as a
  different method family rather than as “another TS option”.

## What The Current Model Layer Means

The current front door is explicit about:

- `reward_model = "scalar_payoff"`
- `posterior_model = "beta_pseudo"`

The main richer alternative already implemented is:

- `reward_model = "categorical_outcome"`
- `posterior_model = "dirichlet_multinomial"`

That makes the next coherent extensions clearer:

- bounded categorical outcome modeling via Dirichlet-multinomial TS;
- scalar equity summaries with Gaussian approximations only if the reward
  definition is changed accordingly.

## What Still Needs Heavy Compute

- a stable high-budget opening truth battery for all 21 unordered opening rolls;
- larger repeated-seed TS, TTTS, and UCB opening studies;
- broader state batteries beyond openings;
- CRN studies on hard near-tie states;
- direct evaluation of richer posterior families once their reward definitions
  are implemented coherently.
