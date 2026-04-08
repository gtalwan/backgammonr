# Posterior Models First Pass

This package now separates:

- `reward_model`
- `posterior_model`

and enforces coherent pairings rather than comparing Beta to arbitrary distributions.

## Current coherent stacks

Under current package semantics, `scalar_payoff` is the bounded rollout reward
used by the engine, with unresolved rollouts mapped to `unresolved_value`
inside `[0, 1]`. It is not a generic continuous equity scale.

- `win_loss + beta_bernoulli`
- `win_loss + gaussian_approx`
- `win_loss + bootstrap`
- `categorical_outcome + dirichlet_multinomial`
- `categorical_outcome + bootstrap`
- `scalar_payoff + beta_pseudo`
- `scalar_payoff + gaussian_approx`
- `scalar_payoff + normal_inverse_gamma`
- `scalar_payoff + student_t_marginal`
- `scalar_payoff + bootstrap`

Important limitation:

- The rollout engine now emits scored terminal categories:
  - `single_loss`
  - `gammon_loss`
  - `backgammon_loss`
  - `unresolved`
  - `single_win`
  - `gammon_win`
  - `backgammon_win`
- So the categorical stack is now a scored Dirichlet model over explicit backgammon-style outcome classes, with unresolved rollouts kept as a separate truncation category.

## What was implemented

- C++ posterior kernels for:
  - `beta_bernoulli`
  - `beta_pseudo`
  - `dirichlet_multinomial`
  - `gaussian_approx`
  - `normal_inverse_gamma`
  - `student_t_marginal`
  - `bootstrap`
- A generic sequential TS driver that uses those kernels while preserving the existing fast C++ allocation engine for the legacy/default `scalar_payoff + beta_pseudo` path.
- Front-door comparison workflows:
  - `bg_compare_posteriors()`
  - `bg_compare_reward_models()`
- New plots:
  - `plot_bg_posterior_compare()`

Default comparison behavior:

- `bg_compare_posteriors()` now defaults to the recommended subset for the chosen reward model, not the full supported zoo.
- To compare the rougher secondary baselines as well, pass `posterior_models = bg_supported_posterior_models(reward_model)`.
- Scalar comparators such as UCB and equal allocation remain on the legacy
  scalar engine. They are not treated as generic comparators across all
  posterior families in this rescue pass.

## Pilot study

Saved pilot artifacts:

- `artifacts/pilot/posterior_compare_pilot_3_openings.rds`
- `artifacts/pilot/reward_compare_pilot_3_openings.rds`

Pilot design:

- 3 opening rolls: `1-6`, `3-1`, `6-6`
- budgets: `16`, `32`, `64`
- repeated seeds
- proxy references built once per state

## Provisional findings

### Scalar-payoff posterior comparison

At budget `16`:

- `bootstrap` had the lowest mean simple regret: `0.182`
- `student_t_marginal` next: `0.198`
- `beta_pseudo`: `0.204`
- `normal_inverse_gamma`: `0.248`

At budget `32`:

- `bootstrap` remained best on simple regret: `0.221`
- `student_t_marginal`: `0.235`
- `beta_pseudo`: `0.239`
- `normal_inverse_gamma`: `0.299`

At budget `64`:

- `student_t_marginal`: `0.205`
- `normal_inverse_gamma`: `0.211`
- `beta_pseudo`: `0.237`
- `bootstrap`: `0.259`

Runtime:

- `beta_pseudo` stayed clearly cheapest.
- In the pilot, the richer scalar models were roughly `1.8x` to `2.1x` slower than `beta_pseudo`.

Interpretation:

- `beta_pseudo` remains a strong default because it is fast and not obviously dominated.
- `student_t_marginal` looks like the most promising scalar-payoff alternative so far.
- `normal_inverse_gamma` does not currently justify itself as a separate default relative to `student_t_marginal`.
- `bootstrap` is interesting at very small budgets, but it looks less stable by budget `64`.

### Reward-model comparison

The pilot compared:

- `scalar_payoff + beta_pseudo`
- `categorical_outcome + dirichlet_multinomial`

At budget `16`:

- `scalar_payoff + beta_pseudo` had lower simple regret: `0.201` vs `0.249`
- `scalar_payoff + beta_pseudo` had higher top-1 match: `0.167` vs `0.000`
- `categorical_outcome + dirichlet_multinomial` had better mean Spearman: `0.101` vs `-0.138`
- `categorical_outcome + dirichlet_multinomial` had better weighted rank loss: `4.27` vs `4.66`

At budget `32`:

- `scalar_payoff + beta_pseudo` still had lower simple regret: `0.218` vs `0.235`
- `scalar_payoff + beta_pseudo` still had higher top-1 match: `0.250` vs `0.000`
- `categorical_outcome + dirichlet_multinomial` still had slightly better ranking metrics

Runtime:

- `dirichlet_multinomial` was slower than `beta_pseudo`, but not dramatically so in the pilot.

Interpretation:

- The scalar Beta-style default currently looks better for top-decision quality on this small pilot.
- The Dirichlet model is still scientifically important because it is more honest about the rollout outcome structure and may help ranking recovery.
- The right comparison is not “does Dirichlet immediately beat Beta on every metric?” but “does the richer categorical model buy better ranking or uncertainty behavior where that matters?”

## Recommendation after this pass

Keep:

- `scalar_payoff + beta_pseudo` as the fast default
- `categorical_outcome + dirichlet_multinomial` as the main richer backgammon-facing extension
- `scalar_payoff + student_t_marginal` as the most promising scalar alternative
- `bootstrap` as a robustness comparator, not a default

For a stricter audit of which models are exact, approximate, central, or
secondary, see [MODEL_AUDIT_TABLE.md](./MODEL_AUDIT_TABLE.md).

De-emphasize:

- `gaussian_approx` except as a rough sanity baseline
- `normal_inverse_gamma` as a separate front-door highlight unless later results separate it clearly from `student_t_marginal`

Next computation priorities:

- run the same posterior comparison across all `21` opening rolls
- stratify by small-gap vs clear-gap openings
- compare repeated-seed stability explicitly
- test whether Dirichlet helps more on ranking than on top-1
- run the scored categorical model on larger opening and state batteries now
  that the rollout engine exposes full single/gammon/backgammon categories
