# Model Audit Table

This package should compare a small number of statistically coherent
`reward_model + posterior_model` stacks, not a distribution zoo.

The audit standard is:

1. the reward variable must be explicit,
2. the posterior family must match that reward,
3. the sufficient statistics and update path must be valid,
4. the Thompson sample must have a clear interpretation,
5. limitations must be labeled honestly,
6. the model must be useful for this project.

## Current audit

| reward_model | posterior_model | parameterization | sufficient statistics | exact vs approximate | assumptions | why appropriate here | expected advantages | expected weaknesses | decision |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `win_loss` | `beta_bernoulli` | Beta prior on Bernoulli success probability | per move: `allocation_count`, binary successes/failures | Exact conjugate | rollout reward is genuinely binary | Correct baseline whenever reward is intentionally reduced to win/loss | Fast, interpretable, exact, easy to calibrate | throws away gammon/backgammon structure and ranking nuance | `keep now` |
| `win_loss` | `bootstrap` | empirical resampling of binary outcomes | per move: binary outcome counts | Approximate, nonparametric | empirical outcome distribution is a useful posterior-like proxy | Honest robustness check against conjugate Beta assumptions | Flexible, simple comparator | noisier, less stable, not exact Bayes | `keep now, secondary` |
| `win_loss` | `gaussian_approx` | normal approximation to mean binary reward | per move: `reward_sum`, `reward_sum_sq`, `allocation_count` | Approximate, misspecified | binary reward can be treated as approximately continuous near moderate sample size | Only a rough sanity baseline | cheap and familiar | poor tail behavior, ignores bounded discrete structure | `implemented, de-emphasize` |
| `categorical_outcome` | `dirichlet_multinomial` | Dirichlet prior on scored outcome-category probabilities | per move: category counts over `single/gammon/backgammon loss`, `unresolved`, `single/gammon/backgammon win` | Exact conjugate | rollout terminal outcomes belong to a finite category set with a valid payoff map | Most natural backgammon-shaped model in the package | preserves bounded categorical structure, interpretable, ranking-friendly | payoff mapping must be chosen explicitly; unresolved category remains a truncation artifact | `keep now, central` |
| `categorical_outcome` | `bootstrap` | empirical resampling of scored categorical outcomes | per move: scored outcome-category counts | Approximate, nonparametric | empirical category distribution is informative enough for posterior-like sampling | Good robustness check against Dirichlet shrinkage assumptions | simple comparator, few modeling assumptions | noisier, less stable, not exact Bayes | `keep now, secondary` |
| `scalar_payoff` | `beta_pseudo` | Beta-style pseudo-posterior on bounded scalar payoff in `[0,1]` | per move: `reward_sum`, `allocation_count` | Approximate, pseudo-conjugate | bounded scalar reward can be summarized like fractional Bernoulli evidence | Preserves current package behavior and fast legacy semantics | fastest scalar default, easy to compare historically | not exact conjugacy once payoff is fractional or multi-atomic | `keep now for continuity, label clearly` |
| `scalar_payoff` | `student_t_marginal` | NIG prior with Student-t marginal sampling for mean | per move: `reward_sum`, `reward_sum_sq`, `allocation_count` | Approximate Bayesian scalar model | scalar reward can be treated as approximately continuous with unknown variance | Strongest current scalar alternative to `beta_pseudo` | coherent mean/variance uncertainty, more honest than pseudo-Beta | still ignores bounded/discrete support exactly | `keep now, central scalar comparator` |
| `scalar_payoff` | `normal_inverse_gamma` | NIG posterior over mean/variance | per move: `reward_sum`, `reward_sum_sq`, `allocation_count` | Approximate Bayesian scalar model | same as above | Mathematically coherent, but mostly a parameterization detail relative to Student-t marginal | useful internal reference / sanity check | redundant with `student_t_marginal` for presentation | `implemented, de-emphasize` |
| `scalar_payoff` | `bootstrap` | empirical resampling of scalar rewards | per move: empirical scalar outcomes via stored summaries | Approximate, nonparametric | empirical scalar outcome law is informative enough for posterior-like sampling | Good robustness check against parametric scalar assumptions | flexible comparator | noisy at modest budgets, not exact Bayes | `keep now, secondary` |
| `scalar_payoff` | `gaussian_approx` | plug-in normal approximation to reward mean | per move: `reward_sum`, `reward_sum_sq`, `allocation_count` | Approximate, misspecified | scalar payoff is treated as approximately continuous and homoscedastic enough | Only as a rough baseline | cheap, interpretable | poor fit to bounded atomic reward support | `implemented, de-emphasize strongly` |

## What should stay central

These are the models worth actively teaching and comparing:

- `win_loss + beta_bernoulli`
- `categorical_outcome + dirichlet_multinomial`
- `scalar_payoff + beta_pseudo`
- `scalar_payoff + student_t_marginal`
- `bootstrap` variants only as explicit robustness checks

## What should not drive the package story

- `gaussian_approx`
  Keep only as a rough sanity baseline, not a recommended model.
- `normal_inverse_gamma`
  Keep as an internal or secondary scalar comparator, but do not present it as a main front-door choice unless it separates empirically from `student_t_marginal`.

## Not for now

These should not be added until the data structure and statistical role are much clearer:

- non-conjugate posterior TS
- hierarchical / feature-informed priors
- logistic-normal simplex models
- arbitrary bounded continuous families

Mathematical possibility is not enough. The package should add only models that are:

- coherent for the reward,
- interpretable,
- useful for ranking and finite-budget analysis,
- and worth the complexity.
