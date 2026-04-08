# backgammonr

`backgammonr` is a research package for finite-budget Monte Carlo allocation in
backgammon. The package treats one local backgammon decision as a stochastic
best-action-identification problem and uses Thompson sampling as the central
allocation rule.

This package is not trying to tell you the universal truth about backgammon.
Its scientific object is narrower and more explicit:

- one board state
- one realized dice roll
- the legal root moves from that state and roll
- a finite rollout budget
- a declared continuation policy for the rest of the game
- a declared reward map from rollout outcomes
- a finite-budget algorithm that learns move values under that rollout model

The package truth object is:

- high-budget Monte Carlo proxy truth under the package's rollout environment

The package truth object is not:

- expert backgammon truth
- strong-bot truth
- exact game-theoretic truth

## Why Backgammon Is A Good Laboratory

Backgammon is a good setting for finite-budget simulation allocation because a
single local decision already has all of the hard ingredients:

- a discrete but nontrivial legal action set
- noisy action values because continuation play is simulated
- heterogeneous state difficulty
- ranking questions, not just winner questions
- serious runtime cost because rollouts are expensive

The canonical opening laboratory is the set of 21 unordered opening rolls:

- 15 non-doubles
- 6 doubles

That battery matters because the initial board is fixed while the realized roll
changes, so action count, value gaps, and difficulty all vary in a controlled
way.

## Core Scientific Problem

For one state `s`, one realized roll `r`, and legal actions
`A(s, r) = {a_1, ..., a_K}`, the package studies move values

```text
mu_i = E[Y_i | s, r, a_i, rollout environment]
```

where `Y_i` is the rollout reward for move `a_i`.

The best action under the rollout model is

```text
i* = argmax_i mu_i
```

The finite-budget problem is:

```text
Given a total rollout budget T, how should we allocate that budget across
legal moves so that we identify or rank the best moves as accurately and as
quickly as possible?
```

The rollout environment is part of the estimand. It includes:

- the continuation policy, such as `simulation_policy = "random"`
- the truncation rule, such as `max_rollout_turns = 220L`
- the unresolved payoff, such as `unresolved_value = 0.5`
- the reward model

So the package's estimand is:

```text
expected move value under the declared rollout environment
```

That is the package identity.

## Proxy Truth

The package uses high-budget Monte Carlo proxy references.

For move `a_i`, if the proxy-reference rollout budget for that action is `N_i`,
then the proxy-reference mean is

```text
mu_ref_i = (1 / N_i) * sum_{j=1}^{N_i} Y_{i,j}
```

The proxy-reference best move is the move with the largest `mu_ref_i`.

The usual proxy-reference Monte Carlo interval is the normal approximation

```text
mu_ref_i +/- 1.96 * SE(mu_ref_i)
```

Important interpretation:

- these are Monte Carlo intervals under the rollout model
- they are not "truth intervals"
- they describe uncertainty in the proxy-reference estimate itself

## Minimal Workflow

```r
library(backgammonr)

problem <- bg_problem(
  state = bg_initial_board(),
  roll = bg_roll(1L, 6L),
  simulation_policy = "random",
  reward_model = "scalar_payoff",
  posterior_model = "beta_pseudo",
  problem_id = "opening_1-6"
)

truth <- bg_truth_state(
  problem = problem,
  budget = 4096L,
  n_cores = 1L,
  parallel = FALSE,
  save_path = file.path(tempdir(), "opening_1-6_truth.rds"),
  seed = 1L
)

fit <- bg_ts_run(
  problem = problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth$reference,
  seed = 1L
)

panel <- bg_eval_reference_aware(fit, truth = truth$reference)
panel[, c(
  "checkpoint",
  "recommended_move_label",
  "truth_best_move_label",
  "top1_match",
  "simple_regret",
  "spearman"
)]
```

## Reward Models

The package separates:

- what one rollout returns
- how uncertainty about that rollout return is modeled

The reward model says what the random variable `Y` is.

### `reward_model = "win_loss"`

Each rollout returns:

- `1` for win
- `0` for loss

Then

```text
mu_i = P(win | s, r, a_i, rollout environment)
```

This is the clean textbook bandit case.

### `reward_model = "scalar_payoff"`

Each rollout returns a bounded scalar in `[0, 1]`.

This is the package's main scalar-valued path. It treats move value as expected
payoff under the rollout environment:

```text
mu_i = E[Y_i]
```

This is broader than pure win probability.

### `reward_model = "categorical_outcome"`

Each rollout returns a category and then the category probabilities are mapped
to an expected scalar payoff.

The full scored-outcome model usually uses 7 categories:

- `single_loss`
- `gammon_loss`
- `backgammon_loss`
- `unresolved`
- `single_win`
- `gammon_win`
- `backgammon_win`

There is also a 3-category collapse:

- `loss`
- `unresolved`
- `win`

## Posterior Models And Distributions

The posterior model says how uncertainty about move value is represented.

Implemented posterior families:

- `beta_bernoulli`
- `beta_pseudo`
- `dirichlet_multinomial`
- `gaussian_approx`
- `normal_inverse_gamma`
- `student_t_marginal`
- `bootstrap`

The main logic lives in:

- `R/bg_model_spec.R`
- `R/bg_posterior_kernels.R`
- `src/bg_posterior_models.cpp`

### 1. `win_loss + beta_bernoulli`

This is the statistically clean binary model.

If move `i` has:

- `s_i` wins
- `f_i` losses

and prior

```text
p_i ~ Beta(alpha_0, beta_0)
```

then the posterior is

```text
p_i | data ~ Beta(alpha_0 + s_i, beta_0 + f_i)
```

Canonical Thompson then samples

```text
tilde_p_i ~ Beta(alpha_0 + s_i, beta_0 + f_i)
```

and allocates to the move with the largest sampled win probability.

Status:

- exact conjugate Bayes for binary data
- statistically clean

### 2. `scalar_payoff + beta_pseudo`

This is the package's main scalar default and the main fast-path stack.

If move `i` has:

- `n_i` rollouts
- reward sum `R_i = sum_j Y_{i,j}`

then the package uses pseudo-counts

```text
alpha_i = alpha_0 + R_i
beta_i  = beta_0 + n_i - R_i
```

and samples

```text
tilde_mu_i ~ Beta(alpha_i, beta_i)
```

This looks like Beta-Bernoulli Thompson, but it is not exact Bayes unless the
data are truly Bernoulli. It is a pseudo-Bayesian approximation for bounded
scalar payoffs.

Status:

- central in the package
- approximate, not exact

### 3. `categorical_outcome + dirichlet_multinomial`

If move `i` has category counts `c_{i1}, ..., c_{iK}` and prior vector
`alpha_01, ..., alpha_0K`, then

```text
theta_i | data ~ Dirichlet(alpha_01 + c_{i1}, ..., alpha_0K + c_{iK})
```

If the category payoff map is `w_1, ..., w_K`, then Thompson draws:

```text
1. draw theta_tilde_i from the Dirichlet posterior
2. set mu_tilde_i = sum_k theta_tilde_{ik} * w_k
```

This is the most domain-faithful family in the package because it keeps scored
outcome structure rather than collapsing everything immediately to a single
scalar.

Status:

- exact conjugate Bayes for categorical data
- statistically clean

### 4. `scalar_payoff + student_t_marginal`

This uses a normal-inverse-gamma style update for scalar rewards and samples
the mean from the posterior marginal Student-t distribution.

At the sufficient-statistic level it uses:

- count
- reward sum
- reward sum of squares

The package then integrates out variance and samples the mean through the
Student-t marginal.

Status:

- approximate for bounded scalar rewards
- more defensible than a plain Gaussian approximation

### 5. `scalar_payoff + gaussian_approx`

This uses a Gaussian approximation for the mean of a bounded scalar reward.

Status:

- approximate
- secondary

### 6. `scalar_payoff + normal_inverse_gamma`

This models mean and variance jointly and then samples the mean conditionally.

Status:

- approximate for bounded payoff data
- secondary

### 7. `bootstrap`

This uses resampling-style uncertainty rather than a conjugate Bayesian family.

Status:

- robustness comparator
- heuristic rather than canonical Bayesian modeling

## Which Model Stacks Matter Most

The package supports more model families than it should headline. The main
coherent stacks are:

- `win_loss + beta_bernoulli`
- `scalar_payoff + beta_pseudo`
- `scalar_payoff + student_t_marginal`
- `categorical_outcome + dirichlet_multinomial`

The first and fourth are the cleanest statistically.

The second is the package's main scalar baseline.

The third is the main scalar alternative when you want a more explicit
continuous posterior for the mean.

## Canonical Thompson Sampling

Every Thompson-style method in the package follows the same template:

```text
for t = 1, ..., T:
  1. build a posterior for each legal move
  2. sample one plausible move value from each posterior
  3. choose the move with the best sampled value
  4. simulate one rollout for that move
  5. update only that move
```

If the posterior for move `i` at time `t` is `pi_i(. | data_t)`, then canonical
Thompson chooses

```text
a_t = argmax_i mu_tilde_{i,t}
where mu_tilde_{i,t} ~ pi_i(. | data_t)
```

This balances:

- exploitation of moves with high posterior means
- exploration of moves with high posterior uncertainty

## Thompson Extensions In This Package

The package supports several Thompson-family extensions. The central ones are:

- `thompson`
- `top_two_thompson`

The rest are compare-ready but more experimental:

- `multi_sample_thompson`
- `tempered_thompson`
- `budget_aware_thompson`
- `elimination_thompson`
- `ranking_aware_thompson`

The selection logic is implemented in:

- `R/ts_frontdoor.R`
- `R/bg_posterior_kernels.R`

### `thompson`

Canonical Thompson:

```text
draw one posterior sample for each move
pick the move with the largest draw
```

This is the baseline.

### `top_two_thompson`

This is the TTTS comparator.

Operationally the package:

- uses a larger draw matrix
- finds a leading sampled winner
- finds a sampled challenger
- allocates between them using `ttts_beta`

Conceptually:

```text
1. sample a current best candidate i_1
2. sample an alternative challenger i_2
3. allocate to i_1 with probability beta
4. allocate to i_2 with probability 1 - beta
```

Key parameter:

- `ttts_beta`
  Probability of staying with the current Thompson winner. Smaller values force
  more challenger sampling.

Why use it:

- canonical Thompson can over-commit too early when two moves are close
- TTTS keeps explicit pressure on the near-best alternatives

### `multi_sample_thompson`

The package draws several Thompson winner samples, counts how often each action
wins, and then allocates to the action with the largest winner frequency.

Operationally:

```text
1. draw M posterior winner samples
2. record which move won each draw
3. tabulate winner frequencies
4. allocate to the move with the highest frequency
```

Key parameter:

- `multi_sample_draws`
  Number of winner draws used in the voting step.

Why use it:

- reduces sensitivity to one lucky posterior draw
- makes the policy more consensus-oriented

### `tempered_thompson`

The package rescales posterior dispersion before sampling.

Conceptually:

```text
posterior draws are tempered around the posterior mean
```

Interpretation of `temperature`:

- `temperature > 1`: noisier draws, more exploration
- `temperature < 1`: tighter draws, more exploitation
- `temperature = 1`: canonical Thompson

Why use it:

- to tune exploration pressure without adding a separate bonus term

### `ranking_aware_thompson`

This variant focuses on uncertainty within the near-top set.

The package:

- takes a draw matrix
- keeps a focus set of size `ranking_top_k`
- computes pairwise uncertainty inside that focus set
- allocates to the move whose near-top ordering is most uncertain

The core uncertainty score is built from terms like

```text
p_ij * (1 - p_ij)
```

where `p_ij` is the posterior probability that move `i` beats move `j`.

This is largest when `p_ij` is near `0.5`, so the method spends budget on
pairwise ambiguities near the top of the ranking.

Key parameters:

- `ranking_top_k`
  Size of the near-top focus set.
- `ranking_draws`
  Number of posterior draws used to estimate ranking uncertainty.

Why use it:

- if the real goal is top-of-ranking recovery, not just one sampled winner,
  then near-top pairwise uncertainty is often the right place to spend budget

### `elimination_thompson`

This variant is Thompson sampling plus screening.

The package:

- keeps a set of active actions
- waits until every active action has at least `elimination_min_allocations`
- computes approximate 95 percent posterior intervals
- removes actions whose upper bound is clearly below the leader's lower bound,
  up to a protection set of size `elimination_keep_top`

The screening rule is approximately:

```text
eliminate action i if
upper_95(i) + elimination_margin < max_j lower_95(j)
```

except that the top `elimination_keep_top` actions by posterior mean are always
protected.

Key parameters:

- `elimination_min_allocations`
- `elimination_keep_top`
- `elimination_margin`

Why use it:

- to stop wasting budget on clearly inferior actions

Risk:

- if elimination happens too early, the method can remove a move that should
  have survived

### `budget_aware_thompson`

This is a staged hybrid policy.

The package uses budget progress

```text
progress = spent / total_budget
```

and then switches behavior by phase.

Current logic:

- early phase, `progress < 0.3`
  - use a more exploratory multi-sample style rule with at least a few winner
    draws and elevated temperature
- middle phase
  - behave like canonical Thompson
- late phase, `progress >= 0.75`
  - inspect the top gap and near-top uncertainty
  - if the top gap is small relative to posterior uncertainty, use top-two
    Thompson
  - otherwise use ranking-aware allocation

The near-tie trigger is approximately:

```text
top_gap <= 1.25 * gap_sd
```

where `gap_sd` is built from the posterior standard deviations of the leading
actions.

Why use it:

- different phases of the budget path need different behavior
- early budgets reward broad exploration
- late budgets reward careful discrimination among near-best moves

This is one of the most heuristic variants in the package.

## Important Parameters For Important Functions

This section focuses on the functions you will actually use most often.

### `bg_problem()`

Purpose:

- build the canonical one-state, one-roll decision object

Most important parameters:

- `state`
  A `bg_board` object.
- `roll`
  A `bg_roll` object.
- `simulation_policy`
  Continuation policy after the root action. This changes the estimand.
- `heuristic_policy`
  Only used when `simulation_policy = "heuristic"`.
- `max_rollout_turns`
  Rollout truncation horizon.
- `unresolved_value`
  Payoff assigned to unresolved rollouts.
- `prior_alpha`, `prior_beta`
  Pseudo-counts used by beta-style models.
- `reward_model`
  The rollout reward variable.
- `posterior_model`
  The posterior family used for Thompson-style summaries.
- `posterior_prior`
  Optional named prior override.
- `legal_moves`
  Optional precomputed legal move set.
- `cache`
  Reuse a cached problem object when possible.
- `problem_id`
  Stable label for studies and saved artifacts.

What it returns:

- a `bg_problem` object containing the board, roll, legal actions, model stack,
  and rollout settings

### `bg_reference()`

Purpose:

- build one high-budget proxy reference for one `bg_problem`

Most important parameters:

- `problem`
  A `bg_problem`.
- `budget`
  Total proxy-reference rollout budget.
- `workers_truth`
  Worker count for truth computation.
- `truth_block_size`
  Block size for rollout chunks.
- `reference_mode`
  `"equal"` or `"focused"`.
  `"equal"` is the main conservative mode.
  `"focused"` is faster but approximate.
- `extend_existing_reference`
  Extend a previously built reference instead of starting from zero.
- `dice_mode`
  Dice-generation mode.
- `crn`
  Whether to use common random numbers.
- `focus_top`
  Number of actions protected into the focused second stage.
- `focus_share`
  Fraction of the total budget allocated to the focused stage.
- `seed`
  Reproducible random seed.

What it returns:

- a `bg_reference` object with action-level proxy means, MC intervals, and
  rankings

### `bg_truth_state()`

Purpose:

- build, save, load, or extend one cached proxy-truth object for one state-roll
  problem

Most important parameters:

- `state`, `roll`
  Used when you are not passing a prebuilt `problem`.
- `problem`
  Optional prebuilt `bg_problem`.
- `budget`
  Proxy-truth budget.
- `simulation_policy`, `reward_model`, `posterior_model`
  Only used when `problem` is not supplied.
- `n_cores`, `parallel`
  Worker control.
- `truth_block_size`
  Block size for rollout chunks.
- `reference_mode`
  `"equal"` or `"focused"`.
- `cache`
  Whether to reuse existing truth files automatically.
- `cache_dir`
  Directory used when `save_path` is omitted.
- `save_path`
  Explicit artifact path.
- `overwrite`
  Rebuild even if the file already exists.
- `dice_mode`, `crn`, `seed`
  Variance and RNG controls.
- `problem_id`
  Label used when building a new `bg_problem` internally.

What it returns:

- a `bg_truth_state` object containing the wrapped `bg_reference`, metadata, and
  summary tables

### `bg_truth_opening()`

Purpose:

- build a battery of cached opening-roll proxy truths

Most important parameters:

- `rolls`
  Optional list of `bg_roll` objects. If omitted, the opening battery is used.
- `include_doubles`
  Whether the six doubles are included.
- `budget`
  Proxy-truth budget per roll.
- `simulation_policy`, `reward_model`, `posterior_model`
  Model stack for all opening problems.
- `n_cores`, `parallel`
  Worker control.
- `reference_mode`
  `"equal"` or `"focused"`.
- `cache`, `cache_dir`, `save_path`, `overwrite`
  File management.
- `dice_mode`, `crn`, `seed`
  Variance and RNG control.
- `verbose`
  Progress display.

What it returns:

- a `bg_truth_battery` object

### `bg_truth_battery()`

Purpose:

- build proxy truths for a list of prebuilt problems

Most important parameters:

- `problems`
  One `bg_problem` or a list of them.
- `budget`
  Per-problem truth budget.
- `n_cores`, `parallel`, `truth_block_size`
  Compute controls.
- `reference_mode`
  `"equal"` or `"focused"`.
- `cache`, `cache_dir`, `save_path`, `overwrite`
  Persistence controls.
- `dice_mode`, `crn`, `seed`
  Variance controls.
- `verbose`
  Progress display.

### `bg_ts_run()`

Purpose:

- run canonical Thompson sampling on one problem

Direct parameters:

- `problem`
- `budget`
- `proxy_reference`
- `checkpoints`
- `seed`

Additional important controls passed through `...`:

- `ts_mode`
  `"sequential"` or `"batched"`. Batched mode is limited.
- `dice_mode`
  `"iid"`, `"stratified_first_roll"`, or `"stratified_first_two_rolls"`.
- `crn`
  Common random numbers.
- `task_block_size`
  Block size for rollout blocks.

What it returns:

- a `bg_ts_run` object with a final action table and a checkpoint table

### `bg_ttts_run()`

Purpose:

- run top-two Thompson sampling on one problem

Direct parameters:

- `problem`
- `budget`
- `proxy_reference`
- `checkpoints`
- `seed`
- `ttts_beta`

`ttts_beta` interpretation:

- probability of staying with the current Thompson winner rather than the
  challenger

### `bg_uniform_run()` and `bg_ucb_run()`

Purpose:

- run the main non-Thompson baselines

Use these when:

- you want a simple equal-allocation benchmark
- you want a confidence-bound benchmark

Current caveat:

- non-Thompson comparators remain tied to the legacy scalar fast path, so they
  are mainly intended for `scalar_payoff + beta_pseudo` problems

### `bg_compare_algorithms()`

Purpose:

- compare methods across problems, budgets, and seeds

Most important parameters:

- `problems`
  One `bg_problem` or a list of them.
- `methods`
  Character vector such as
  `c("thompson", "top_two_thompson", "ucb", "equal")`.
- `budgets`
  Budget grid.
- `seeds`
  Repeated-seed set.
- `proxy_references`
  Optional supplied truth objects.
- `reference_budget`
  Budget used to build references if `proxy_references` is omitted.
- `save_path`
  Optional study artifact path.
- `overwrite`
  Whether existing artifacts may be replaced.
- `n_cores`, `parallel`
  Study-level parallel controls.
- `progress`
  Progress display.
- `...`
  Passed into the underlying comparison engine.

What it returns:

- a `bg_method_compare` object

### `bg_compare_posteriors()`

Purpose:

- keep the reward model fixed and compare posterior families

Most important parameters:

- `problems`
- `posterior_models`
  If omitted, the package uses the recommended posterior families for the chosen
  reward model.
- `reward_model`
  Fixed reward model for the comparison.
- `budgets`
- `seeds`
- `allocation_policy`
  Usually `"thompson"` or `"top_two_thompson"`.
- `proxy_references`
- `reference_budget`
- `n_cores`, `parallel`, `progress`
- `save_path`, `overwrite`

What it returns:

- a `bg_posterior_compare` object

### `bg_compare_reward_models()`

Purpose:

- compare coherent reward-model/posterior-model stacks

Most important parameters:

- `problems`
- `reward_model_map`
  Named vector mapping each reward model to its posterior model.
- `budgets`
- `seeds`
- `allocation_policy`
- `win_loss_unresolved_value`
  Special unresolved value when comparing the `win_loss` stack.
- `reference_budget`
- `n_cores`, `parallel`, `progress`
- `save_path`, `overwrite`

What it returns:

- a `bg_reward_model_compare` object

## Evaluation Metrics

The package distinguishes:

- rollout-model values
- proxy-reference values
- posterior summaries from finite-budget runs
- runtime summaries

### Decision-quality metrics

- `top1_match`

```text
1{selected move = proxy-truth best move}
```

- `simple_regret`

```text
mu_ref(best) - mu_ref(selected)
```

- `epsilon_optimal`

```text
1{simple_regret <= epsilon}
```

- `selected_reference_rank`

```text
proxy-truth rank of the selected move
```

- `recommended_prob_best`

```text
posterior probability that the recommended move is best
```

- `posterior_top_k_mass`

```text
posterior mass carried by the top-k estimated moves
```

### Ranking metrics

- `spearman`
  Spearman rank correlation between estimated and proxy-truth rankings.
- `kendall`
  Kendall rank correlation between estimated and proxy-truth rankings.
- `top_k_overlap`

```text
|Top_k_estimated intersect Top_k_truth| / k
```

- `top_k_overlap_n`

```text
|Top_k_estimated intersect Top_k_truth|
```

- `pairwise_ordering_accuracy`

```text
proportion of move pairs ordered the same way by estimate and proxy truth
```

- `pairwise_disagreement_count`

```text
number of move pairs ordered differently
```

- `weighted_rank_loss`

```text
sum_i w_i * |rank_hat_i - rank_truth_i| / sum_i w_i
with w_i = 1 / rank_truth_i
```

### Allocation metrics

If `p_i` is the budget share allocated to move `i`, then:

- `allocation_entropy`

```text
- sum_i p_i log(p_i) / log(K)
```

- `allocation_hhi`

```text
sum_i p_i^2
```

- `allocation_max_share`

```text
max_i p_i
```

- `share_top_k_truth`

```text
sum_{i in Top_k_truth} p_i
```

- `share_best_truth`

```text
p_(truth-best)
```

- `share_mc_screened_suboptimal`

```text
budget share spent on moves screened as clearly suboptimal by proxy-reference
MC intervals or the fallback gap rule
```

- `mc_screened_suboptimal_count`
  Number of screened suboptimal moves.

- `total_allocation`
  Total rollouts allocated through the checkpoint.

- `n_allocated_actions`
  Number of actions that have received at least one rollout.

### MC-screening and gap-aware metrics

- `top_two_gap_estimate`

```text
mu_ref(best) - mu_ref(second_best)
```

- `near_tie`
  Whether the top-two proxy-reference gap is smaller than the chosen tolerance.

- `mc_not_separated_from_best_set_size`

```text
number of moves whose proxy-reference upper interval still overlaps the best
move's lower interval
```

- `chosen_mc_not_separated_from_best`
  Whether the selected move lies in that non-separated set.

- `chosen_gap_to_best`

```text
mu_ref(best) - mu_ref(selected)
```

### Efficiency metrics

These summarize how quickly a method becomes good.

- `first_budget_top1_match`
  First checkpoint where the method recommends the proxy-truth best move.
- `first_budget_epsilon_optimal`
  First checkpoint where regret falls below the epsilon target.
- `first_runtime_top1_match`
  First runtime where top-1 correctness is achieved.
- `first_runtime_epsilon_optimal`
  First runtime where epsilon-optimality is achieved.
- `auc_top1_match`
  Area under the budget-path curve for top-1 correctness.
- `auc_simple_regret`
  Area under the budget-path curve for simple regret.

### Calibration metrics

These ask whether posterior confidence is trustworthy.

- `brier_top1`

```text
(recommended_prob_best - top1_outcome)^2
```

- calibration-bin summaries:
  - `mean_predicted_prob_best`
  - `observed_top1_rate`
  - `calibration_gap`
  - `ece_component`

Interpretation:

- if predicted probability-best is well calibrated, `mean_predicted_prob_best`
  should track `observed_top1_rate`

### Runtime summaries

- `runtime_seconds`
- `rollout_throughput`

## Public API By Group

### Board, roll, move, and simulation functions

- `bg_board()`
- `bg_initial_board()`
- `bg_roll()`
- `bg_legal_moves()`
- `bg_apply_move_sequence()`
- `bg_play_turn()`
- `bg_play_game()`
- `bg_print_board()`
- `bg_validate_board()`

### Problem and truth functions

- `bg_problem()`
- `bg_reference()`
- `bg_truth_state()`
- `bg_truth_opening()`
- `bg_truth_battery()`
- `bg_truth_save()`
- `bg_truth_load()`
- `bg_truth_diagnostics()`
- `bg_study_save()`
- `bg_study_load()`

### Methods

- `bg_ts_run()`
- `bg_ttts_run()`
- `bg_uniform_run()`
- `bg_ucb_run()`
- `bg_compare_algorithms()`
- `bg_compare_posteriors()`
- `bg_compare_reward_models()`

### Evaluation

- `bg_eval_top1()`
- `bg_eval_rank()`
- `bg_eval_allocation()`
- `bg_eval_efficiency()`
- `bg_eval_calibration()`
- `bg_eval_reference_aware()`

### State and feature functions

- `bg_board_features()`
- `bg_move_features()`
- `bg_state_classify()`
- `bg_state_difficulty()`

### Plot functions

- `plot_bg_truth()`
- `plot_bg_ts_trace()`
- `plot_bg_allocation()`
- `plot_bg_budget_curve()`
- `plot_bg_rank_compare()`
- `plot_bg_posterior_compare()`

## Current Notebooks

The current notebook sequence is:

1. `01_save_all_21_opening_truths`
2. `02_roll_1_6_thompson_over_budget`
3. `03_roll_1_6_thompson_vs_non_thompson`
4. `04_roll_1_6_thompson_variants`
5. `05_roll_1_6_thompson_model_comparison`
6. `06_roll_1_6_mc_screening_efficiency_and_calibration`

These notebooks are meant to be run directly and edited directly.

## Installation And Development

Install from a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
library(backgammonr)
```

For development:

```r
pkgload::load_all(".")
```

## Final Scientific Frame

`backgammonr` is a package about finite-budget simulation allocation with
backgammon as a realistic stochastic laboratory.

Its core scientific distinctions are:

- rollout-model value versus unavailable game-theoretic truth
- high-budget proxy reference versus finite-budget algorithm output
- posterior uncertainty versus proxy-reference Monte Carlo uncertainty
- decision quality versus runtime cost

That is the package story.
