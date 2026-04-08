# backgammonr

`backgammonr` is a research package for **finite-budget Monte Carlo allocation
in backgammon**. The package treats one local backgammon decision as a
stochastic best-action-identification problem and uses **Thompson sampling** as
the central allocation rule.

The point of the package is not to claim expert backgammon truth, rollout-bot
truth, or game-theoretic truth. The point is to study how a finite simulation
budget should be spent when:

- the board state is fixed;
- the dice roll is already realized;
- the legal root moves are known;
- continuation play is simulated under a declared rollout environment; and
- move values must be learned under sampling noise.

That makes backgammon a useful scientific laboratory:

- legal action sets are discrete but nontrivial;
- action values are noisy because they are estimated by rollouts;
- state difficulty varies sharply from one roll or position to another;
- ranking recovery matters, not just winner identification;
- runtime matters because rollouts are expensive.

In short, `backgammonr` studies **how to learn which move is best when only a
limited rollout budget is available**.

## Why Backgammon Matters Here

Backgammon is relevant because it creates exactly the kind of stochastic local
decision problem that finite-budget allocation methods are built for.

For one board state and one realized roll:

- there are multiple legal root moves;
- each root move induces a different distribution of future rollout outcomes;
- those distributions are not directly known;
- high-budget simulation can approximate them, but low-budget decision-making is
  the real problem.

The package’s canonical opening laboratory is the set of **21 unordered opening
rolls**:

- 15 non-doubles;
- 6 doubles.

Each opening-roll problem uses the same initial board but a different realized
roll, so the action count, gap structure, and difficulty all vary in a
controlled way.

## Package Goal

Formally, the package studies:

1. one board state \(s\);
2. one realized roll \(r\);
3. the legal root action set \(A(s, r) = \{a_1, \dots, a_K\}\);
4. a finite budget \(T\) of rollout simulations;
5. a rollout reward variable \(Y_{i,j}\) for move \(a_i\);
6. the move values

\[
\mu_i = \mathbb{E}[Y_{i,j} \mid s, r, a_i, \text{rollout environment}],
\]

7. and the induced optimal move

\[
i^\star = \arg\max_{i \in \{1,\dots,K\}} \mu_i.
\]

The rollout environment is part of the estimand. It includes:

- the continuation policy, such as `simulation_policy = "random"`;
- the terminal or truncated reward mapping;
- the unresolved handling rule, such as `unresolved_value = 0.5`;
- the maximum rollout horizon.

So the scientific estimand is:

> expected move value under the declared rollout environment.

It is not “true backgammon value” in a universal sense.

## Proxy Truth

The package uses **high-budget Monte Carlo proxy references** as its truth
objects.

For move \(a_i\), a proxy reference uses a large rollout count \(N_i\) and
forms:

\[
\hat{\mu}^{\text{ref}}_i = \frac{1}{N_i} \sum_{j=1}^{N_i} Y_{i,j}.
\]

The package then ranks moves by \(\hat{\mu}^{\text{ref}}_i\) and stores the
result as a `bg_reference` or `bg_truth_state` object.

The standard Monte Carlo interval summary is the normal approximation

\[
\hat{\mu}^{\text{ref}}_i \pm 1.96 \cdot \widehat{\mathrm{SE}}(\hat{\mu}^{\text{ref}}_i).
\]

Important:

- proxy truth is still noisy;
- proxy-reference intervals are **Monte Carlo intervals**, not “truth
  intervals”;
- the best move under a proxy reference is the best move under the package’s
  rollout model, not necessarily the best move according to human experts or
  strong bots.

## Thompson Sampling In This Package

At a high level, Thompson sampling does the same thing regardless of the chosen
posterior family.

At step \(t\):

1. maintain posterior uncertainty for each move \(a_i\);
2. sample one plausible move value \(\tilde{\mu}_{i,t}\) from each posterior;
3. choose

\[
a_t = \arg\max_i \tilde{\mu}_{i,t};
\]

4. simulate one new rollout for \(a_t\);
5. update only that move’s sufficient statistics;
6. repeat until the budget is exhausted.

This balances exploration and exploitation automatically:

- moves with high posterior means tend to be sampled often;
- moves with high posterior uncertainty can still win the draw and receive more
  budget.

## Reward Models

The package separates **what a rollout returns** from **how uncertainty about
that return is modeled**.

The reward model choices are:

- `win_loss`
- `scalar_payoff`
- `categorical_outcome`

### `win_loss`

Each rollout returns:

- `1` for a win;
- `0` for a loss.

Then

\[
\mu_i = \mathbb{P}(\text{win} \mid s, r, a_i, \text{rollout environment}).
\]

### `scalar_payoff`

Each rollout returns a scalar in \([0,1]\). In the standard package setup,
losses, unresolved outcomes, and wins are mapped into a bounded payoff scale.

Then

\[
\mu_i = \mathbb{E}[Y_i], \qquad Y_i \in [0,1].
\]

### `categorical_outcome`

Each rollout returns a categorical scored outcome. The full scored-outcome model
uses 7 categories:

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

The categorical probabilities are then mapped back to a scalar expected payoff.

## Posterior Models And Distributions

The posterior-model choices are:

- `beta_bernoulli`
- `beta_pseudo`
- `dirichlet_multinomial`
- `gaussian_approx`
- `normal_inverse_gamma`
- `student_t_marginal`
- `bootstrap`

These are configured in `R/bg_model_spec.R` and implemented numerically in
`src/bg_posterior_models.cpp`.

### 1. `win_loss + beta_bernoulli`

This is the clean textbook binary model.

If move \(a_i\) has:

- \(s_i\) wins;
- \(f_i\) losses;

and prior

\[
p_i \sim \text{Beta}(\alpha_0, \beta_0),
\]

then the posterior is

\[
p_i \mid \text{data} \sim \text{Beta}(\alpha_0 + s_i,\ \beta_0 + f_i).
\]

Thompson sampling draws:

\[
\tilde{p}_i \sim \text{Beta}(\alpha_0 + s_i,\ \beta_0 + f_i).
\]

Status:

- exact conjugate Bayes for binary outcomes;
- statistically clean.

### 2. `scalar_payoff + beta_pseudo`

This is the package’s main scalar default.

If move \(a_i\) has:

- \(n_i\) rollouts;
- reward sum \(R_i = \sum_j Y_{i,j}\);

then the package uses pseudo-counts

\[
\alpha_i = \alpha_0 + R_i, \qquad
\beta_i = \beta_0 + n_i - R_i.
\]

Thompson sampling draws:

\[
\tilde{\mu}_i \sim \text{Beta}(\alpha_i, \beta_i).
\]

Status:

- not exact Bayes for general scalar payoff data;
- a pseudo-Bayesian approximation;
- central because it is simple and aligned with the package’s legacy fast path.

### 3. `categorical_outcome + dirichlet_multinomial`

If move \(a_i\) has category counts \(c_{ik}\) and prior vector
\(\alpha_{0k}\), then

\[
\theta_i \mid \text{data} \sim \text{Dirichlet}(\alpha_{01} + c_{i1}, \dots, \alpha_{0K} + c_{iK}).
\]

If the category payoff map is \(w_k\), then a Thompson draw is:

1. sample \(\tilde{\theta}_i\) from the Dirichlet posterior;
2. map to a scalar:

\[
\tilde{\mu}_i = \sum_{k=1}^K \tilde{\theta}_{ik} w_k.
\]

Status:

- exact conjugate Bayes for categorical outcomes;
- statistically clean;
- the most domain-faithful model family in the package.

### 4. `scalar_payoff + student_t_marginal`

This uses a normal-inverse-gamma style update for scalar rewards and draws the
mean from the posterior marginal Student-\(t\) distribution.

Status:

- approximate for bounded payoff data;
- more statistically defensible than a plain Gaussian approximation for scalar
  rewards;
- one of the central scalar alternatives.

### 5. `scalar_payoff + gaussian_approx`

This treats posterior uncertainty in the mean through a Gaussian approximation.

Status:

- approximate;
- supported, but secondary.

### 6. `scalar_payoff + normal_inverse_gamma`

This models the mean and variance through a normal-inverse-gamma family.

Status:

- approximate for bounded scalar payoff data;
- supported, but secondary.

### 7. `bootstrap`

This is a resampling-based robustness comparator rather than a conjugate
Bayesian model.

Status:

- heuristic / robustness-oriented;
- useful as a comparator, but not a canonical posterior family.

## Supported And Recommended Model Stacks

The package supports:

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

The main stacks worth foregrounding are:

- `win_loss + beta_bernoulli`
- `scalar_payoff + beta_pseudo`
- `scalar_payoff + student_t_marginal`
- `categorical_outcome + dirichlet_multinomial`

## Allocation Methods

The package supports the following allocation policies:

- `thompson`
- `top_two_thompson`
- `multi_sample_thompson`
- `tempered_thompson`
- `budget_aware_thompson`
- `elimination_thompson`
- `ranking_aware_thompson`
- `ucb`
- `equal`

### Canonical methods

- `thompson`: standard Thompson sampling.
- `top_two_thompson`: top-two Thompson sampling (TTTS).

### Experimental Thompson variants

- `multi_sample_thompson`: bases allocation on multiple posterior winner draws.
- `tempered_thompson`: changes posterior draw dispersion with a temperature.
- `budget_aware_thompson`: uses different Thompson-style behavior over the
  budget horizon.
- `elimination_thompson`: starts screening clearly dominated actions after
  enough evidence.
- `ranking_aware_thompson`: spends more effort on uncertainty among near-top
  actions.

### Non-Thompson comparators

- `equal`: equal allocation.
- `ucb`: upper-confidence-bound allocation.

## Evaluation Metrics

The package distinguishes:

- rollout-model value;
- high-budget proxy reference;
- finite-budget posterior summaries;
- runtime and throughput summaries.

The exported evaluation functions are:

- `bg_eval_top1()`
- `bg_eval_rank()`
- `bg_eval_allocation()`
- `bg_eval_efficiency()`
- `bg_eval_calibration()`
- `bg_eval_reference_aware()`

### Decision-quality metrics

For a chosen move \(\hat{i}\) and proxy-truth best move \(i^\star\):

- `top1_match`

\[
\mathbf{1}\{\hat{i} = i^\star\}
\]

- `simple_regret`

\[
\mu^\text{ref}_{i^\star} - \mu^\text{ref}_{\hat{i}}
\]

- `epsilon_optimal`

\[
\mathbf{1}\{\mu^\text{ref}_{i^\star} - \mu^\text{ref}_{\hat{i}} \le \epsilon\}
\]

- `selected_reference_rank`

the proxy-truth rank of the selected move.

- `recommended_prob_best`

the model-relative posterior probability that the recommended move is best.

- `posterior_top_k_mass`

the posterior probability mass carried by the top-\(k\) estimated moves.

### Ranking-recovery metrics

Let \(\hat{r}_i\) be the estimated rank and \(r_i^\text{ref}\) the proxy-truth
rank.

- `spearman`

the Spearman rank correlation between \(\hat{r}\) and \(r^\text{ref}\).

- `kendall`

the Kendall rank correlation between \(\hat{r}\) and \(r^\text{ref}\).

- `top_k_overlap`

\[
\frac{|\widehat{\text{Top-}k} \cap \text{Top-}k^\text{ref}|}{k}
\]

- `top_k_overlap_n`

the raw count

\[
|\widehat{\text{Top-}k} \cap \text{Top-}k^\text{ref}|.
\]

- `pairwise_ordering_accuracy`

the fraction of action pairs ordered the same way by the estimate and the proxy
reference.

- `pairwise_disagreement_count`

the number of action pairs ordered differently by the estimate and the proxy
reference.

- `weighted_rank_loss`

a rank-error summary that weights errors near the top of the truth ranking more
heavily.

### Allocation metrics

If the final allocation counts are \(n_i\) and \(p_i = n_i / \sum_j n_j\):

- `allocation_entropy`

\[
-\frac{\sum_i p_i \log p_i}{\log K}
\]

so values near `1` mean diffuse allocation and values near `0` mean highly
concentrated allocation.

- `allocation_hhi`

\[
\sum_i p_i^2
\]

- `allocation_max_share`

\[
\max_i p_i
\]

- `share_top_k_truth`

the fraction of budget spent on the proxy-truth top-\(k\) moves.

- `share_best_truth`

the fraction of budget spent on the proxy-truth best move.

- `share_mc_screened_suboptimal`

the fraction of budget spent on moves that look clearly suboptimal under the
proxy-reference MC screening rule.

- `mc_screened_suboptimal_count`

the number of actions screened as clearly suboptimal.

- `total_allocation`

the total number of rollouts allocated by the checkpoint.

- `n_allocated_actions`

the number of actions that have received at least one rollout.

### MC-screening and gap-aware metrics

These use the Monte Carlo uncertainty of the proxy reference itself.

- `top_two_gap_estimate`

\[
\mu^\text{ref}_{(1)} - \mu^\text{ref}_{(2)}
\]

- `near_tie`

whether the top-two proxy-reference gap is below a chosen tolerance.

- `mc_not_separated_from_best_set_size`

the size of the set of moves whose proxy-reference upper interval still overlaps
the best move’s lower interval.

- `chosen_mc_not_separated_from_best`

whether the chosen move lies inside that non-separated set.

- `chosen_gap_to_best`

the gap between the chosen move’s proxy-reference mean and the best
proxy-reference mean.

### Efficiency metrics

These summarize how fast a method becomes good.

- `first_budget_top1_match`

the first checkpoint where the recommended move matches the proxy-truth best
move.

- `first_budget_epsilon_optimal`

the first checkpoint where simple regret falls below the chosen
\(\epsilon\)-threshold.

- `first_runtime_top1_match`

the first runtime where top-1 correctness is achieved.

- `first_runtime_epsilon_optimal`

the first runtime where \(\epsilon\)-optimality is achieved.

- `auc_top1_match`

the area under the budget-path curve for `top1_match`.

- `auc_simple_regret`

the area under the budget-path curve for `simple_regret`.

### Calibration metrics

These ask whether the package’s reported confidence is honest.

- `brier_top1`

for predicted probability best \(\hat{p}\) and realized top-1 correctness
\(y \in \{0,1\}\),

\[
(\hat{p} - y)^2
\]

- calibration-bin summaries

the package bins `recommended_prob_best` into equal-width probability bins and
reports:

  - `mean_predicted_prob_best`
  - `observed_top1_rate`
  - `calibration_gap`
  - `ece_component`

where `ece_component` is the bin’s contribution to an expected calibration error
summary.

### Runtime summaries

The run and checkpoint tables also report:

- `runtime_seconds`: wall-clock runtime through the checkpoint.
- `rollout_throughput`: completed rollouts per second through the checkpoint.

## Public API

The public package surface is intentionally grouped into a small number of
functional blocks.

### Board, roll, move, and simulation functions

- `bg_board()`: construct a board object directly.
- `bg_initial_board()`: return the standard opening board.
- `bg_roll()`: construct a roll object.
- `bg_legal_moves()`: enumerate legal move sequences for a state and roll.
- `bg_apply_move_sequence()`: apply a chosen legal move sequence.
- `bg_play_turn()`: simulate one turn from a board.
- `bg_play_game()`: simulate a full game.
- `bg_print_board()`: print a board in the package’s display format.
- `bg_validate_board()`: validate board structure.

### Problem and truth functions

- `bg_problem()`: build one decision problem.
- `bg_reference()`: build a proxy reference for one problem.
- `bg_truth_state()`: build or load one saved state-level proxy truth.
- `bg_truth_opening()`: build proxy truth for opening-roll batteries.
- `bg_truth_battery()`: build proxy truth for a list of problems.
- `bg_truth_save()`: save truth objects.
- `bg_truth_load()`: load truth objects.
- `bg_truth_diagnostics()`: summarize gaps, rankings, and uncertainty in a
  truth object.
- `bg_study_save()`: save comparison-study objects.
- `bg_study_load()`: load comparison-study objects.

### Thompson and comparator methods

- `bg_ts_run()`: run canonical Thompson sampling.
- `bg_ttts_run()`: run top-two Thompson sampling.
- `bg_uniform_run()`: run equal allocation.
- `bg_ucb_run()`: run UCB allocation.
- `bg_compare_algorithms()`: compare methods across budgets and seeds.
- `bg_compare_posteriors()`: compare posterior families for a fixed reward
  model.
- `bg_compare_reward_models()`: compare coherent reward/posterior stacks.

### Evaluation functions

- `bg_eval_top1()`: best-move and regret summaries.
- `bg_eval_rank()`: ranking-recovery summaries.
- `bg_eval_allocation()`: allocation-behavior summaries.
- `bg_eval_efficiency()`: budget-path and runtime-efficiency summaries.
- `bg_eval_calibration()`: probability-best calibration summaries.
- `bg_eval_reference_aware()`: one combined evaluation panel.

### State and feature functions

- `bg_board_features()`: extract board-level structural features.
- `bg_move_features()`: extract move-level structural features.
- `bg_state_classify()`: classify state type.
- `bg_state_difficulty()`: summarize structural and truth-aware difficulty.

### Plot functions

- `plot_bg_truth()`: proxy-reference truth plots.
- `plot_bg_ts_trace()`: Thompson trace plots over the budget path.
- `plot_bg_allocation()`: final allocation plots by move.
- `plot_bg_budget_curve()`: method-comparison curves over budget.
- `plot_bg_rank_compare()`: estimated rank versus proxy-truth rank plots.
- `plot_bg_posterior_compare()`: posterior/reward-model comparison plots.

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

## Development And Installation

Install from a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
library(backgammonr)
```

For development:

```r
pkgload::load_all(".")
```

## Notebooks

The current function-first notebook sequence is:

1. `01_save_all_21_opening_truths`
2. `02_roll_1_6_thompson_over_budget`
3. `03_roll_1_6_thompson_vs_non_thompson`
4. `04_roll_1_6_thompson_variants`
5. `05_roll_1_6_thompson_model_comparison`
6. `06_roll_1_6_mc_screening_efficiency_and_calibration`

These notebooks are meant to be run directly and modified directly.

## Final Scientific Frame

`backgammonr` is a package about **finite-budget Monte Carlo decision-making**,
using backgammon as a realistic and interpretable stochastic laboratory.

Its core scientific distinctions are:

- rollout-model value versus unavailable game-theoretic truth;
- high-budget proxy reference versus low-budget algorithm output;
- posterior uncertainty versus Monte Carlo uncertainty;
- method quality versus method cost.

That is the package identity.
