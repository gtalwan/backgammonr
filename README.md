# backgammonr

`backgammonr` is an R package for one narrow research problem:

> Given a fixed rollout budget for one local backgammon decision, how should
> that budget be allocated across legal moves so the best move or the important
> part of the move ranking is identified as accurately as possible?

This is not a general backgammon bot and it is not a game-theoretic solver.
It is a research package for finite-budget local decision problems under a
declared rollout model.

## What The Package Studies

The package studies one object repeatedly:

- one board state
- one realized dice roll
- the legal root actions under that roll
- one declared continuation rollout policy
- one declared reward definition
- one declared posterior model
- one fixed simulation budget

That object is represented by `bg_problem()`.

The package then asks:

1. Which allocation method finds the best move fastest?
2. Which methods spend budget on the moves that actually matter?
3. Which methods recover the ranking, not just the winner?
4. How sensitive are the conclusions to the reward/model stack?
5. How stable is the high-budget proxy truth itself?

## Core Story

The package is organized around six layers:

1. Engine mechanics
2. Problem construction
3. Proxy truth / reference construction
4. Thompson-family allocation methods
5. Diagnostics and metrics
6. Repeated studies and plots

Keep these ideas separate:

- truth: the high-budget Monte Carlo target
- reward model: how rollout outcomes are scored
- posterior model: how finite rollout data are summarized
- algorithm: how the next rollout is allocated

## Learn These Functions First

If you want useful results quickly, do not start by learning the whole export
surface. Start with this smaller user path.

### 1. Problem plus cached truth

- `bg_opening_problem()`: build the local decision problem for one opening roll.
- `bg_truth_load()`: load one preserved master truth from disk.
- `bg_truth_project()`: project that master truth into the reward/posterior
  stack you want to study.

### 2. Direct allocation methods

These are the main algorithm front doors:

- `bg_ts_run()`
- `bg_ttts_run()`
- `bg_multi_sample_ts_run()`
- `bg_soft_elimination_ts_run()`
- `bg_forced_exploration_ts_run()`
- `bg_top_k_ts_run()`
- `bg_equal_run()`

### 3. One evaluation layer

- `bg_eval_reference_aware()`: the main compact evaluation panel.
- `bg_ts_diagnostics()`: the main one-run diagnostics bundle.

### 4. One plotting layer

- `plot_bg_truth()`
- `plot_bg_ts_trace()`
- `plot_bg_rank_compare()`
- `plot_bg_allocation()`
- `plot_bg_budget_curve()`

### 5. One optional study wrapper

- `bg_compare_algorithms()`: convenience wrapper that repeatedly calls the
  direct run functions over methods, budgets, and seeds.

## Ignore These Until You Need Them

Most users do not need to touch these at first:

- engine-mechanics helpers like `bg_board()`, `bg_roll()`, `bg_legal_moves()`,
  and `bg_play_game()`
- truth-building helpers like `bg_reference()`, `bg_truth_state()`,
  `bg_master_truth_state()`, and `bg_opening_truth_build_*()`
- fine-grained metric helpers like `bg_eval_top1()`, `bg_eval_rank()`, and
  `bg_eval_allocation()`
- convenience study/persistence helpers like `bg_opening_compare_study()`,
  `bg_study_save()`, and `bg_study_load()`
- legacy or demoted baselines like `bg_ucb_run()` and `bg_uniform_run()`

The package still exposes those functions, but they are not the main story.

## The Preserved Master Cache

The repo already contains a preserved opening master-truth cache:

- [cache/opening_truths_master](/Users/gabrielalwan/Downloads/backgammonr/cache/opening_truths_master)

Those files are the main cached 21-opening master truths. The presentation
scripts use this cache directly and then project it into the headline model
stacks without rerunning the expensive truth simulation.

For a first example, load one opening directly from the master cache:

```r
library(backgammonr)

master_path_1_6 <- list.files(
  file.path("cache", "opening_truths_master"),
  pattern = "^opening_1_6_.*\\.rds$",
  full.names = TRUE
)[[1L]]

truth_master_1_6 <- bg_truth_load(master_path_1_6)
```

## Headline Model Stacks

The package story emphasizes three coherent reward/posterior stacks:

1. `win_loss + beta_bernoulli`
2. `scalar_payoff + student_t_marginal`
3. `categorical_outcome + dirichlet_multinomial`

Example:

```r
library(backgammonr)

truth_binary <- bg_truth_project(
  truth_master_1_6,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0
)

truth_scalar_student_t <- bg_truth_project(
  truth_master_1_6,
  reward_model = "scalar_payoff",
  posterior_model = "student_t_marginal",
  unresolved_value = 0.5
)

truth_dirichlet <- bg_truth_project(
  truth_master_1_6,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5
)
```

## Main Workflows

### 1. Build one opening problem

```r
library(backgammonr)

problem <- bg_opening_problem("1-6")
print(problem)
```

### 2. Load one preserved master truth and project it

```r
library(backgammonr)

master_path_1_6 <- list.files(
  file.path("cache", "opening_truths_master"),
  pattern = "^opening_1_6_.*\\.rds$",
  full.names = TRUE
)[[1L]]

truth_master_1_6 <- bg_truth_load(master_path_1_6)

truth_1_6 <- bg_truth_project(
  truth_master_1_6,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0
)

plot_bg_truth(truth_1_6)
```

### 3. Run one direct method

```r
library(backgammonr)

fit_ts <- bg_ts_run(
  problem = truth_1_6$problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth_1_6$reference,
  seed = 1L
)
```

### 4. Run TTTS

```r
fit_ttts <- bg_ttts_run(
  problem = truth_1_6$problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth_1_6$reference,
  seed = 1L
)
```

### 5. Run one TS-family variant

```r
fit_soft <- bg_soft_elimination_ts_run(
  problem = truth_1_6$problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth_1_6$reference,
  seed = 1L
)
```

### 6. Run equal allocation

```r
fit_equal <- bg_equal_run(
  problem = truth_1_6$problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth_1_6$reference,
  seed = 1L
)
```

### 7. Diagnostics

```r
diag_ts <- bg_ts_diagnostics(fit_ts, truth = truth_1_6)
eval_ts <- bg_eval_reference_aware(fit_ts, truth = truth_1_6)
```

### 8. Plotting

```r
plot_bg_truth(truth_1_6)
plot_bg_ts_trace(fit_ts)
plot_bg_rank_compare(fit_ts, truth = truth_1_6)
plot_bg_allocation(fit_ts, truth = truth_1_6)
plot_bg_budget_curve(fit_ts, metric = "simple_regret", truth = truth_1_6)
```

### 9. Repeated study

`bg_compare_algorithms()` is a convenience wrapper around the direct method
front doors. Conceptually it is:

- choose problems
- choose methods
- choose seeds
- run the direct method functions repeatedly
- bind the checkpoint results

```r
comparison <- bg_compare_algorithms(
  problems = truth_1_6$problem,
  methods = c("thompson", "top_two_thompson", "equal"),
  budgets = c(32L, 64L, 128L, 256L),
  seeds = 1:4,
  proxy_references = truth_1_6$reference,
  n_cores = 2L,
  parallel = TRUE,
  progress = TRUE
)

plot_bg_budget_curve(comparison, metric = "top1_match", truth = truth_1_6)
```

For the full 21-opening battery:

```r
source(file.path("Presentation", "05_all_openings_ts_family.R"))
```

## Primary Metrics

The package can compute many metrics, but the main presentation story should
center on:

- `top1_match`
- `simple_regret`
- `selected_reference_rank`
- `truth_top2_hit`
- `share_top2_truth`
- `share_mc_screened_suboptimal`
- `gap_weighted_wasted_allocation`
- `high_confidence_wrong_rate`
- `recommendation_instability`
- `top_two_gap_estimate`

Interpretation:

- `top1_match`: did the method choose the truth-best move?
- `simple_regret`: how costly was the mistake?
- `selected_reference_rank`: what truth rank did the chosen move have?
- `truth_top2_hit`: did the method at least get into the true top-2?
- `share_top2_truth`: did the budget go to the moves that matter?
- `share_mc_screened_suboptimal`: how much effort went to moves the truth already screens out?
- `gap_weighted_wasted_allocation`: how much budget was wasted, weighted by how bad those moves are?
- `high_confidence_wrong_rate`: how often was the method confidently wrong?
- `recommendation_instability`: how much do seeds disagree?
- `top_two_gap_estimate`: how hard is the problem itself?

## Plots

The plotting layer is intentionally small and presentation-oriented.

Use:

- `plot_bg_truth()` for truth/reference structure
- `plot_bg_ts_trace()` for one-run checkpoint traces
- `plot_bg_rank_compare()` for estimated versus truth ranking
- `plot_bg_allocation()` for final allocation behavior
- `plot_bg_budget_curve()` for repeated-study budget curves

## Presentation Walkthrough

The main walkthrough now lives in:

- [Presentation/00_start_here.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/00_start_here.R)

The numbered presentation scripts are:

- [Presentation/01_truth_overview_openings.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/01_truth_overview_openings.R)
- [Presentation/02_one_opening_one_algorithm.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/02_one_opening_one_algorithm.R)
- [Presentation/03_one_opening_ts_family.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/03_one_opening_ts_family.R)
- [Presentation/04_one_opening_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/04_one_opening_ts_vs_equal.R)
- [Presentation/05_all_openings_ts_family.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/05_all_openings_ts_family.R)
- [Presentation/06_model_sensitivity_baseline_ts.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/06_model_sensitivity_baseline_ts.R)
- [Presentation/07_metrics_and_diagnostics.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/07_metrics_and_diagnostics.R)
- [Presentation/08_plot_gallery.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/08_plot_gallery.R)
- [Presentation/09_game_mechanics.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/09_game_mechanics.R)
- [Presentation/10_all_opening_rolls.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/10_all_opening_rolls.R)
- [Presentation/11_user_function_reference.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/11_user_function_reference.R)
- [Presentation/12_all_openings_ts_quick_contrasts.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/12_all_openings_ts_quick_contrasts.R)
- [Presentation/13_student_t_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/13_student_t_ts_vs_equal.R)
- [Presentation/14_dirichlet_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/14_dirichlet_ts_vs_equal.R)

Outputs are written to:

- [Presentation/output/plots](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/plots)
- [Presentation/output/tables](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/tables)
- [Presentation/output/studies](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/studies)

## Legacy and Demoted Paths

The package still contains some older baselines and compatibility layers, but
they are no longer the main story.

In particular, the package is now centered on:

- the Thompson family
- equal allocation
- coherent truth/reference workflows
- diagnostics and plotting

Legacy comparators such as UCB are retained for compatibility, but they are
not the recommended headline workflow.

## Repository Map

The main source files are:

- [R/bg_engine_api.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_engine_api.R)
- [R/bg_problem.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_problem.R)
- [R/bg_truth.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_truth.R)
- [R/bg_algorithms.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_algorithms.R)
- [R/bg_metrics.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_metrics.R)
- [R/bg_plots.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_plots.R)
- [R/bg_studies.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_studies.R)
- [R/bg_states.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_states.R)
- [R/bg_s3_methods.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_s3_methods.R)
- [R/bg_internal_utils.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_internal_utils.R)
- [R/bg_legacy.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_legacy.R)

The statistical/native layer is centered on:

- [src/alloc_core.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/alloc_core.cpp)
- [src/alloc_trace.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/alloc_trace.cpp)
- [src/policy_ts.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/policy_ts.cpp)
- [src/policy_ttts.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/policy_ttts.cpp)
- [src/policy_equal.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/policy_equal.cpp)
- [src/model_beta_bernoulli.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/model_beta_bernoulli.cpp)
- [src/model_student_t.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/model_student_t.cpp)
- [src/model_dirichlet_categorical.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/model_dirichlet_categorical.cpp)
- [src/truth_proxy.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/truth_proxy.cpp)

## Cache Preservation

The package and presentation scripts do not require deleting or regenerating
the preserved opening truth cache.

The cache under:

- [cache/opening_truths_master](/Users/gabrielalwan/Downloads/backgammonr/cache/opening_truths_master)

is intended to be reused directly.
