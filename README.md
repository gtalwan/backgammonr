# backgammonr

`backgammonr` is a statistical computing package for studying **finite-budget rollout allocation**, with **Thompson sampling** as the central method and backgammon as the stochastic testbed.

The package is not a strong-play backgammon AI package. Its core focus is best-action identification under simulation noise.

## Core Research Question

For one decision instance

- `d = (state, realized_roll)`,
- legal actions `A_d = {1, ..., K}`,
- rollout rewards `Y_i in [0,1]`,
- action means `mu_i = E[Y_i]`,

we study whether Thompson sampling allocates a finite rollout budget `N` efficiently for recommending the rollout-model best action.

Under budget `N`, a method outputs `a_hat_N`. We evaluate quality relative to a **high-budget reference estimate (proxy truth)**.

## What Is Central vs Baseline

Central method family:

- `evaluate_actions_thompson()`
- `evaluate_actions_ttts()` (Top-Two Thompson)
- `trace_thompson_allocation()`
- `compare_thompson_to_reference()`
- `benchmark_thompson()`
- `summarize_thompson_benchmark()`

Comparison baselines:

- `evaluate_actions_equal()`
- `evaluate_actions_ucb()`
- `evaluate_actions_ocba()`
- `evaluate_actions_greedy()`

Reference-estimation tools:

- `approximate_action_reference()` (alias of `approximate_action_truth()`)
- `certify_reference_truth()`

## Installation (Local Source)

```r
# Run from the package root directory
install.packages(".", repos = NULL, type = "source")
library(backgammonr)
```

## Thompson Quick Start

```r
board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

res <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = 1000L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

res$results[order(res$results$rank), c(
  "rank", "move_label", "allocation_count", "estimate",
  "prob_best", "posterior_expected_regret", "recommended"
)]
```

## Thompson vs Reference Example

```r
ref <- approximate_action_reference(
  board = bg_initial_board(),
  roll = bg_roll(1L, 6L),
  truth_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

cert <- certify_reference_truth(reference = ref)

cmp <- compare_thompson_to_reference(
  board = bg_initial_board(),
  roll = bg_roll(1L, 6L),
  method = "thompson",
  total_budget = 1000L,
  reference = ref,
  reference_certificate = cert,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

cmp$summary
head(cmp$action_table)
```

## Main Evaluation Targets

- proxy PCS (correct selection vs reference-best action),
- simple regret vs reference estimate,
- action-value MSE vs reference estimate,
- runtime and speed/accuracy tradeoff.

## How To Evaluate Thompson Behavior

Use this sequence on each case:

1. Run `evaluate_actions_thompson(...)` and inspect:
   - recommended action,
   - allocation concentration (`alloc_n`),
   - uncertainty (`uncertainty_sd`, `ci95_*`),
   - `prob_best` and `exp_regret`.
2. Compare to a high-budget reference with `compare_thompson_to_reference(...)`:
   - proxy PCS,
   - simple regret,
   - MSE,
   - runtime.
3. Inspect dynamics with `trace_thompson_allocation(...)`:
   - leader stability vs checkpoint,
   - allocation shifts over budget.
4. Compare against baselines and across budgets:
   - `compare_methods_on_position(...)`,
   - `study_budget_tradeoff(...)`,
   - `benchmark_thompson(...)`.

Interpret results honestly:

- Encouraging: early PCS gains, lower regret, focused allocation, shrinking uncertainty.
- Cautionary: small-budget instability, state-dependent gains, near-tie ambiguity, and proxy-truth limitations.

## Documentation

- Deep-documentation generator script:
  - [scripts/generate_vignettes.R](scripts/generate_vignettes.R)
- Example-validation runner (executes core vignette workflow and writes output artifacts):
  - [scripts/run_vignette_examples.R](scripts/run_vignette_examples.R)
- Canonical vignette sequence:
  - [vignettes/01_project_motivation.Rmd](vignettes/01_project_motivation.Rmd)
  - [vignettes/02_thompson_sampling_theory.Rmd](vignettes/02_thompson_sampling_theory.Rmd)
  - [vignettes/03_package_methodology.Rmd](vignettes/03_package_methodology.Rmd)
  - [vignettes/04_backgammon_basics.Rmd](vignettes/04_backgammon_basics.Rmd)
  - [vignettes/05_thompson_workflow.Rmd](vignettes/05_thompson_workflow.Rmd)
  - [vignettes/06_main_question_example.Rmd](vignettes/06_main_question_example.Rmd)
  - [vignettes/07_easy_function_calls.Rmd](vignettes/07_easy_function_calls.Rmd)
- Thompson motivation and package positioning:
  - [docs/THOMPSON_MOTIVATION.md](docs/THOMPSON_MOTIVATION.md)
- End-to-end mechanics + Thompson workflow/testing guide:
  - [docs/STATISTICAL_WORKFLOW.md](docs/STATISTICAL_WORKFLOW.md)
- Standalone copy-paste example bubbles:
  - [docs/EXAMPLE_BUBBLES.md](docs/EXAMPLE_BUBBLES.md)
- Function reference:
  - [docs/FUNCTION_REFERENCE.md](docs/FUNCTION_REFERENCE.md)
- Implementation and optimization deep dive:
  - [docs/IMPLEMENTATION_DEEP_DIVE.md](docs/IMPLEMENTATION_DEEP_DIVE.md)

Regenerate and validate docs:

```r
system("Rscript scripts/generate_vignettes.R --check")
system("Rscript scripts/run_vignette_examples.R --quick")
```

## Interpretation Notes

- “Best action” is model-relative: best under the rollout model and settings.
- High-budget reference output is a proxy truth, not exact game-theoretic truth.
- Baseline methods are retained for comparison; Thompson sampling is the conceptual center.
