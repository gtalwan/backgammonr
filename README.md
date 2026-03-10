# backgammonr

`backgammonr` is an R package for finite-budget Monte Carlo action evaluation
in backgammon. It treats a single board state plus a realized dice roll as a
ranking-and-selection problem under simulation noise:

- what are the legal actions?
- how should a fixed rollout budget be allocated across them?
- how much evidence supports the recommended move?

The package is centered on Thompson sampling and closely related allocation
rules. Backgammon is the stochastic testbed, not the final objective. The core
contribution is a statistical workflow for best-action identification when
rollout evaluations are noisy and budget is limited.

## Statistical framing

For a decision instance `d = (state, realized_roll)` with legal actions
`A_d = {1, ..., K}`, the package studies finite-budget action evaluation under
Monte Carlo rewards in `[0, 1]`. A method receives a rollout budget `N`,
distributes that budget across candidate actions, and returns a recommended
action `a_hat_N`.

The main research question is:

> Does Thompson sampling use a fixed rollout budget more effectively than simple
> baselines when the goal is to identify the rollout-model best action?

The package therefore emphasizes:

- posterior-style uncertainty summaries;
- probability-of-being-best style diagnostics;
- proxy probability of correct selection (PCS);
- simple regret against a high-budget reference;
- action-value MSE;
- runtime and speed/accuracy tradeoffs.

## What The Package Is And Is Not

`backgammonr` is:

- a research-oriented statistical computing package;
- a backgammon rollout engine wrapped in R for experimentation;
- a toolkit for comparing adaptive and non-adaptive allocation rules.

`backgammonr` is not:

- a full-strength competitive backgammon bot;
- a claim of exact game-theoretic truth;
- a package focused on the doubling cube, match equity, or human-style play.

## Installation

### Install from GitHub

```r
install.packages("remotes")
remotes::install_github("gtalwan/backgammonr")
library(backgammonr)
```

### Install from local source

```r
setwd("/path/to/backgammonr")
install.packages(".", repos = NULL, type = "source")
library(backgammonr)
```

## Recommended Workflow

1. Create a board and realized roll with `bg_initial_board()` and `bg_roll()`.
2. Enumerate candidate actions with `generate_legal_moves()`.
3. Run a finite-budget evaluator such as `evaluate_actions_thompson()`.
4. Build a higher-budget proxy truth with `approximate_action_reference()`.
5. Compare finite-budget output to the reference with
   `compare_thompson_to_reference()`.
6. Study stability across budgets, variance controls, and benchmark cases.

This workflow is the backbone of the vignettes and the main user-facing API.

## Main Statistical Functions

| Question | Primary functions | What they do |
| --- | --- | --- |
| What position am I analyzing? | `bg_initial_board()`, `bg_roll()`, `generate_legal_moves()`, `summarize_legal_moves()` | Construct a reproducible decision instance and inspect the candidate action set. |
| How should I allocate a fixed budget now? | `evaluate_actions_thompson()`, `evaluate_actions_ttts()` | Run Thompson-family adaptive allocation and return recommendation, uncertainty, and allocation summaries. |
| What baselines should I compare against? | `evaluate_actions_equal()`, `evaluate_actions_ucb()`, `evaluate_actions_ocba()`, `evaluate_actions_greedy()` | Evaluate simpler or alternative budget-allocation rules on the same position. |
| What is the proxy truth for this position? | `approximate_action_reference()`, `approximate_action_truth()`, `certify_reference_truth()` | Build a higher-budget reference estimate and attach a certificate describing the strength of the reference. |
| How close is Thompson to the reference? | `compare_thompson_to_reference()`, `compare_methods_on_position()` | Measure proxy PCS, simple regret, MSE, runtime, and action-by-action gaps. |
| How does allocation evolve over budget? | `trace_thompson_allocation()`, `trace_allocation_history()`, `plot_thompson_convergence()` | Inspect checkpoint-by-checkpoint allocation dynamics and recommendation stability. |
| How sensitive are results to budget and variance controls? | `study_budget_tradeoff()`, `study_variance_controls()` | Run structured experiments over budgets, dice modes, and common random number settings. |
| How do methods compare over many cases? | `benchmark_allocation_methods()`, `benchmark_thompson()`, `summarize_thompson_benchmark()` | Benchmark methods across curated positions and summarize selection accuracy and runtime. |
| How do I explain or visualize results? | `bg_explain_recommendation()`, `bg_analysis_report()`, `plot_thompson_vs_baselines()`, `bg_plot_benchmark_summary()` | Turn raw evaluation output into readable tables, explanations, and plots. |

## Quick Start: Finite-Budget Thompson Evaluation

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

legal <- generate_legal_moves(board, roll)
summarize_legal_moves(legal, max_candidates = 10L)

th <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = 800L,
  rollout_policy = "random",
  max_rollout_turns = 220L,
  fast_diagnostics = FALSE,
  seed = 11L
)

summary(th)[, c(
  "method",
  "total_budget",
  "recommended_move_label",
  "recommended_estimate",
  "recommended_prob_best",
  "recommended_expected_regret",
  "recommended_allocation_count",
  "runtime_seconds"
)]

th[["results"]][, c(
  "move_label",
  "recommended",
  "allocation_count",
  "estimate",
  "posterior_sd",
  "prob_best",
  "posterior_expected_regret"
)]
```

This is the central package call: it returns both a one-row summary and a
candidate-level result table with allocation counts, estimates, uncertainty,
probability-best style diagnostics, and a recommendation flag.

## Reference And Comparison Workflow

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = 5000L,
  rollout_policy = "random",
  max_rollout_turns = 220L,
  seed = 21L
)

cert <- certify_reference_truth(ref)

cmp <- compare_thompson_to_reference(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = 800L,
  reference = ref,
  reference_certificate = cert,
  rollout_policy = "random",
  max_rollout_turns = 220L,
  seed = 11L
)

cmp$summary
cmp$action_table[, c(
  "move_label",
  "estimate",
  "reference_estimate",
  "estimate_error",
  "prob_best",
  "simple_regret",
  "mse"
)]
```

The reference is a high-budget proxy truth, not exact truth. That distinction is
important when interpreting proxy PCS or regret values on difficult positions.

## Study And Benchmark Helpers

Once a single position workflow is clear, the package exposes several higher
level experiment helpers.

- `study_budget_tradeoff()` asks how recommendation quality changes as total
  budget increases.
- `study_variance_controls()` examines the effect of dice modes and common
  random numbers on variance and runtime.
- `benchmark_allocation_methods()` and `benchmark_thompson()` run repeated
  method comparisons over curated benchmark cases.
- `summarize_thompson_benchmark()` compresses multi-case experiments into
  readable accuracy, regret, and runtime summaries.

These functions are designed for repeated statistical experiments rather than
one-off exploratory calls.

## Key Output Columns

Most action-evaluation tables use a shared vocabulary. The most important
columns are:

- `move_label`: a readable label for the candidate move.
- `allocation_count`: how many rollouts the method spent on that move.
- `estimate`: the estimated win probability or value under the rollout model.
- `posterior_sd`: posterior-style uncertainty summary for the estimate.
- `prob_best`: estimated probability that the move is currently best.
- `posterior_expected_regret`: regret-like summary under the posterior.
- `recommended`: whether the move is the method's recommendation.
- `runtime_seconds`: wall-clock runtime for the evaluation call.

These defaults are meant to be interpretable without hiding the underlying
simulation structure.

## Repository Layout

The package is organized around a small set of R and C++ files that carry most
of the statistical logic.

| Path | Role |
| --- | --- |
| `R/allocation_methods.R` | High-level action-evaluation APIs and shared allocation helpers. |
| `R/thompson_research.R` | Thompson-specific comparison, tracing, and benchmark helpers. |
| `R/statistical_studies.R` | Budget and variance-control study wrappers. |
| `R/statistical_api.R` | User-facing aliases and research-friendly entry points. |
| `R/visualization.R` | Tables, plots, and reporting helpers for evaluation outputs. |
| `src/bg_allocation.cpp` | Core finite-budget allocation engine and posterior bookkeeping. |
| `src/bg_rollout.cpp` | Equal-allocation rollout wrappers. |
| `src/bg_thompson_rollout.cpp` | Thompson-family rollout wrappers. |
| `src/bg_simulation.cpp` | Full-game simulation kernels and matchup drivers. |
| `src/bg_benchmark.cpp` | Structured benchmarking across positions and methods. |

## Documentation

The package documentation is layered from introductory workflow to deeper
implementation material.

- [vignettes/05_thompson_workflow.Rmd](vignettes/05_thompson_workflow.Rmd)
  gives a Thompson-centered walkthrough.
- [vignettes/06_main_question_example.Rmd](vignettes/06_main_question_example.Rmd)
  develops the main research question in more detail.
- [vignettes/07_easy_function_calls.Rmd](vignettes/07_easy_function_calls.Rmd)
  is the most direct, step-by-step user workflow.
- [docs/FUNCTION_REFERENCE.md](docs/FUNCTION_REFERENCE.md) catalogs the API.
- [docs/IMPLEMENTATION_DEEP_DIVE.md](docs/IMPLEMENTATION_DEEP_DIVE.md)
  explains the internal design and optimization choices.

## Interpretation Notes

- "Best action" always means best under the current rollout model and rollout
  settings.
- A higher-budget reference is still a Monte Carlo estimate, not exact truth.
- Small-budget recommendations can be unstable on near-tie positions.
- Runtime comparisons should be interpreted together with recommendation
  quality, not in isolation.
