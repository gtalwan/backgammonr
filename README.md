# backgammonr

`backgammonr` is a statistical computing package for studying **finite-budget rollout allocation**, with **Thompson sampling** as the central method and backgammon as the stochastic testbed.

The package is not a strong-play backgammon AI package. Its core focus is best-action identification under simulation noise.


NOTE: The main statistical and simulation logic of the project lives in a few C++ files in the src/ folder: bg_allocation.cpp, bg_rollout.cpp, bg_thompson_rollout.cpp, bg_simulation.cpp, and bg_benchmark.cpp. These are really the core of the project. They handle the Monte Carlo simulation, rollout evaluation, adaptive sampling methods, and the benchmarking framework used to compare different approaches. These files are also heavily commented, which makes it fairly straightforward to follow the logic of how simulations are generated, how rollouts are evaluated, and how the allocation strategies are applied.

Overall these five files contain the core statistical and simulation logic of the project. Many of the other files in the repository, particularly those related to plotting, board visualization, and parts of the game mechanics, were largely helped along by generative AI during development. Because of that, those files will need additional inspection and cleanup to verify correctness, simplify the structure, and remove redundancy. The focus so far has been making sure the statistical allocation and simulation framework is working properly, and those components are primarily implemented in the C++ files described above.

bg_allocation.cpp is the most important file from the statistical side. This is where the allocation logic is implemented. The goal of this file is to determine how a fixed simulation budget should be distributed across candidate moves. Instead of dividing simulations evenly, the code supports several strategies including equal allocation, greedy allocation based on posterior means, UCB style rules, Thompson sampling, top two Thompson sampling, and an OCBA style allocation rule. The file first generates the legal moves and collapses moves that lead to the same resulting board state so that simulation effort is not wasted on equivalent positions. For each candidate move the code runs rollouts, which simulate the rest of the game starting from that position. Outcomes are tracked using a Beta Bernoulli framework where each move maintains alpha and beta parameters representing a posterior distribution over its win probability. These posterior estimates are then used by the allocation rules to decide which move should receive the next simulation.

bg_rollout.cpp provides a simpler interface for running traditional rollout evaluations. In this case the allocation method is fixed to equal allocation, so each candidate move receives roughly the same number of simulations. Rather than implementing a completely separate rollout system, this file simply calls the allocation engine with the equal allocation option. Its main role is to expose cleaner user facing functions for evaluating moves and returning summaries such as win rates and simulation counts.

bg_thompson_rollout.cpp works in a similar way but focuses on Thompson sampling. Instead of allocating simulations evenly, it uses Thompson sampling to determine which candidate move should receive the next rollout. The underlying Thompson sampling logic still lives in bg_allocation.cpp; this file mainly acts as a wrapper that exposes a simpler interface for running Thompson based rollout experiments and returning the most relevant statistics.

bg_simulation.cpp handles full game simulation. Instead of evaluating a single move, this file simulates complete backgammon games between two policies. Each turn a player selects a move according to its policy, which might be random play, rollout based decision making, or Thompson sampling. The file includes functions for simulating individual games and for running batches of many games. After simulations are complete the results are aggregated into summaries such as win rates, number of turns, and whether games reached the turn limit.

bg_benchmark.cpp provides a framework for running structured experiments that compare different methods. It supports evaluating decision methods on multiple positions as well as running repeated game simulations between policies. One important feature is that it carefully manages random number seeds so experiments are reproducible and comparisons between methods are fair. The benchmarking code records which moves each method selects, how often those selections match a reference estimate, and how long each method takes to run.




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
