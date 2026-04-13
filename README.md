# backgammonr

`backgammonr` is an R package for one narrow research program:

> Given a fixed simulation budget for one local backgammon decision problem,
> how should rollouts be allocated across legal moves so the best move, or the
> important part of the move ranking, is identified as accurately and
> efficiently as possible under a declared rollout model?

This repository is not organized as a general backgammon bot. It is organized
as a research package with four connected layers:

1. a backgammon engine
2. a proxy-truth layer built from high-budget rollout simulation
3. a statistical allocation layer centered on Thompson-style methods
4. an analysis layer that compares methods against fixed truth objects

The package's truth objects are always:

- high-budget Monte Carlo proxy truths under the package's declared rollout environment

They are not:

- exact backgammon truth
- expert truth
- GNU Backgammon truth
- game-theoretic equilibrium truth

## Table of contents

- [What this package studies](#what-this-package-studies)
- [What the package is not](#what-the-package-is-not)
- [Where to start in the repo](#where-to-start-in-the-repo)
- [Core mental model](#core-mental-model)
- [Truth, reward, posterior, and algorithm are different](#truth-reward-posterior-and-algorithm-are-different)
- [Package architecture](#package-architecture)
- [Main exported functions](#main-exported-functions)
- [Game mechanics workflow](#game-mechanics-workflow)
- [Opening-roll workflow](#opening-roll-workflow)
- [Master truth workflow: simulate once, reuse across reward systems](#master-truth-workflow-simulate-once-reuse-across-reward-systems)
- [Custom-board workflow](#custom-board-workflow)
- [Reward systems](#reward-systems)
- [Posterior models](#posterior-models)
- [Allocation policies](#allocation-policies)
- [Diagnostics and metrics](#diagnostics-and-metrics)
- [Truth certification and stability](#truth-certification-and-stability)
- [Analysis entrypoints](#analysis-entrypoints)
- [Cache directories](#cache-directories)
- [Performance notes](#performance-notes)
- [Repository map](#repository-map)
- [Reproducibility](#reproducibility)

## What this package studies

The package studies one object repeatedly:

- one board state
- one realized dice roll
- the legal moves available under that roll
- a declared continuation rollout policy
- a declared reward definition
- a declared posterior model
- a fixed simulation budget

That object is represented by `bg_problem()`.

The scientific questions are then:

1. Which method finds the best move fastest under a fixed budget?
2. Which method allocates budget toward the truly competitive moves?
3. How well does the method recover the move ranking, not just the winner?
4. How sensitive are those conclusions to the chosen reward system?
5. How stable is the high-budget proxy truth itself?

## What the package is not

`backgammonr` is intentionally not centered on:

- a large method zoo
- a large plotting zoo
- full-game match equity
- bot-strength move evaluation
- polished end-user backgammon play

The focus is narrower:

- local move-choice problems
- proxy truth from expensive rollouts
- Thompson-style allocation methods
- equal allocation as the simple baseline
- clear evaluation and diagnostics

## Where to start in the repo

If you are opening the repository for the first time, start here:

- [`analysis/00_walkthrough_and_results.R`](analysis/00_walkthrough_and_results.R): the main guided walkthrough and results file
- [`analysis/01_opening_truth_overview.R`](analysis/01_opening_truth_overview.R): truth-battery summaries
- [`analysis/02_ts_vs_ttts_opening_study.R`](analysis/02_ts_vs_ttts_opening_study.R): the coherent TS vs TTTS opening study
- [`analysis/03_build_reward_truth_caches.R`](analysis/03_build_reward_truth_caches.R): build one master opening battery, then materialize reward-specific caches

The preserved repo-local million-rollout opening cache lives in:

- [`cache/opening_truths_restart/`](cache/opening_truths_restart)

## Core mental model

The shortest practical mental model is:

- engine: backgammon mechanics
- truth: expensive Monte Carlo reference
- model: how finite rollout data are summarized statistically
- algorithm: how the rollout budget is allocated
- metrics: did it work?

This separation matters.

The package feels complicated when those layers are blurred together. It
becomes much easier to reason about once they are kept separate.

## Truth, reward, posterior, and algorithm are different

These four ideas are distinct.

### Truth

Truth means:

- the high-budget reference quantity you are trying to estimate for each move

Truth depends on things like:

- board state
- realized roll
- rollout continuation policy
- max rollout turns
- unresolved handling
- dice mode / CRN settings
- reward definition

Truth does not fundamentally depend on:

- whether you later use Thompson sampling or TTTS
- whether you later summarize uncertainty with a Student-t or Beta posterior

### Reward system

Reward system means:

- how rollout outcomes are converted into move value

Changing the reward system changes the estimand.

### Posterior model

Posterior model means:

- how finite rollout data are summarized into uncertainty for one move

Changing the posterior model does not necessarily change the estimand. It
changes the statistical update used by TS or TTTS.

### Allocation algorithm

Allocation algorithm means:

- how the next rollout is assigned across competing moves

Examples:

- Thompson sampling
- top-two Thompson sampling
- equal allocation

## Package architecture

The cleaned repository is organized around these source files.

### Engine-facing R surface

- [`R/bg_engine_api.R`](R/bg_engine_api.R)
- [`R/bg_s3_methods.R`](R/bg_s3_methods.R)

### Problem and truth layer

- [`R/bg_problem.R`](R/bg_problem.R)
- [`R/bg_truth.R`](R/bg_truth.R)
- [`R/bg_states.R`](R/bg_states.R)

### Statistical algorithms and studies

- [`R/bg_algorithms.R`](R/bg_algorithms.R)
- [`R/bg_metrics.R`](R/bg_metrics.R)
- [`R/bg_studies.R`](R/bg_studies.R)
- [`R/bg_plots.R`](R/bg_plots.R)

### Internal helpers and legacy compatibility

- [`R/bg_internal_utils.R`](R/bg_internal_utils.R)
- [`R/bg_legacy.R`](R/bg_legacy.R)
- [`R/backgammonr-package.R`](R/backgammonr-package.R)

### Core native engine files

- [`src/bg_board.cpp`](src/bg_board.cpp)
- [`src/bg_dice.cpp`](src/bg_dice.cpp)
- [`src/bg_move.cpp`](src/bg_move.cpp)
- [`src/bg_movegen.cpp`](src/bg_movegen.cpp)
- [`src/bg_rules.cpp`](src/bg_rules.cpp)
- [`src/bg_game.cpp`](src/bg_game.cpp)
- [`src/engine_playout.cpp`](src/engine_playout.cpp)
- [`src/engine_players.cpp`](src/engine_players.cpp)

### Statistical native files

- [`src/alloc_core.cpp`](src/alloc_core.cpp)
- [`src/alloc_trace.cpp`](src/alloc_trace.cpp)
- [`src/policy_ts.cpp`](src/policy_ts.cpp)
- [`src/policy_ttts.cpp`](src/policy_ttts.cpp)
- [`src/policy_equal.cpp`](src/policy_equal.cpp)
- [`src/model_beta_bernoulli.cpp`](src/model_beta_bernoulli.cpp)
- [`src/model_student_t.cpp`](src/model_student_t.cpp)
- [`src/model_dirichlet_categorical.cpp`](src/model_dirichlet_categorical.cpp)
- [`src/model_bootstrap.cpp`](src/model_bootstrap.cpp)
- [`src/truth_proxy.cpp`](src/truth_proxy.cpp)
- [`src/metrics_summary.cpp`](src/metrics_summary.cpp)

## Main exported functions

### Problem setup

- `bg_problem()`
- `bg_opening_problem()`
- `bg_opening_rolls()`

### Truth and reference

- `bg_reference()`
- `bg_truth_state()`
- `bg_truth_battery()`
- `bg_truth_certify()`
- `bg_truth_stability()`
- `bg_master_truth_state()`
- `bg_opening_truth_build_one()`
- `bg_opening_truth_build_all()`
- `bg_opening_master_truth_build_one()`
- `bg_opening_truth_load_one()`
- `bg_opening_truth_load_all()`
- `bg_opening_truth_index()`
- `bg_truth_project()`
- `bg_reference_project()`

### Algorithms

- `bg_ts_run()`
- `bg_ttts_run()`
- `bg_multi_sample_ts_run()`
- `bg_soft_elimination_ts_run()`
- `bg_forced_exploration_ts_run()`
- `bg_top_k_ts_run()`
- `bg_equal_run()`

### Metrics and diagnostics

- `bg_eval_top1()`
- `bg_eval_topk()`
- `bg_eval_rank()`
- `bg_eval_restricted_rank()`
- `bg_eval_allocation()`
- `bg_eval_efficiency()`
- `bg_eval_calibration()`
- `bg_eval_reference_aware()`
- `bg_ts_diagnostics()`
- `bg_stopping_diagnostics()`
- `bg_posterior_adequacy()`

### Studies and plots

- `bg_compare_algorithms()`
- `bg_opening_compare_study()`
- `bg_sanity_lab()`
- `plot_bg_truth()`
- `plot_bg_ts_trace()`
- `plot_bg_budget_curve()`
- `plot_bg_rank_compare()`
- `plot_bg_allocation()`
- `plot_bg_posterior_compare()`

### Low-level engine helpers

- `bg_initial_board()`
- `bg_board()`
- `bg_validate_board()`
- `bg_inspect_board()`
- `bg_print_board()`
- `bg_roll()`
- `bg_roll_dice()`
- `bg_legal_moves()`
- `bg_apply_move_sequence()`
- `bg_play_turn()`
- `bg_play_game()`

## Game mechanics workflow

This is the smallest way to see the engine in action.

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1, 6)
moves <- bg_legal_moves(board, roll)
after <- bg_apply_move_sequence(board, moves[[1]])
turn <- bg_play_turn(board, roll = roll)
game <- bg_play_game(board, max_turns = 12L, selection = "random", seed = 123L)

bg_print_board(board)
print(roll)
print(moves[[1]])
bg_print_board(after)
print(turn)
bg_print_board(game$final_board)
```

This layer is pure game mechanics. It has nothing to do with Thompson sampling
yet.

## Opening-roll workflow

The 21 unordered opening rolls are the package's main study battery.

```r
library(backgammonr)

rolls <- bg_opening_rolls()

truth_1_6 <- bg_opening_truth_build_one(
  roll = "1-6",
  budget = 4096L,
  n_cores = 1L,
  parallel = FALSE,
  seed = 1L
)

fit_ts <- bg_ts_run(
  problem = truth_1_6$problem,
  budget = 256L,
  checkpoints = c(32L, 64L, 128L, 256L),
  proxy_reference = truth_1_6$reference,
  seed = 1L
)

bg_ts_diagnostics(fit_ts, truth = truth_1_6)
```

The opening-roll battery exists because it is:

- small enough to study repeatedly
- rich enough to show hard and easy move-choice problems
- standardized enough for coherent method comparisons

## Master truth workflow: simulate once, reuse across reward systems

This is the most important new workflow in the cleaned package.

The idea is:

1. build one expensive truth object under the full scored-outcome representation
2. store the per-move scored outcome counts
3. project that truth into the reward system you want
4. compare methods against the projected truth

This avoids rerunning a huge rollout simulation just because you changed the
reward projection.

### For one opening roll

```r
library(backgammonr)

n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

truth_1_6 <- bg_opening_master_truth_build_one(
  roll = "1-6",
  budget = 1000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_scalar <- bg_truth_project(
  truth_1_6,
  reward_model = "scalar_payoff",
  posterior_model = "student_t_marginal",
  unresolved_value = 0.5
)

truth_binary <- bg_truth_project(
  truth_1_6,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0
)

truth_categorical <- bg_truth_project(
  truth_1_6,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5
)
```

### For all 21 opening rolls

```r
library(backgammonr)

n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

rolls <- bg_opening_rolls()$opening_roll

master_truths <- setNames(
  lapply(
    rolls,
    function(r) {
      bg_opening_master_truth_build_one(
        roll = r,
        budget = 1000000L,
        n_cores = n_cores,
        parallel = TRUE,
        truth_block_size = 512L,
        cache = TRUE,
        cache_dir = "cache/opening_truths_master",
        overwrite = FALSE,
        seed = 1L
      )
    }
  ),
  rolls
)
```

### Why this matters

The expensive part is the rollout simulation. Once the master truth exists, the
package already has the information needed to derive:

- `scalar_payoff` truth
- `win_loss` truth
- `categorical_outcome` truth

for the same board, roll, and rollout environment.

## Custom-board workflow

The package is not restricted to the opening position.

### Build a custom board directly

```r
library(backgammonr)

points <- integer(24)
points[1] <- 2L
points[6] <- 5L
points[8] <- 3L
points[12] <- 5L
points[13] <- -5L
points[17] <- -3L
points[19] <- -5L
points[24] <- -2L

board <- bg_board(points = points, bar = c(0L, 0L), off = c(0L, 0L), turn = 1L)
roll <- bg_roll(3, 2)

mid_truth <- bg_master_truth_state(
  state = board,
  roll = roll,
  budget = 250000L,
  n_cores = 1L,
  parallel = FALSE,
  cache = FALSE,
  seed = 1L
)
```

### Or derive a non-opening board from actual play

```r
library(backgammonr)

game <- bg_play_game(
  board = bg_initial_board(),
  max_turns = 12L,
  selection = "random",
  seed = 123L
)

mid_board <- game$final_board
mid_roll <- bg_roll_dice(seed = 999L)

mid_truth <- bg_master_truth_state(
  state = mid_board,
  roll = mid_roll,
  budget = 250000L,
  n_cores = 1L,
  parallel = FALSE,
  cache = FALSE,
  seed = 1L
)
```

Use `bg_master_truth_state()` for arbitrary positions.

Use `bg_opening_master_truth_build_one()` for the standard opening battery.

## Reward systems

The package supports three main reward definitions.

### `win_loss`

Binary reward:

- any win category maps to `1`
- any loss category maps to `0`
- unresolved handling is controlled by `unresolved_value`

This is the clean Bernoulli-style setup.

### `scalar_payoff`

Bounded scalar reward.

In the current package implementation:

- any win category maps to `1`
- any loss category maps to `0`
- unresolved maps to `unresolved_value`

With the default unresolved handling, that means:

- loss = `0`
- unresolved = `0.5`
- win = `1`

This is a normalized decision-value scale, not raw backgammon points.

### `categorical_outcome`

Full scored outcome categories:

- `single_loss`
- `gammon_loss`
- `backgammon_loss`
- `unresolved`
- `single_win`
- `gammon_win`
- `backgammon_win`

This is the richest outcome representation in the package.

Its default payoff map is:

- backgammon loss -> `0`
- gammon loss -> `1/6`
- single loss -> `1/3`
- unresolved -> `0.5`
- single win -> `2/3`
- gammon win -> `5/6`
- backgammon win -> `1`

That richer categorical representation is exactly why the master-truth workflow
can support multiple later projections.

## Posterior models

Posterior models are grouped by reward system.

### `win_loss`

Central:

- `beta_bernoulli`

Secondary:

- `bootstrap`

### `scalar_payoff`

Central:

- `beta_pseudo`
- `student_t_marginal`

Secondary:

- `bootstrap`

### `categorical_outcome`

Central:

- `dirichlet_multinomial`

Secondary:

- `bootstrap`

### Important distinction

`reward_model` and `posterior_model` are not the same knob.

- `reward_model` changes what quantity you are trying to learn
- `posterior_model` changes how uncertainty about that quantity is summarized

## Allocation policies

The main package story is deliberately small.

### Central

- `bg_ts_run()`
- `bg_ttts_run()`
- `bg_equal_run()`

### Supported TS-family variants

- `bg_multi_sample_ts_run()`
- `bg_soft_elimination_ts_run()`
- `bg_forced_exploration_ts_run()`
- `bg_top_k_ts_run()`

### Legacy / secondary

- `bg_uniform_run()` as a deprecated alias to `bg_equal_run()`
- `bg_ucb_run()` as a legacy comparator

The package intentionally does not center a large baseline zoo.

## Diagnostics and metrics

The main evaluation helpers are:

- `bg_eval_top1()`
- `bg_eval_topk()`
- `bg_eval_rank()`
- `bg_eval_restricted_rank()`
- `bg_eval_allocation()`
- `bg_eval_efficiency()`
- `bg_eval_calibration()`
- `bg_eval_reference_aware()`
- `bg_ts_diagnostics()`

These answer questions like:

- Did the run select the truth-best move?
- What was the truth rank of the selected move?
- Was the selected move in the truth top 2 or top k?
- How much simple regret remained?
- How much budget was wasted on clearly inferior moves?
- How stable was the final recommendation across seeds?

## Truth certification and stability

The package tries not to overclaim Monte Carlo proxy truth.

### `bg_truth_certify()`

This summarizes whether a proxy truth looks separated enough to support
comparisons. It focuses on:

- the estimated top-two gap
- the Monte Carlo interval around that gap
- whether the interval excludes zero
- near-optimal set size
- not-separated-from-best set size
- screening labels such as `clear`, `hard`, `ambiguous`, or `uncertain`

### `bg_truth_stability()`

This asks a different question:

- if the truth build is repeated over budgets and seeds, does the reference
  stabilize?

That matters because a method comparison is only as trustworthy as the truth
object it is being scored against.

## Analysis entrypoints

The repository's main human-facing path is `analysis/`.

### Main walkthrough

- [`analysis/00_walkthrough_and_results.R`](analysis/00_walkthrough_and_results.R)

This file is intentionally long and heavily commented. It is the best place to
start if you want:

- a tour of the package
- actual results
- saved tables and plots
- use of the preserved opening cache

### Focused analysis scripts

- [`analysis/01_opening_truth_overview.R`](analysis/01_opening_truth_overview.R)
- [`analysis/02_ts_vs_ttts_opening_study.R`](analysis/02_ts_vs_ttts_opening_study.R)
- [`analysis/03_build_reward_truth_caches.R`](analysis/03_build_reward_truth_caches.R)

All generated outputs are written under:

- `analysis/output/`

## Cache directories

### Preserved repo cache

- [`cache/opening_truths_restart/`](cache/opening_truths_restart)

This directory contains the preserved 21-opening proxy truths at budget
`1,000,000` and should be kept intact.

### New master-truth workflow

Typical new cache directories are:

- `cache/opening_truths_master/`
- `cache/opening_truths_scalar_payoff/`
- `cache/opening_truths_win_loss/`
- `cache/opening_truths_categorical/`

The package's master-truth workflow is designed so that:

1. one expensive master simulation is run once
2. later reward-system-specific truth objects are projected from it

### Loading and projection example

```r
library(backgammonr)

master_truth <- bg_opening_master_truth_build_one(
  roll = "1-6",
  budget = 1000000L,
  n_cores = 4L,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  seed = 1L
)

truth_scalar <- bg_truth_project(
  master_truth,
  reward_model = "scalar_payoff",
  posterior_model = "student_t_marginal",
  unresolved_value = 0.5
)
```

## Performance notes

Large truth builds are slow because the work is genuinely large.

The truth builder does call compiled C++ rollout code. The runtime is not
mostly R overhead.

When a truth build is slow, the main reasons are:

- large rollout budget
- many candidate states
- long continuation games
- large `max_rollout_turns`
- equal-allocation reference mode spending budget across every candidate

### What `truth_block_size` means

`truth_block_size` is a chunk size for the rollout-block engine.

It is:

- a computational tuning parameter

It is not:

- a scientific tuning parameter

Changing it may affect overhead modestly, but it does not change the estimand.

### Practical runtime guidance

If you want to stay productive:

1. build all 21 openings at `250000` or `500000` first
2. inspect certification
3. extend only the hard openings to `1000000` or `2000000`

Truth builds extend cached objects cleanly when the problem identity matches and
the new requested budget is larger.

## Repository map

The easiest reading order is:

1. [`analysis/00_walkthrough_and_results.R`](analysis/00_walkthrough_and_results.R)
2. [`R/bg_problem.R`](R/bg_problem.R)
3. [`R/bg_truth.R`](R/bg_truth.R)
4. [`R/bg_algorithms.R`](R/bg_algorithms.R)
5. [`R/bg_metrics.R`](R/bg_metrics.R)
6. [`src/truth_proxy.cpp`](src/truth_proxy.cpp)
7. [`src/alloc_core.cpp`](src/alloc_core.cpp)
8. [`src/policy_ts.cpp`](src/policy_ts.cpp)
9. [`src/policy_ttts.cpp`](src/policy_ttts.cpp)

If you want the engine only, start with:

1. [`R/bg_engine_api.R`](R/bg_engine_api.R)
2. [`src/bg_board.cpp`](src/bg_board.cpp)
3. [`src/bg_movegen.cpp`](src/bg_movegen.cpp)
4. [`src/bg_rules.cpp`](src/bg_rules.cpp)
5. [`src/bg_game.cpp`](src/bg_game.cpp)
6. [`src/engine_playout.cpp`](src/engine_playout.cpp)

## Reproducibility

For reproducible work:

- set explicit `seed` values
- save truth objects to cache
- keep reward system and unresolved handling explicit
- keep rollout continuation policy explicit
- record the truth cache directory used by each study

The package supports reproducible saved objects through:

- `bg_truth_save()`
- `bg_truth_load()`
- `bg_study_save()`
- `bg_study_load()`

## Minimal example

This is the shortest research-facing example that reflects the cleaned package
design.

```r
library(backgammonr)

master_truth <- bg_opening_master_truth_build_one(
  roll = "1-6",
  budget = 1000000L,
  n_cores = 4L,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  seed = 1L
)

truth_scalar <- bg_truth_project(
  master_truth,
  reward_model = "scalar_payoff",
  posterior_model = "student_t_marginal",
  unresolved_value = 0.5
)

fit_ts <- bg_ts_run(
  problem = truth_scalar$problem,
  budget = 128L,
  checkpoints = c(32L, 64L, 128L),
  proxy_reference = truth_scalar$reference,
  seed = 1L
)

diag_ts <- bg_ts_diagnostics(fit_ts, truth = truth_scalar)
diag_ts$accuracy
diag_ts$allocation
```

That is the package in one workflow:

- simulate one high-budget truth
- project it into the reward system you want
- run a lower-budget Thompson method
- score it against the fixed truth
