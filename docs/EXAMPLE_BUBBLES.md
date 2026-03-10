# Thompson-Centered Example Bubbles (Standalone, Copy-Paste Ready)

Each bubble is fully standalone and answers one statistical question.

---

## Bubble 1: Define the Decision Instance

Question: What `(state, realized_roll)` are we studying?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

bg_print_board(board)
roll
```

---

## Bubble 2: Enumerate Candidate Actions

Question: What is the legal action set `A_d`?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

legal <- generate_legal_moves(board, roll)
length(legal)
legal[[1]]
```

---

## Bubble 3: Thompson on One Decision

Question: With budget `N`, what does Thompson recommend?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

th <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = 1000L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = FALSE,
  seed = 1L
)

th$results[order(th$results$rank), c(
  "rank", "move_label", "allocation_count", "estimate",
  "prob_best", "posterior_expected_regret", "recommended"
)]
```

---

## Bubble 4: Top-Two Thompson (TTTS)

Question: Does TTTS allocate differently under the same budget?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

ttts <- evaluate_actions_ttts(
  board = board,
  roll = roll,
  total_budget = 1000L,
  ttts_beta = 0.5,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = FALSE,
  seed = 1L
)

ttts$results[order(ttts$results$rank), c(
  "rank", "move_label", "allocation_count", "estimate",
  "prob_best", "posterior_expected_regret", "recommended"
)]
```

---

## Bubble 5: Trace Thompson Allocation Over Time

Question: How does Thompson shift allocation as evidence accumulates?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

tr <- trace_thompson_allocation(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = 800L,
  trace_every = 20L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

head(tr$checkpoint_summary)
plot_thompson_convergence(tr, metric = "estimate")
```

---

## Bubble 6: Build High-Budget Reference (Proxy Truth)

Question: What high-budget reference estimate do we compare against?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

ref$results[order(ref$results$rank), c("rank", "move_label", "estimate")]
```

---

## Bubble 7: Compare Thompson to Reference

Question: How good is finite-budget Thompson vs high-budget reference?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)
cert <- certify_reference_truth(reference = ref)

cmp <- compare_thompson_to_reference(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = 1000L,
  reference = ref,
  reference_certificate = cert,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

cmp$summary[, c(
  "chosen_move_label", "reference_best_move_label",
  "proxy_pcs", "simple_regret", "mse"
)]

head(cmp$action_table)
```

---

## Bubble 8: Certify Reference Separation

Question: Is the reference best move clearly separated from second-best?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

ref <- approximate_action_reference(
  board = board,
  roll = roll,
  truth_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

cert <- certify_reference_truth(reference = ref)
cert$certificate
```

---

## Bubble 9: Benchmark Thompson vs Baselines

Question: Across budgets, how does Thompson compare to equal/UCB/OCBA/greedy?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)
case1 <- bg_benchmark_case(board, roll, case_id = "init_1_6")

bm <- benchmark_thompson(
  cases = list(case1),
  budgets = c(250L, 1000L, 4000L),
  baselines = c("equal", "ucb", "ocba", "greedy"),
  include_ttts = TRUE,
  reference_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

bm$summary[, c(
  "method", "total_budget", "correct_selection_rate",
  "mean_simple_regret", "mean_mse", "mean_runtime_seconds"
)]
```

---

## Bubble 10: Thompson-Focused Benchmark Summary

Question: What are Thompson’s gains/losses relative to each baseline?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)
case1 <- bg_benchmark_case(board, roll, case_id = "init_1_6")

bm <- benchmark_thompson(
  cases = list(case1),
  budgets = c(250L, 1000L, 4000L),
  reference_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

th_sum <- summarize_thompson_benchmark(bm)
th_sum$thompson
th_sum$relative_to_thompson
th_sum$by_difficulty
```

---

## Bubble 11: Plot Thompson vs Baselines

Question: How do quality/runtime curves compare visually?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)
case1 <- bg_benchmark_case(board, roll, case_id = "init_1_6")

bm <- benchmark_thompson(
  cases = list(case1),
  budgets = c(250L, 1000L, 4000L),
  reference_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

plot_thompson_vs_baselines(bm, metric = "correct_selection_rate")
plot_thompson_vs_baselines(bm, metric = "mean_simple_regret")
plot_thompson_vs_baselines(bm, metric = "mean_mse")
plot_thompson_vs_baselines(bm, metric = "mean_runtime_seconds")
```

---

## Bubble 12: Runtime Decomposition

Question: Where is runtime spent (legal moves, apply, rollout, batched eval)?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

profile <- bg_profile_runtime(
  board = board,
  roll = roll,
  legal_reps = 200L,
  apply_reps = 2000L,
  one_rollout_reps = 25L,
  total_budget = 4000L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

profile
```

---

## Bubble 13: Fast vs Rich Diagnostics (Speed-Accuracy Tradeoff)

Question: How much speed do we gain by skipping expensive diagnostics?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

t_fast <- system.time(
  a_fast <- evaluate_actions_thompson(
    board = board,
    roll = roll,
    total_budget = 4000L,
    rollout_policy = "random",
    max_rollout_turns = 300L,
    fast_diagnostics = TRUE,
    seed = 1L
  )
)

t_full <- system.time(
  a_full <- evaluate_actions_thompson(
    board = board,
    roll = roll,
    total_budget = 4000L,
    rollout_policy = "random",
    max_rollout_turns = 300L,
    fast_diagnostics = FALSE,
    seed = 1L
  )
)

data.frame(
  mode = c("fast_diagnostics=TRUE", "fast_diagnostics=FALSE"),
  elapsed_seconds = c(unname(t_fast["elapsed"]), unname(t_full["elapsed"]))
)
```

---

## Bubble 14: Compact Baseline Comparison on One Instance

Question: Are non-Thompson baselines materially different at this budget?

```r
library(backgammonr)

board <- bg_initial_board()
roll  <- bg_roll(1L, 6L)

cmp <- compare_methods_on_position(
  board = board,
  roll = roll,
  methods = c("thompson", "ttts", "equal", "ucb", "ocba", "greedy"),
  total_budget = 1200L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = TRUE,
  seed = 1L
)

cmp$summary[, c("method", "recommended_move_label", "estimate", "runtime_seconds")]
```

These bubbles collectively answer the package’s main research question:
finite-budget best-action identification with Thompson sampling at the center, and baseline methods as comparators.
