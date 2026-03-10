# backgammonr Statistical Workflow: Investigating Thompson Sampling

This workflow is designed as an investigation, not a sequence of random tables.

At each stage we ask a clear question:

1. What is the concrete decision instance?
2. How does Thompson spend finite budget on that instance?
3. Does the finite-budget recommendation agree with a high-budget proxy?
4. How does behavior change with budget?
5. How does Thompson compare with baselines?
6. What counts as encouraging vs cautionary evidence?

Every section below is self-contained.

---

## 1) Setup (Self-Contained)

```r
library(backgammonr)
```

---

## 2) Define One Decision Problem

Question: **What exact board/roll/action set are we evaluating?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

bg_print_board(board)
print(roll)

legal <- generate_legal_moves(board, roll)
move_table <- summarize_legal_moves(legal, max_candidates = 12L)
move_table
```

What to look for:

- `candidate_index` and `move_label` define the action set.
- Large action sets imply stronger finite-budget pressure.
- This table is the action universe for all later comparisons.

---

## 3) Run Finite-Budget Thompson Evaluation

Question: **What does Thompson recommend at budget `N`, and why?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

th <- evaluate_actions_thompson(
  board = board,
  roll = roll,
  total_budget = 1000L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = FALSE,
  seed = 1L
)

print(th)
```

How to interpret the default table:

- `action`: legal move label.
- `alloc_n`: simulations allocated to that action.
- `estimate`: posterior mean value.
- `uncertainty_sd` and `ci95_*`: uncertainty.
- `prob_best`: posterior probability action is best.
- `exp_regret`: posterior expected simple regret if selected.
- `recommended`: finite-budget recommendation.

Encouraging pattern:

- high `prob_best`, low `exp_regret`, and clear budget concentration on top plausible moves.

Cautionary pattern:

- diffuse allocation plus similar estimates/uncertainty across top actions (hard state or insufficient budget).

---

## 4) Compare to High-Budget Reference (Proxy Truth)

Question: **Does finite-budget Thompson recover the high-budget reference-best action?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

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

print(cmp)
```

Core metrics:

- `proxy_pcs`: 1 if finite choice matches reference-best, else 0.
- `simple_regret`: reference value gap between best and chosen.
- `mse`: full action-value vector error vs reference.
- `reference_certified` + `difficulty_label`: how clearly the reference separates top two actions.

Important limitation:

- Reference is still approximate simulation output, not exact truth.

---

## 5) Inspect Allocation Dynamics Over Time

Question: **How does Thompson’s allocation evolve during the run?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

tr <- trace_thompson_allocation(
  board = board,
  roll = roll,
  method = "thompson",
  total_budget = 1000L,
  trace_every = 50L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

print(tr)
```

How to interpret:

- `selected_move_label`: action sampled at each checkpoint.
- `leader_move_label`: highest-estimate action at checkpoint.
- `leader_allocation_count`: how quickly budget concentrates.

Encouraging:

- leader stabilizes and receives increasing share of budget.

Caution:

- frequent late-stage leader flips (possible hard state or under-budgeting).

---

## 6) Budget Sensitivity on One Position

Question: **How quickly does Thompson’s recommendation quality improve with budget?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

tradeoff <- study_budget_tradeoff(
  board = board,
  roll = roll,
  method = "thompson",
  budgets = c(128L, 512L, 2048L, 4096L),
  truth_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = FALSE,
  seed = 1L
)

print(tradeoff)
```

What to check:

- `correct_selection` trend vs budget.
- `simple_regret` and `mse` decay vs budget.
- runtime growth.

Interpretation caution:

- finite-budget curves can be non-monotone because rollouts are noisy.

---

## 7) Same Budget, Different Allocation Rules

Question: **At fixed budget, does Thompson beat baselines on this position?**

```r
library(backgammonr)

board <- bg_initial_board()
roll <- bg_roll(1L, 6L)

cmp_methods <- compare_methods_on_position(
  board = board,
  roll = roll,
  methods = c("equal", "ucb", "ocba", "thompson", "ttts"),
  total_budget = 1024L,
  rollout_policy = "random",
  max_rollout_turns = 300L,
  fast_diagnostics = FALSE,
  seed = 1L
)

print(cmp_methods)
```

Interpretation:

- Compare `recommended_estimate`, `recommended_prob_best`, `recommended_expected_regret`, and `runtime_seconds`.
- Do not treat estimate alone as decisive when uncertainty is high.

---

## 8) Multi-Case Benchmark: Thompson vs Baselines

Question: **Are Thompson gains consistent across cases and budgets?**

```r
library(backgammonr)

case1 <- bg_benchmark_case(bg_initial_board(), bg_roll(1L, 6L), case_id = "init_1_6")
case2 <- bg_benchmark_case(bg_initial_board(), bg_roll(3L, 2L), case_id = "init_3_2")

bm <- benchmark_thompson(
  cases = list(case1, case2),
  budgets = c(256L, 1024L, 4096L),
  baselines = c("equal", "ucb", "ocba", "greedy"),
  include_ttts = TRUE,
  reference_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

print(bm)
focus <- summarize_thompson_benchmark(bm)
print(focus)
```

What this stage answers:

- Does Thompson recover reference-best actions more often (`PCS`)?
- Does it reduce simple regret faster?
- Is any improvement consistent or state-dependent?
- What runtime premium does it pay?

---

## 9) Easy vs Hard Case Interpretation

Question: **Does Thompson behavior depend on top-two reference gap difficulty?**

```r
library(backgammonr)

case1 <- bg_benchmark_case(bg_initial_board(), bg_roll(1L, 6L), case_id = "init_1_6")
case2 <- bg_benchmark_case(bg_initial_board(), bg_roll(3L, 2L), case_id = "init_3_2")

bm <- benchmark_thompson(
  cases = list(case1, case2),
  budgets = c(256L, 1024L),
  baselines = c("equal", "ucb"),
  include_ttts = TRUE,
  reference_budget = 30000L,  # quick option: 4000L
  rollout_policy = "random",
  max_rollout_turns = 300L,
  seed = 1L
)

bm$truth[, c("case_id", "difficulty_gap", "difficulty_label")]
summarize_thompson_benchmark(bm)$by_difficulty
```

Interpretation expectation:

- Easy cases (larger gaps) should generally stabilize faster.
- Hard cases can remain ambiguous at moderate budgets even with adaptive allocation.

---

## 10) Evaluation Checklist (Use This When Reading Results)

When studying Thompson sampling, ask all of these:

1. **Proxy best-action recovery:** Does finite recommendation match reference-best action (PCS)?
2. **Regret:** How large is simple regret when it misses?
3. **Estimation quality:** Does MSE shrink as budget grows?
4. **Allocation behavior:** Is budget concentrated on plausible high-value actions?
5. **Uncertainty:** Are top actions clearly separated or still overlapping?
6. **Stability:** Do recommendations/leader actions stabilize with budget?
7. **Runtime:** Is quality gain worth computational cost?
8. **Difficulty sensitivity:** Are gains robust across easy and hard states?

---

## 11) Honest Interpretation Template

Report both positives and negatives.

Possible positives:

- Thompson concentrates budget on plausible best actions.
- Thompson reaches high proxy PCS at relatively small budgets.
- Thompson reduces simple regret faster than equal allocation.
- Posterior uncertainty shrinks in a coherent way.

Possible cautions:

- Very small budgets can be unstable.
- Early noisy wins can mislead allocation.
- Posterior diagnostics depend on rollout-model assumptions.
- Hard states with tiny top-two gaps may remain unresolved at moderate budgets.
- Reference estimates are high-budget proxies, not exact truth.
