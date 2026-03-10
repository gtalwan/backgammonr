# backgammonr Function Reference (Thompson Investigation Focus)

This reference explains what each core function is for, what question it answers, and how to interpret output.

## 1) Core Single-Instance Evaluators

### `evaluate_actions_thompson(...)`

Problem solved:

- Allocate a finite rollout budget adaptively across legal actions using Thompson sampling.

Statistical question:

- With budget `N`, which action is best under the rollout model, and how confident are we?

Read output as:

- `alloc_n` / `allocation_count`: budget spent per action.
- `estimate`: posterior mean action value.
- `uncertainty_sd` and `ci95_*`: uncertainty.
- `prob_best`: posterior chance this action is best.
- `exp_regret`: posterior expected simple regret if selected.
- `recommended`: finite-budget recommendation.

Limitations:

- small budgets can be unstable;
- posterior diagnostics depend on model assumptions.

### `evaluate_actions_ttts(...)`

Problem solved:

- Top-Two Thompson variant for harder best-action identification settings.

Statistical question:

- Does emphasizing top-two discrimination improve finite-budget decisions?

Key parameter:

- `ttts_beta`: probability of sampling the current posterior best vs a sampled challenger.

### `evaluate_actions_equal(...)`, `evaluate_actions_ucb(...)`, `evaluate_actions_ocba(...)`, `evaluate_actions_greedy(...)`

Use these as baselines for Thompson comparisons under identical budget/settings.

## 2) Reference Layer (Proxy Truth)

### `approximate_action_reference(...)` / `approximate_action_truth(...)`

Problem solved:

- Build a high-budget proxy truth for one decision instance.

Statistical question:

- What should finite-budget outputs be compared against?

Important caveat:

- proxy truth is high-budget simulation, not exact game-theoretic truth.

### `certify_reference_truth(...)`

Problem solved:

- Quantify top-two separation reliability for the reference estimate.

Read output as:

- `top_two_gap_estimate`, uncertainty interval, `certified`, `difficulty_label`.

### `compare_thompson_to_reference(...)`

Problem solved:

- Evaluate one finite Thompson/TTTS run against high-budget reference.

Important usage note:

- If you already computed `ref <- approximate_action_reference(...)`, pass
  `reference = ref` (and optionally `reference_certificate = certify_reference_truth(reference = ref)`).
- This avoids rerunning expensive reference simulations and keeps one consistent
  proxy-truth object for interpretation.

Core outputs:

- `proxy_pcs`, `simple_regret`, `mse`, runtime;
- action-wise finite vs reference error table.

## 3) Allocation Behavior and Stability

### `trace_thompson_allocation(...)`

Problem solved:

- Inspect checkpoint-level allocation dynamics.

Statistical questions:

- Does allocation concentrate on plausible best actions?
- Does the leader stabilize with increasing budget?

### `plot_thompson_convergence(...)`

Use for visual diagnostics of estimate/allocation trajectories over checkpoints.

## 4) Single-Position Study Wrappers

### `compare_methods_on_position(...)`

Question answered:

- At fixed budget, which method gives stronger recommendation quality/runtime on this position?

### `study_budget_tradeoff(...)`

Question answered:

- How do PCS, simple regret, MSE, and runtime change as budget increases?

### `study_variance_controls(...)`

Question answered:

- How sensitive are results to `dice_mode` and CRN settings?

## 5) Multi-Case Benchmarking

### `benchmark_thompson(...)`

Problem solved:

- Benchmark Thompson against baselines across cases/budgets.

### `summarize_thompson_benchmark(...)`

Problem solved:

- Produce Thompson-centered tables for PCS, regret, MSE, runtime, and difficulty strata.

### `benchmark_allocation_methods(...)`

General crossed benchmark engine used by the Thompson wrapper.

## 6) Interpretation Metrics

- `compute_probability_of_correct_selection(...)` (proxy PCS)
- `compute_simple_regret(...)`
- `compute_mse(...)` / `compute_value_mse(...)`

Use all metrics together. A method can improve PCS yet pay runtime cost, or reduce regret while leaving MSE high.

## 7) Recommended Investigation Order

1. Define one `(board, roll)` instance and legal actions.
2. Run finite Thompson and inspect compact action table.
3. Compare to high-budget reference (`proxy_pcs`, regret, MSE).
4. Inspect trace behavior for allocation stability.
5. Run budget sensitivity and baseline comparisons.
6. Benchmark across easy/hard cases before broad conclusions.
