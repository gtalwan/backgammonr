# backgammonr Implementation Deep Dive

This document explains how the package works internally, why the current implementation is fast for random-policy rollouts, and where further improvements are still possible.

It is intentionally code-oriented and algorithmic.

## 1) Design Goal

`backgammonr` treats backgammon move choice as a fixed-budget simulation problem:

- candidate actions are legal move sequences for one `(board, roll)` pair,
- each action value is estimated by rollouts,
- budget allocation method decides where to spend simulation effort.

The implementation target is **high-throughput random-policy simulation** suitable for statistical computing experiments.

## 2) Code Map (What Lives Where)

- `R/`: user-facing APIs, input validation, workflow wrappers, reporting, plotting.
- `src/bg_rules.cpp`: legality of one-step moves and fast rule primitives.
- `src/bg_movegen.cpp`: full legal move generation + fast random sequence sampling.
- `src/bg_allocation.cpp`: budget allocation engine (equal/greedy/UCB/OCBA/Thompson) + rollout loop.
- `src/bg_simulation.cpp`: full-game/matchup simulation loops.
- `src/bg_game.cpp`: turn/game orchestration and policy dispatch.

The fast path is in C++; R code should mostly orchestrate and summarize.

## 3) Core Data Structures

### `BoardState` (`src/bg_board.h`)

```cpp
struct BoardState {
  std::array<int, 24> points;
  std::array<int, 2> bar;
  std::array<int, 2> off;
  int turn;
};
```

Why this matters:

- fixed-size arrays avoid per-turn heap allocation,
- contiguous memory is cache-friendly,
- copying is predictable and relatively cheap when needed.

### `MoveStep` and `MoveSequence`

- `MoveStep`: one checker movement with `{from, to, die, hit}`.
- `MoveSequence`: full legal action for the turn.

## 4) Move Generation Algorithm

Main entry: `generate_legal_move_sequences(...)` in `src/bg_movegen.cpp`.

### 4.1 DFS over dice usage

- Expand roll to 2 dice (normal) or 4 dice (double).
- Recursively enumerate legal step continuations.
- Track used dice slots with a bit mask.

Pseudo-flow:

1. For each available die value:
2. Generate legal one-step moves into a fixed stack array (`std::array<MoveStep,24>`).
3. For each step:
4. Apply step **in place**.
5. Recurse.
6. Undo step.

### 4.2 Forced-use legality filtering

After DFS, enforce backgammon rules:

- keep only max-step sequences,
- if non-double and only one die playable, require higher die when possible,
- deduplicate equivalent sequences.

Dedup uses a compact fixed-field signature + `unordered_set`.

## 5) Fast Random-Policy Turn (Key Optimization)

Function: `play_random_turn_rollout_fast(...)` in `src/bg_movegen.cpp`.

Instead of building the full legal sequence vector every rollout turn:

- run DFS that **streams** legal leaves,
- maintain a reservoir sampler to pick one sequence uniformly at random,
- enforce forced-use legality logic during selection,
- apply selected sequence directly.

This keeps random policy fully random over legal sequences while avoiding large temporary containers in rollout hot loops.

## 6) Rollout/Allocation Engine

Main entry: `evaluate_move_sequences_with_allocation(...)` in `src/bg_allocation.cpp`.

### 6.1 Candidate collapse

Before simulation, legal move sequences that produce identical post-move boards are collapsed:

- hash board-after state,
- keep one representative index,
- store multiplicity as `n_equivalent_sequences`.

This reduces duplicated simulation work while preserving mapping back to legal candidates.

### 6.2 Allocation loop

For each rollout unit:

1. Choose candidate index by method:
- equal: round-robin,
- greedy: highest posterior mean,
- UCB: mean + exploration bonus,
- Thompson: sampled Beta draw,
- OCBA: deficit vs target allocation.

2. Simulate one rollout from candidate board-after.
3. Update counts and Beta posterior parameters.

### 6.3 Random-policy rollout path

For `rollout_policy = "random"`:

- no expensive policy logic,
- turn loop uses `play_random_turn_rollout_fast`,
- minimal per-turn object work.

### 6.4 Optional diagnostics

Posterior diagnostics (`prob_best`, `posterior_expected_regret`) are Monte Carlo approximations. They are useful but not free.

- `fast_diagnostics = TRUE` skips that expensive section.
- ranking estimates still remain available.

## 7) Simulation Engine

`src/bg_simulation.cpp` provides full-game/matchup simulation.

Important fast-path behavior:

- when player selection is `"random"`, the loop uses the lightweight fast-turn helper,
- avoids constructing heavy per-turn objects in the hottest path,
- batch simulation (`n_games`) happens in one C++ call.

## 8) Why This Is Faster Than Naive C++

Speed does not come from “being in C++” alone. It comes from specific choices:

1. Apply/undo recursion rather than cloning board each branch.
2. Fixed-size temporary arrays in hot loops.
3. One-pass random legal-sequence sampling in rollouts.
4. Candidate-state collapsing before allocation loop.
5. Batched rollouts/games in a single C++ call.
6. Optional expensive diagnostics switch.
7. Limited R object creation inside simulation loops.

## 9) Main Exported Functions: Algorithmic Logic

### `generate_legal_moves(state, dice)`

- Calls C++ move generator.
- Returns all legal turn sequences after forced-use + dedup filtering.

### `evaluate_actions_equal/ucb/ocba/thompson/...`

- Normalize input and legal moves.
- Run common C++ allocation engine with selected method.
- Convert summaries to ranked action table.
- Attach move labels and recommended action.

### `approximate_action_truth(...)`

- Calls equal-allocation evaluation with large budget.
- Used as proxy truth for regret/PCS/MSE studies.

### `benchmark_allocation_methods(...)`

- For each case:
1. Build high-budget truth.
2. For each method × budget × variance-control config, run evaluation.
3. Compute decision metrics (`correct_selection`, regret, MSE, runtime).
4. Produce case-level and grouped summary tables.

### `bg_profile_runtime(...)`

- Times isolated components:
- legal move generation,
- move application,
- one rollout,
- batched rollout evaluation.

This identifies where time is spent on the current machine.

### New high-level study wrappers

- `compare_methods_on_position(...)`: same budget, different methods.
- `study_budget_tradeoff(...)`: one method across budgets vs reference truth.
- `study_variance_controls(...)`: one method/budget across `dice_mode` and `crn` settings.

These wrappers expose the same core question through different experimental views.

## 10) Correctness Guardrails

- Board parsing/validation checks integrity (checker totals, field types, turn values).
- Move application for external APIs validates against generated legal moves.
- Forced-use rules enforced for generated legal actions.
- Candidate index mapping preserved even after state collapsing.

## 11) Known Tradeoffs

- Random-policy engine optimized for throughput, not playing strength.
- Posterior intervals are normal-approx around Beta mean, not exact quantiles.
- Approximate truth is still simulation-based, not exact game-theoretic truth.

## 12) Improvement Roadmap (Next)

These are practical next optimizations that preserve package scope:

1. **Zero-copy candidate board caching across repeated calls**
- Cache board-after states keyed by `(board, roll)` hash for repeated benchmarking loops.

2. **Cheaper posterior diagnostics option**
- Add `diagnostic_draws` parameter to tune cost/precision of `prob_best` and expected regret.

3. **Tighter OCBA implementation variant**
- Add a more explicit finite-budget OCBA heuristic variant for side-by-side comparison.

4. **Profiling hooks per internal phase**
- Optional counters for rollout phases (selection, simulation, update) in one returned profile object.

5. **Optional parallel backend (default off)**
- Keep single-core fast path as default,
- allow opt-in parallel budget splitting with deterministic seed partitioning.

6. **Rollout-specific compressed state experiment**
- Investigate a narrower rollout-only state struct for further cache gains,
- keep public API unchanged.

## 13) Practical Recommendation

For statistical studies where throughput matters most:

- use `rollout_policy = "random"`,
- start with `fast_diagnostics = TRUE`,
- use high-budget truth only for selected benchmark comparisons,
- use study wrappers for clean, repeatable experiment tables.
