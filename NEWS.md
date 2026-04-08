# backgammonr NEWS

## 0.3.0

### Package cleanup and workflow rebuild

- Removed the old mixed vignette stack, generated vignette outputs, scratch
  scripts, and redundant narrative docs.
- Rebuilt the vignette system from scratch as a sequential `00`--`15`
  workflow centered on cached truth objects and later analysis/loading steps.
- Added cleanup and scientific reframing docs:
  - `docs/CLEANUP_MANIFEST.md`
  - `docs/RESEARCH_REFRAMING_FIRST_PASS.md`
- Added an `artifacts/` convention for saved truth objects, study objects, and
  presentation-ready outputs without polluting the package source tree.

### TS-first redesign

- Repositioned `backgammonr` as a Thompson-sampling toolkit for fixed-budget
  best-action identification under Monte Carlo noise.
- Added a new TS-first public workflow:
  - `bg_problem()`
  - `bg_ts_decide()`
  - `bg_reference()`
  - `bg_ts_profile()`
  - `bg_compare_methods()`
  - `bg_opening_study()`
  - `bg_game_trace()`
  - `bg_board_features()`
  - `bg_structure_study()`
- Added research-facing wrappers and utilities for the sequential workflow:
  - `bg_ts_run()`
  - `bg_ts_posterior_summary()`
  - `bg_study_save()`
  - `bg_study_load()`
  - `bg_move_features()`
- Added consistent TS-first object classes with `print()`, `summary()`,
  `plot()`, `autoplot()`, and `as_tibble()` methods.

### Proxy-reference engine

- Added a parallel rollout-block backend in `src/bg_parallel.cpp` using
  `RcppParallel` and TBB.
- Added deterministic proxy-reference aggregation so worker-count changes do
  not change the resulting estimates when the seed is fixed.
- Added focused proxy-reference mode as an explicit approximation on top of the
  equal-reference baseline.
- Kept sequential Thompson sampling as the canonical semantics while exposing
  batched Thompson as an experimental acceleration mode.

### Studies and visuals

- Added TS-budget profiles, method-comparison studies, opening-roll atlas
  workflows, move-by-move game-trace studies, and experimental structure
  studies.
- Added reference-aware evaluation and cleaner TS diagnostics for ranking,
  allocation, seed variability, and gap-aware correctness:
  - `bg_eval_reference_aware()`
  - `plot_bg_gap()`
  - `plot_bg_allocation()`
  - `plot_bg_runtime_scaling()`
- Added budget-path, allocation-flow, probability-best, seed-heatmap,
  opening-atlas, game-trace, and structure-map plotting helpers.
- Tightened user-facing language around:
  - rollout-model values;
  - proxy references;
  - unavailable game-theoretic truth.

### Documentation and site

- Rewrote `README.md` around the TS-first workflow and added an opening-study
  figure.
- Updated the package title and description in `DESCRIPTION`.
- Added `_pkgdown.yml` with a TS-first homepage and reference sections.
- Added new vignettes for:
  - getting started with Thompson sampling;
  - TS budget profiling;
  - TS vs TTTS vs baselines;
  - opening-roll atlas;
  - move-by-move game traces;
  - proxy-reference uncertainty;
  - experimental structured studies.

### Compatibility

- Kept the legacy evaluation and comparison helpers available so existing
  workflows, including the locked easy-function-call vignette, continue to run.
- Preserved the distinction between exact-semantics speedups and optional
  advanced modes such as batched TS, CRN, and stratified dice.
