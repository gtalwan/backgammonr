# backgammonr NEWS

## 0.2.1

### Package framing and metadata

- Reorganized the package-facing documentation around the actual research
  problem: finite-budget rollout allocation with Thompson sampling as the
  conceptual center.
- Replaced placeholder package metadata in `DESCRIPTION` with repository-aligned
  maintainer and author information.
- Added repository metadata fields for GitHub URL and issue tracking.
- Kept the package under the standard R-compatible MIT declaration
  (`MIT + file LICENSE`) and added a human-readable MIT license text for the
  repository.

### Core action-evaluation workflow

- Thompson sampling and Top-Two Thompson remain the primary finite-budget
  methods:
  - `evaluate_actions_thompson()`
  - `evaluate_actions_ttts()`
  - `trace_thompson_allocation()`
- Baseline comparison methods remain available for controlled experiments:
  - `evaluate_actions_equal()`
  - `evaluate_actions_greedy()`
  - `evaluate_actions_ucb()`
  - `evaluate_actions_ocba()`
- Runtime fields are propagated through evaluation outputs so higher-level
  studies and benchmarks can summarize speed/accuracy tradeoffs without wrapping
  every call in extra timing code.

### Reference estimation and method comparison

- The package keeps a dedicated reference-estimation workflow for proxy truth:
  - `approximate_action_reference()`
  - `approximate_action_truth()`
  - `certify_reference_truth()`
- Thompson/reference comparison helpers remain first-class:
  - `compare_thompson_to_reference()`
  - `compare_methods_on_position()`
- The central comparison metrics remain:
  - proxy probability of correct selection;
  - simple regret;
  - action-value MSE;
  - runtime.

### Research helpers and repeated studies

- Budget and variance-study helpers remain part of the core statistical API:
  - `study_budget_tradeoff()`
  - `study_variance_controls()`
- Benchmarking helpers support repeated multi-case evaluation:
  - `benchmark_allocation_methods()`
  - `benchmark_thompson()`
  - `summarize_thompson_benchmark()`
- Benchmark configuration continues to support crossed settings over methods,
  budgets, dice-mode choices, and common-random-number controls.

### Visualization and reporting

- Plotting and reporting remain organized around interpretable statistical
  outputs rather than raw engine internals.
- The package keeps convenience helpers for allocation traces, convergence
  checks, benchmark summaries, and narrative reports:
  - `plot_thompson_convergence()`
  - `plot_thompson_vs_baselines()`
  - `bg_plot_benchmark_summary()`
  - `bg_analysis_report()`

### Documentation refresh

- Rewrote `README.md` to emphasize the main statistical workflow and the most
  important exported functions.
- Reworked `DEVELOPMENT.md` into a project maintenance guide with architecture,
  invariants, validation rules, and next priorities.
- Reorganized `NEWS.md` into release-oriented sections instead of an ungrouped
  feature dump.
- Kept the vignette stack aligned with the Thompson-centered package framing.
