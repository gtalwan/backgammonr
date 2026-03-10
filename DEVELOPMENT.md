# Development Guide

## Project Scope

`backgammonr` is a statistical computing package built around one core problem:
finite-budget best-action identification under simulation noise. The backgammon
engine exists to supply a realistic stochastic decision environment; the main
research contribution is the allocation and evaluation workflow.

Development decisions should preserve that framing.

- Backgammon mechanics are infrastructure.
- Finite-budget rollout allocation is the central contribution.
- Thompson sampling and Top-Two Thompson are the conceptual center.
- Equal, greedy, UCB, and OCBA remain important baselines.
- User-facing outputs should stay rectangular, interpretable, and reproducible.

## Architecture Overview

The package is split into four layers.

### 1. Game-state and engine infrastructure

These files define board objects, dice rolls, legal move generation, state
transitions, and full-game simulation support.

- `R/backgammon_core.R`
- `R/board_wrappers.R`
- `src/bg_board.cpp`
- `src/bg_moves.cpp`
- `src/bg_rules.cpp`
- `src/bg_game.cpp`
- `src/bg_simulation.cpp`

### 2. Allocation and action-evaluation kernels

These are the core research files. Most performance-sensitive logic lives here.

- `src/bg_allocation.cpp`
- `src/bg_rollout.cpp`
- `src/bg_thompson_rollout.cpp`
- `R/allocation_methods.R`
- `R/statistical_api.R`

### 3. Research and experiment helpers

These layers organize repeated studies, reference comparisons, and benchmarks.

- `R/thompson_research.R`
- `R/statistical_studies.R`
- `src/bg_benchmark.cpp`

### 4. Visualization, explanation, and reporting

These files turn raw evaluation output into tables, plots, and narrative
summaries.

- `R/visualization.R`
- `R/analysis_reporting.R`

## Primary Source Files To Read First

If you are new to the codebase, start with these files in order.

1. `README.md`
2. `R/backgammonr-package.R`
3. `R/allocation_methods.R`
4. `R/thompson_research.R`
5. `R/statistical_studies.R`
6. `src/bg_allocation.cpp`
7. `src/bg_rollout.cpp`
8. `src/bg_thompson_rollout.cpp`
9. `src/bg_simulation.cpp`
10. `src/bg_benchmark.cpp`

This sequence moves from package framing to public API, then into the
performance-critical C++ kernels.

## Statistical Workflow Contract

The central workflow should remain stable:

1. build a board and realized roll;
2. enumerate legal actions;
3. evaluate actions under a finite simulation budget;
4. build a higher-budget reference estimate;
5. compare the finite-budget recommendation to the reference;
6. aggregate results across budgets, cases, and variance settings.

The default user flow in the vignettes is organized around these steps. New code
should fit naturally into this pipeline rather than creating parallel interfaces
without a strong reason.

## Core Statistical Objects

Most statistical functions return one of a small number of structured objects.

- `bg_action_evaluation`: candidate-level allocation, estimate, uncertainty,
  and recommendation output for one position.
- `bg_reference_certificate`: diagnostic summary describing how trustworthy a
  high-budget reference estimate appears to be.
- `bg_thompson_reference_comparison`: finite-budget versus reference summary and
  action-by-action comparison table.
- `bg_budget_tradeoff`: repeated comparisons over budgets.
- `bg_variance_control_study`: repeated comparisons over variance-control
  settings such as dice mode and common random numbers.
- `bg_thompson_benchmark` and `bg_thompson_benchmark_summary`: multi-case
  benchmark outputs.

Whenever possible, new helpers should reuse these structures or extend them in a
backward-compatible way.

## Performance Rules

Performance work should focus on the actual hotspots rather than cosmetic
wrapper changes.

- Keep legal-move generation and rollout loops in C++.
- Avoid repeated R-to-C++ conversions inside repeated-study wrappers.
- Reuse precomputed action tables and reference summaries where possible.
- Do not add duplicate simulations just to reshape outputs.
- Keep runtime fields attached to results so higher-level studies can avoid
  re-timing already measured calls.
- Preserve deterministic behavior under fixed seeds and fixed settings.

If a proposed optimization changes statistical semantics, it needs to be
justified explicitly.

## Output Design Rules

Default outputs should stay concise but decision-relevant. The most important
columns across the package are:

- action label;
- allocation count;
- estimate;
- uncertainty;
- probability best;
- regret-like quantity;
- recommendation flag;
- runtime.

Advanced diagnostics are valuable, but they should remain optional rather than
overwhelming the default print path.

## Documentation Workflow

The documentation stack should stay layered and consistent.

- `README.md` should explain the package framing and the main statistical
  workflow quickly.
- `NEWS.md` should summarize meaningful package changes by release.
- `DEVELOPMENT.md` should explain architecture, invariants, and maintenance
  practices.
- `vignettes/05_thompson_workflow.Rmd`,
  `vignettes/06_main_question_example.Rmd`, and
  `vignettes/07_easy_function_calls.Rmd` should remain aligned with the exported
  API.
- `docs/` files can carry deeper implementation and methodological detail.

If user-facing behavior changes, update the README and the relevant vignette in
the same pass unless the vignette is intentionally locked.

## Validation Checklist

Before merging substantive changes, run at least the following:

1. install from source with `R CMD INSTALL`;
2. smoke-test the central exported statistical functions;
3. run the easiest end-to-end vignette workflow;
4. verify that public APIs and key printed outputs remain coherent;
5. remove local build artifacts such as `src/*.o` and shared objects.

For changes touching allocation logic, also compare runtimes and confirm that
the change does not silently add extra simulation work.

## Package Metadata And Release Hygiene

The package should keep:

- a real maintainer and author entry in `DESCRIPTION`;
- a standard R-compatible MIT license declaration via
  `License: MIT + file LICENSE`;
- a human-readable repository license file for GitHub readers;
- version and `NEWS.md` entries that move together.

Placeholder authorship or generic machine-generated metadata should not remain
in release-facing files.

## Known Limitations

- Best-action claims are always rollout-model-relative.
- A high-budget reference is still proxy truth, not exact truth.
- Near-tie positions can remain unstable even under large budgets.
- Statistical trust is bounded by the completeness of the game engine and
  rollout policy.
- Some visualization/reporting helpers assume rectangular outputs from the core
  evaluation functions and may need adjustment if schemas evolve.

## Near-Term Priorities

1. Extend regression coverage for the Thompson research helpers and study
   wrappers.
2. Add stronger curated benchmark sets with difficulty labels and expected
   behaviors.
3. Profile rollout and move-generation kernels with larger benchmark batches.
4. Expand documentation around reference-certification limits and interpretation.
