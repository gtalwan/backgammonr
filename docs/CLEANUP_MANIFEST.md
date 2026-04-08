# Cleanup Manifest

This file records the first-pass repo cleanup, the remaining package map, and
the keep/refactor/delete decisions behind it.

## Repo Map

### Core package code

- `R/board.R`, `R/move.R`, `R/game.R`, `R/legal_moves.R`, `R/simulation.R`
- `R/allocation_methods.R`
- `R/statistical_api.R`, `R/statistical_studies.R`, `R/thompson_research.R`
- `R/ts_frontdoor.R`, `R/ts_methods.R`, `R/ts_studies.R`
- `R/bg_model_spec.R`, `R/bg_posterior_kernels.R`,
  `R/bg_posterior_compare_workflows.R`
- `R/bg_research_frontdoor.R`, `R/bg_truth_workflows.R`,
  `R/bg_eval_workflows.R`, `R/bg_state_workflows.R`,
  `R/bg_research_plots.R`
- `R/output_tables.R`
- `src/*.cpp`, `src/*.h`

### Tests

- `tests/testthat/*.R`

### Documentation kept

- `README.md`
- `DEVELOPMENT.md`
- `docs/IMPLEMENTATION_DEEP_DIVE.md`
- `docs/RESEARCH_REFRAMING_FIRST_PASS.md`
- `docs/MODEL_AUDIT_TABLE.md`
- `docs/POSTERIOR_MODEL_FIRST_PASS.md`
- `docs/PROVISIONAL_CONCLUSIONS.md`
- `vignettes/00_*` through `vignettes/18_*`

### Infrastructure kept

- `.gitignore`
- `_pkgdown.yml`
- `artifacts/README.md`
- `backgammonr.Rproj`

## Classification

### KEEP

- engine code in `src/`
- compatibility and benchmark code in `R/`
- tests
- `docs/IMPLEMENTATION_DEEP_DIVE.md`
- new research-facing workflow files in `R/bg_*`
- explicit model-layer files in `R/bg_model_spec.R`,
  `R/bg_posterior_kernels.R`, and `R/bg_posterior_compare_workflows.R`
- sequential vignette suite `vignettes/00_*` through `vignettes/18_*`
- artifact conventions in `artifacts/`

### REFACTOR

- legacy workflow files:
  - `R/allocation_methods.R`
  - `R/statistical_api.R`
  - `R/statistical_studies.R`
  - `R/thompson_research.R`
  These remain useful, but the package should increasingly teach through the
  newer TS-first and research-facing layer.
- `R/output_tables.R`
  Compact output helpers are useful, but the package still has room to unify
  table formatting more aggressively.
- `docs/IMPLEMENTATION_DEEP_DIVE.md`
  Kept because it documents real engine structure, but it should keep tracking
  the sequential cached workflow as the package evolves.

### MERGE / REORGANIZE

- old vignette narrative
  Previous vignettes and template scripts were replaced by one sequential
  00–18 suite with explicit compute/result separation.
- public workflow documentation
  README, vignettes, and the new research-layer APIs now carry the main user
  narrative; redundant auxiliary docs were removed.
- export surface
  Legacy `@export` tags were stripped from old benchmark/statistical files so
  the curated namespace is no longer one `roxygenise()` away from re-bloating.
- package identity
  The old mixed “benchmark/demo/package-tour” structure was collapsed into one
  TS-centered sequence: identity -> setup -> truth -> workflow -> TS ->
  metrics -> openings -> comparisons -> broader state batteries.

### DELETE

Removed as stale, redundant, generated, or harmful to clarity:

- scratch notebooks and scripts:
  - `research_random_play_thompson_backgammon.Rmd`
  - `walkthrough.Rmd`
  - `goingthrough.R`
- obsolete vignette tooling:
  - empty `scripts/` directory after template removal
  - `scripts/generate_vignettes.R`
  - `scripts/render_main_question_pdf.R`
  - `scripts/simple_functions_quickstart.R`
  - `scripts/run_vignette_examples.R`
  - `scripts/ts_first_pass_studies.R`
  - `scripts/vignette_templates/`
- generated artifacts:
  - `figure/`
  - `man/figures/`
  - `vignettes/_example_outputs/`
  - `vignettes/06_main_question_example_quick.pdf`
- redundant docs:
  - `docs/FUNCTION_REFERENCE.md`
  - `docs/STATISTICAL_WORKFLOW.md`
  - `docs/THOMPSON_MOTIVATION.md`
- old vignette set:
  - previous mixed `vignettes/01_*` through `vignettes/20_*`
    sequence

## Why the New Structure Is Better

The new repo is organized around:

1. engine + allocation code;
2. research-facing public workflows;
3. saved truth / saved study infrastructure;
4. a sequential vignette system.

That aligns the codebase with the actual scientific goal: a Thompson-sampling
study in a structured Monte Carlo backgammon environment.
