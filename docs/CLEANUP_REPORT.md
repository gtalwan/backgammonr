# Cleanup Report

This report records the current first-pass cleanup after the audit:

- repo junk removed;
- public API curated hard;
- legacy source exports stripped back so the curated namespace is durable;
- misleading surrogate naming removed;
- repeated-study parallelism pushed into major workflows;
- explicit reward/posterior model layer kept and documented honestly;
- UCB kept as a first-class public comparator family, but only on the scalar
  legacy engine where that comparison is currently honest;
- vignette system rebuilt into a tighter 00--18 staged workflow.

## Removed Immediately

Deleted as junk or build/session residue:

- `.DS_Store`
- `.RData`
- `.Rhistory`
- `src/*.o`
- `src/*.so`
- `tests/testthat/Rplots.pdf`
- `backgammonr-chatgpt-review.zip`

Deleted as stale public documentation or vignette clutter:

- all legacy `man/*.Rd` topics outside the curated front-door API
- the over-split 08--26 vignette branch that had grown too thin and repetitive

## Renamed

- `bg_sr_run()` -> `bg_ocba_run()`

Reason:

- the implementation was not a true Successive Rejects engine;
- the old name was semantically misleading;
- the new name matches the actual OCBA-style baseline that is implemented.

## Split / Reorganized

- repeated-study entry points moved out of [R/ts_frontdoor.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_frontdoor.R) into [R/ts_compare_studies.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_compare_studies.R)
- explicit model layer stabilized around:
  - [R/bg_model_spec.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_model_spec.R)
  - [R/bg_posterior_kernels.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_posterior_kernels.R)
  - [R/bg_posterior_compare_workflows.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_posterior_compare_workflows.R)
- legacy benchmark/statistical files had plain `@export` tags stripped so the
  curated namespace is no longer contradicted by the source tree.

Reason:

- `bg_ts_profile()` and `bg_compare_methods()` are workflow-study code, not
  one-problem TS front-door code;
- this reduces the main TS file's responsibility and makes the workflow
  boundary clearer.

## Public API After Curation

Exported front-door surface now consists of:

- domain/problem setup:
  - `bg_apply_move_sequence`
  - `bg_board`
  - `bg_initial_board`
  - `bg_legal_moves`
  - `bg_play_game`
  - `bg_play_turn`
  - `bg_print_board`
  - `bg_problem`
  - `bg_roll`
  - `bg_validate_board`
- truth/reference:
  - `bg_reference`
  - `bg_truth_state`
  - `bg_truth_opening`
  - `bg_truth_battery`
  - `bg_truth_save`
  - `bg_truth_load`
  - `bg_truth_diagnostics`
  - `bg_study_save`
  - `bg_study_load`
- Thompson workflows:
  - `bg_ts_decide`
  - `bg_ts_run`
  - `bg_ttts_run`
  - `bg_ucb_run`
  - `bg_uniform_run`
  - `bg_ts_trace`
  - `bg_ts_profile`
  - `bg_compare_algorithms`
  - `bg_compare_posteriors`
  - `bg_compare_reward_models`
  - `bg_opening_study`
- evaluation:
  - `bg_eval_top1`
  - `bg_eval_rank`
  - `bg_eval_allocation`
  - `bg_eval_reference_aware`
  - `bg_eval_seed_stability`
- state analysis:
  - `bg_board_features`
  - `bg_move_features`
  - `bg_state_classify`
  - `bg_state_difficulty`
  - `bg_state_battery`
- plots:
  - `plot_bg_truth`
  - `plot_bg_ts_trace`
  - `plot_bg_allocation`
  - `plot_bg_budget_curve`
  - `plot_bg_posterior_compare`
  - `plot_bg_rank_compare`
  - `plot_bg_state_battery`

Everything else now remains internal package machinery.

## Repo Manifest

### KEEP

Top-level package/infrastructure:

- [DESCRIPTION](/Users/gabrielalwan/Downloads/backgammonr/DESCRIPTION)
- [LICENSE](/Users/gabrielalwan/Downloads/backgammonr/LICENSE)
- [LICENSE.md](/Users/gabrielalwan/Downloads/backgammonr/LICENSE.md)
- [NAMESPACE](/Users/gabrielalwan/Downloads/backgammonr/NAMESPACE)
- [README.md](/Users/gabrielalwan/Downloads/backgammonr/README.md)
- [_pkgdown.yml](/Users/gabrielalwan/Downloads/backgammonr/_pkgdown.yml)
- [.gitignore](/Users/gabrielalwan/Downloads/backgammonr/.gitignore)
- [.Rbuildignore](/Users/gabrielalwan/Downloads/backgammonr/.Rbuildignore)
- [DEVELOPMENT.md](/Users/gabrielalwan/Downloads/backgammonr/DEVELOPMENT.md)
- [NEWS.md](/Users/gabrielalwan/Downloads/backgammonr/NEWS.md)
- [backgammonr.Rproj](/Users/gabrielalwan/Downloads/backgammonr/backgammonr.Rproj)

Core domain/engine-facing R files:

- [R/board.R](/Users/gabrielalwan/Downloads/backgammonr/R/board.R)
- [R/dice.R](/Users/gabrielalwan/Downloads/backgammonr/R/dice.R)
- [R/game.R](/Users/gabrielalwan/Downloads/backgammonr/R/game.R)
- [R/legal_moves.R](/Users/gabrielalwan/Downloads/backgammonr/R/legal_moves.R)
- [R/move.R](/Users/gabrielalwan/Downloads/backgammonr/R/move.R)
- [R/performance_profile.R](/Users/gabrielalwan/Downloads/backgammonr/R/performance_profile.R)
- [R/print.R](/Users/gabrielalwan/Downloads/backgammonr/R/print.R)
- [R/random_player.R](/Users/gabrielalwan/Downloads/backgammonr/R/random_player.R)
- [R/simulation.R](/Users/gabrielalwan/Downloads/backgammonr/R/simulation.R)

Research/front-door R files:

- [R/bg_model_spec.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_model_spec.R)
- [R/bg_posterior_kernels.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_posterior_kernels.R)
- [R/bg_posterior_compare_workflows.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_posterior_compare_workflows.R)
- [R/bg_research_frontdoor.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_research_frontdoor.R)
- [R/bg_truth_workflows.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_truth_workflows.R)
- [R/bg_eval_workflows.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_eval_workflows.R)
- [R/bg_state_workflows.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_state_workflows.R)
- [R/bg_research_plots.R](/Users/gabrielalwan/Downloads/backgammonr/R/bg_research_plots.R)
- [R/ts_frontdoor.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_frontdoor.R)
- [R/ts_compare_studies.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_compare_studies.R)
- [R/ts_methods.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_methods.R)
- [R/ts_studies.R](/Users/gabrielalwan/Downloads/backgammonr/R/ts_studies.R)
- [R/output_tables.R](/Users/gabrielalwan/Downloads/backgammonr/R/output_tables.R)
- [R/backgammonr-package.R](/Users/gabrielalwan/Downloads/backgammonr/R/backgammonr-package.R)

C++ / src:

- [src/Makevars](/Users/gabrielalwan/Downloads/backgammonr/src/Makevars)
- [src/Makevars.win](/Users/gabrielalwan/Downloads/backgammonr/src/Makevars.win)
- [src/RcppExports.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/RcppExports.cpp)
- [src/bg_allocation.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_allocation.cpp)
- [src/bg_allocation.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_allocation.h)
- [src/bg_benchmark.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_benchmark.cpp)
- [src/bg_benchmark.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_benchmark.h)
- [src/bg_board.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_board.cpp)
- [src/bg_board.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_board.h)
- [src/bg_dice.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_dice.cpp)
- [src/bg_dice.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_dice.h)
- [src/bg_game.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_game.cpp)
- [src/bg_game.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_game.h)
- [src/bg_heuristic.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_heuristic.cpp)
- [src/bg_heuristic.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_heuristic.h)
- [src/bg_move.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_move.cpp)
- [src/bg_move.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_move.h)
- [src/bg_movegen.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_movegen.cpp)
- [src/bg_movegen.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_movegen.h)
- [src/bg_parallel.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_parallel.cpp)
- [src/bg_random_player.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_random_player.cpp)
- [src/bg_random_player.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_random_player.h)
- [src/bg_rng.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_rng.h)
- [src/bg_rollout.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_rollout.cpp)
- [src/bg_rollout.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_rollout.h)
- [src/bg_rules.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_rules.cpp)
- [src/bg_rules.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_rules.h)
- [src/bg_simulation.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_simulation.cpp)
- [src/bg_simulation.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_simulation.h)
- [src/bg_thompson_rollout.cpp](/Users/gabrielalwan/Downloads/backgammonr/src/bg_thompson_rollout.cpp)
- [src/bg_thompson_rollout.h](/Users/gabrielalwan/Downloads/backgammonr/src/bg_thompson_rollout.h)

Tests:

- [tests/testthat.R](/Users/gabrielalwan/Downloads/backgammonr/tests/testthat.R)
- [tests/testthat/test-research-layer.R](/Users/gabrielalwan/Downloads/backgammonr/tests/testthat/test-research-layer.R)
- [tests/testthat/test-ts-frontdoor.R](/Users/gabrielalwan/Downloads/backgammonr/tests/testthat/test-ts-frontdoor.R)

Docs/artifacts:

- [artifacts/.gitkeep](/Users/gabrielalwan/Downloads/backgammonr/artifacts/.gitkeep)
- [artifacts/README.md](/Users/gabrielalwan/Downloads/backgammonr/artifacts/README.md)
- [docs/CLEANUP_MANIFEST.md](/Users/gabrielalwan/Downloads/backgammonr/docs/CLEANUP_MANIFEST.md)
- [docs/CLEANUP_REPORT.md](/Users/gabrielalwan/Downloads/backgammonr/docs/CLEANUP_REPORT.md)
- [docs/IMPLEMENTATION_DEEP_DIVE.md](/Users/gabrielalwan/Downloads/backgammonr/docs/IMPLEMENTATION_DEEP_DIVE.md)
- [docs/MODEL_AUDIT_TABLE.md](/Users/gabrielalwan/Downloads/backgammonr/docs/MODEL_AUDIT_TABLE.md)
- [docs/POSTERIOR_MODEL_FIRST_PASS.md](/Users/gabrielalwan/Downloads/backgammonr/docs/POSTERIOR_MODEL_FIRST_PASS.md)
- [docs/PROVISIONAL_CONCLUSIONS.md](/Users/gabrielalwan/Downloads/backgammonr/docs/PROVISIONAL_CONCLUSIONS.md)
- [docs/RESEARCH_REFRAMING_FIRST_PASS.md](/Users/gabrielalwan/Downloads/backgammonr/docs/RESEARCH_REFRAMING_FIRST_PASS.md)

Vignettes:

- [vignettes/00_mathematical_foundations_of_finite_budget_backgammon_decision_problems.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/00_mathematical_foundations_of_finite_budget_backgammon_decision_problems.Rmd)
- [vignettes/01_opening_proxy_truth_battery_all_21_opening_rolls.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/01_opening_proxy_truth_battery_all_21_opening_rolls.Rmd)
- [vignettes/02_backgammon_game_mechanics_and_curated_package_functionality.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/02_backgammon_game_mechanics_and_curated_package_functionality.Rmd)
- [vignettes/03_single_state_thompson_sampling_walkthrough_against_cached_truth.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/03_single_state_thompson_sampling_walkthrough_against_cached_truth.Rmd)
- [vignettes/04_thompson_vs_non_thompson_methods_on_the_opening_battery.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/04_thompson_vs_non_thompson_methods_on_the_opening_battery.Rmd)
- [vignettes/05_thompson_family_variants_interactive_walkthrough_and_comparison.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/05_thompson_family_variants_interactive_walkthrough_and_comparison.Rmd)
- [vignettes/06_basic_thompson_sensitivity_to_reward_and_posterior_models.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/06_basic_thompson_sensitivity_to_reward_and_posterior_models.Rmd)
- [vignettes/07_why_opening_states_matter_and_where_the_research_goes_next.Rmd](/Users/gabrielalwan/Downloads/backgammonr/vignettes/07_why_opening_states_matter_and_where_the_research_goes_next.Rmd)

### REFACTOR

Useful but still too legacy-heavy or too broad in responsibility:

- [R/allocation_methods.R](/Users/gabrielalwan/Downloads/backgammonr/R/allocation_methods.R)
- [R/benchmarking.R](/Users/gabrielalwan/Downloads/backgammonr/R/benchmarking.R)
- [R/heuristic_player.R](/Users/gabrielalwan/Downloads/backgammonr/R/heuristic_player.R)
- [R/ocba_rollout_player.R](/Users/gabrielalwan/Downloads/backgammonr/R/ocba_rollout_player.R)
- [R/print_benchmark.R](/Users/gabrielalwan/Downloads/backgammonr/R/print_benchmark.R)
- [R/recommendation.R](/Users/gabrielalwan/Downloads/backgammonr/R/recommendation.R)
- [R/reporting.R](/Users/gabrielalwan/Downloads/backgammonr/R/reporting.R)
- [R/rollout_player.R](/Users/gabrielalwan/Downloads/backgammonr/R/rollout_player.R)
- [R/statistical_api.R](/Users/gabrielalwan/Downloads/backgammonr/R/statistical_api.R)
- [R/statistical_studies.R](/Users/gabrielalwan/Downloads/backgammonr/R/statistical_studies.R)
- [R/thompson_research.R](/Users/gabrielalwan/Downloads/backgammonr/R/thompson_research.R)
- [R/thompson_rollout_player.R](/Users/gabrielalwan/Downloads/backgammonr/R/thompson_rollout_player.R)
- [R/visualization.R](/Users/gabrielalwan/Downloads/backgammonr/R/visualization.R)

Reason:

- these files still carry compatibility layers, older naming, or overlapping
  functionality that should either be folded into the front-door research
  layer or explicitly retired later.

### DELETE

Already deleted in this pass:

- repo junk and build artifacts listed above
- stale session files
- stale zipped review artifact
- legacy `man/` topics for unexported/retired functions
- over-split 08--26 vignette branch

## Remaining Weak Spots

Still not finished:

- the old legacy R layer remains in the repo and still needs further deletion
  or merger once compatibility decisions are finalized;
- TS variant coverage is still not deep enough on posterior design and bounded
  categorical outcomes;
- the new 00--18 vignette suite is operationally better, but the heavy cached
  artifacts still need to be built for a truly finished presentation run.
