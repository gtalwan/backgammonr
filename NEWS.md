# backgammonr NEWS

## 0.2.1 (development)

### Thompson-centered research toolkit

- Reframed package documentation and examples around Thompson sampling as the
  primary finite-budget allocation method, with equal/UCB/OCBA/greedy retained
  as baselines.
- Added Top-Two Thompson support:
  - `evaluate_actions_ttts()`
  - `evaluate_moves_ttts()`
  - method aliases: `"ttts"` and `"ttts_rollout"`.
- Added Thompson-specific research helpers:
  - `trace_thompson_allocation()`
  - `compare_thompson_to_reference()`
  - `certify_reference_truth()`
  - `benchmark_thompson()`
  - `summarize_thompson_benchmark()`
  - `plot_thompson_convergence()`
  - `plot_thompson_vs_baselines()`
- Added explicit reference-estimation naming:
  - `approximate_action_reference()` alias for
    `approximate_action_truth()`.
- Added runtime field propagation for action-evaluation outputs to simplify
  speed/accuracy reporting in benchmarks.

### Documentation and package framing

- Updated README to Thompson-centered finite-budget framing.
- Added motivation document:
  - `docs/THOMPSON_MOTIVATION.md`
- Added deep Thompson vignette:
  - `vignettes/thompson-sampling-deep-dive.Rmd`
- Reworked user workflow docs and standalone example bubbles:
  - `docs/STATISTICAL_WORKFLOW.md`
  - `docs/EXAMPLE_BUBBLES.md`
- Added/updated implementation-level deep dive:
  - `docs/IMPLEMENTATION_DEEP_DIVE.md`

### Statistical computing and allocation

- Added `evaluate_actions_ocba()` and OCBA rollout wrappers.
- Added research-oriented API aliases:
  - `initialize_board()`, `validate_board()`, `print_board()`, `plot_board()`
  - `generate_legal_moves()`, `apply_move()`, `simulate_game()`
  - `evaluate_moves_equal_allocation()`, `evaluate_moves_ucb()`,
    `evaluate_moves_thompson()`, `evaluate_moves_successive_elimination()`
  - `identify_reference_best_move()`, `benchmark_evaluators()`
  - `summarize_benchmark_results()`, `plot_benchmark_results()`,
    `plot_budget_accuracy_curve()`, `plot_runtime_curve()`
  - `explain_position()`, `explain_move_evaluation()`,
    `compare_action_posteriors()`, `trace_allocation_history()`

### Performance and tracing

- Added native C++ trace export path (`bg_cpp_allocation_evaluate_trace`) so
  `trace = TRUE` no longer requires repeated full re-evaluations at each
  checkpoint.
- Kept deterministic trace behavior under fixed seeds/settings.

### Benchmarking and metrics

- Extended `benchmark_allocation_methods()` to support crossed grids over:
  methods, budgets, `dice_mode`, and `crn`.
- Added metric aliases:
  - `compute_probability_of_correct_selection()`
  - `compute_simple_regret()`
  - `compute_value_mse()`

### Visualization/reporting

- Added board/diagnostic plotting helpers:
  - `format.bg_board()`, `plot.bg_board()`, `bg_plot_board()`
  - `bg_compare_boards()`, `bg_plot_move()`, `bg_plot_legal_moves()`
  - `bg_plot_move_ranking()`, `bg_plot_allocation_trace()`
  - `bg_plot_budget_stability()`, `bg_plot_benchmark_summary()`
- Added `bg_analysis_report()` and `plot.bg_analysis_report()`.

### Documentation

- Rewrote README with explicit ranking-and-selection framing.
- Added deep technical vignette:
  - `vignettes/statistical-foundations.Rmd`
- Added `DEVELOPMENT.md` for architecture, limitations, and next steps.
