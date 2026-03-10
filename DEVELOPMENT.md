# DEVELOPMENT NOTES

## Current Design Principles

1. Backgammon mechanics are environment infrastructure.
2. Statistical allocation/evaluation logic is the core contribution.
3. Performance-critical rollout loops stay in C++.
4. R layer handles interfaces, diagnostics, plotting, and benchmarking.
5. Outputs are designed to be rectangular and reproducible.

## Recent Refactor Highlights

1. Added OCBA evaluator and rollout-family wrappers.
2. Added variance-reduction options (`dice_mode`, `crn`) to evaluation APIs.
3. Added native C++ allocation tracing export to avoid expensive repeated
   re-evaluation in R.
4. Added user-facing visualization/reporting modules.
5. Added research-oriented alias API (`initialize_board`, `simulate_game`,
   `benchmark_evaluators`, etc.).
6. Expanded benchmark design to cross methods, budgets, dice modes, and CRN.

## Known Limitations

1. Successive elimination API currently routes to an OCBA-backed surrogate.
2. Best-action claims are always rollout-model-relative.
3. Engine-level completeness still determines upper bound on statistical trust.
4. Posterior diagnostics are Monte Carlo approximations, not exact integrals.

## Testing Strategy

1. Engine tests for board/move/roll/turn/game behavior.
2. Statistical tests for allocation accounting, reproducibility, and metric
   shape correctness.
3. Visualization smoke tests to ensure plotting helpers execute and return
   structured data.

## Next Engineering Steps

1. Native C++ successive elimination with full trace support.
2. Additional benchmark fixtures with curated difficulty tiers.
3. Continuous performance regression checks for rollout throughput.
4. Vignette expansion with synthetic known-probability calibration examples.
