# Analysis Entry Points

Start with [00_walkthrough_and_results.R](/Users/gabrielalwan/Downloads/backgammonr/analysis/00_walkthrough_and_results.R). It is the main guided walkthrough for the cleaned research layer and uses the cached million-rollout opening truths in [cache/opening_truths_restart](/Users/gabrielalwan/Downloads/backgammonr/cache/opening_truths_restart).

Focused scripts:

- [01_opening_truth_overview.R](/Users/gabrielalwan/Downloads/backgammonr/analysis/01_opening_truth_overview.R): load the 21 opening truths, certify them, and save the main truth tables/plots.
- [02_ts_vs_ttts_opening_study.R](/Users/gabrielalwan/Downloads/backgammonr/analysis/02_ts_vs_ttts_opening_study.R): run the coherent opening study comparing TS, TTTS, and the equal-allocation baseline.
- [03_build_reward_truth_caches.R](/Users/gabrielalwan/Downloads/backgammonr/analysis/03_build_reward_truth_caches.R): build one master scored-outcome opening battery, then materialize separate cached truth folders for `scalar_payoff`, `win_loss`, and `categorical_outcome`.

All generated tables, plots, and saved study objects are written under `analysis/output/`. That directory is intentionally ignored by git and excluded from package builds.
