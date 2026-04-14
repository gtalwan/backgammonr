# Results Folder

This folder is a simple, direct-run walkthrough built on the preserved master
truth cache in [cache/opening_truths_master](/Users/gabrielalwan/Downloads/backgammonr/cache/opening_truths_master).

## Start Here

Read the files in this order:

1. [01_all_opening_truths.R](/Users/gabrielalwan/Downloads/backgammonr/results/01_all_opening_truths.R)
2. [02_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/02_ts_vs_equal.R)
3. [03_ttts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/03_ttts_vs_equal.R)
4. [04_multi_sample_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/04_multi_sample_ts_vs_equal.R)
5. [05_soft_elimination_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/05_soft_elimination_ts_vs_equal.R)
6. [06_forced_exploration_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/06_forced_exploration_ts_vs_equal.R)
7. [07_top_k_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/07_top_k_ts_vs_equal.R)
8. [08_ts_family_one_roll.R](/Users/gabrielalwan/Downloads/backgammonr/results/08_ts_family_one_roll.R)
9. [09_student_t_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/09_student_t_ts_vs_equal.R)
10. [10_dirichlet_ts_vs_equal.R](/Users/gabrielalwan/Downloads/backgammonr/results/10_dirichlet_ts_vs_equal.R)
11. [11_what_we_missed.R](/Users/gabrielalwan/Downloads/backgammonr/results/11_what_we_missed.R)
12. [12_in_game_board_ts.R](/Users/gabrielalwan/Downloads/backgammonr/results/12_in_game_board_ts.R)

## What To Change

Every one-opening file has the same configuration block near the top:

```r
roll <- "1-6"
stack <- "beta_bernoulli"
budget <- results_run_budget()
checkpoints <- results_checkpoint_grid(budget)
```

Change:
- `roll` to another opening like `"3-5"` or `"4-6"`
- `stack` to `"beta_bernoulli"`, `"student_t"`, or `"dirichlet"`
- `budget` to any integer budget you want
- `checkpoints` to any vector of checkpoint budgets you want

Example:

```r
roll <- "4-6"
budget <- 4096L
checkpoints <- c(32L, 64L, 128L, 256L, 512L, 1024L, 2048L, 4096L)
```

## What Each One-Opening File Shows

Each one-opening method file saves:

- a truth overview table
- a truth action table
- a checkpoint summary table
- a final summary table
- `plot_bg_truth()`
- `plot_bg_ts_trace()` for the method and for equal
- `plot_bg_rank_compare()` for the method and for equal
- `plot_bg_allocation()` for the method and for equal
- high-level checkpoint plots for:
  - `top1_match`
  - `simple_regret`
  - `spearman`
  - `share_top2_truth`
  - `recommended_prob_best`

## The In-Game Board File

[12_in_game_board_ts.R](/Users/gabrielalwan/Downloads/backgammonr/results/12_in_game_board_ts.R) is the one file in this folder that does **not** use the opening master cache.

It instead:
- builds one custom `bg_board()` object directly
- fixes one realized in-game roll
- constructs a local `bg_problem()`
- builds a fresh local proxy truth with `bg_truth_state()`
- runs `bg_ts_run()` on that in-game decision problem

Change near the top of that file:
- `build_demo_midgame_board()`
- `roll <- bg_roll(...)`
- `truth_budget`
- `budget`
- `checkpoints`

## Where Outputs Go

- plots: [results/output/plots](/Users/gabrielalwan/Downloads/backgammonr/results/output/plots)
- tables: [results/output/tables](/Users/gabrielalwan/Downloads/backgammonr/results/output/tables)
- study objects: [results/output/studies](/Users/gabrielalwan/Downloads/backgammonr/results/output/studies)
