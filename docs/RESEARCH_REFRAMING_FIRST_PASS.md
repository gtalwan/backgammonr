# Research Reframing and First-Pass Plan

## North Star

`backgammonr` is a computational-statistics package about one main question:

> How should expensive Monte Carlo rollouts be allocated in order to learn the move-value structure of a backgammon state?

The estimand is explicit:

- start from a board state and a realized roll;
- commit to one legal move;
- continue the rest of the game under a defined rollout policy;
- measure the expected reward of that move under that rollout environment.

This package does **not** claim expert backgammon truth or game-theoretic truth.
It studies **random-play rollout truth approximated by high-budget Monte Carlo**.

## What This First Pass Adds

The first pass stays conservative about engine semantics and builds a cleaner
research layer on top of the current implementation.

Implemented additions:

- `bg_truth_state()`, `bg_truth_battery()`, `bg_truth_opening()`
  Persistent proxy-truth workflows with metadata, caching, and save/load support.
- explicit `reward_model` / `posterior_model` settings on `bg_problem()`
  and truth-building wrappers
  The current front door now declares an explicit scalar-payoff default and
  exposes coherent categorical and win/loss alternatives instead of hiding the
  model layer behind legacy aliases.
- `bg_truth_save()`, `bg_truth_load()`, `bg_truth_diagnostics()`
  Auditable storage for large truth objects and compact diagnostics for display.
- `bg_ttts_run()`, `bg_ucb_run()`, `bg_uniform_run()`, `bg_ts_trace()`, `bg_ts_profile()`, `bg_compare_algorithms()`
  A cleaner algorithm-facing family that keeps canonical TS and TTTS central while treating scalar comparators honestly as scalar-engine tools.
- `bg_eval_top1()`, `bg_eval_rank()`, `bg_eval_allocation()`, `bg_eval_reference_aware()`, `bg_eval_seed_stability()`
  A layered evaluation framework covering top-decision, ranking, allocation, uncertainty-aware, and stability diagnostics.
- `bg_state_classify()`, `bg_state_difficulty()`, `bg_state_battery()`
  A heuristic state-taxonomy layer for studying where TS is strong or fragile.
- `plot_bg_truth()`, `plot_bg_ts_trace()`, `plot_bg_budget_curve()`, `plot_bg_rank_compare()`, `plot_bg_state_battery()`
  A smaller set of question-specific figures rather than larger generic plotting surfaces.

## Compute and Storage Plan

High-budget truth construction is the main compute bottleneck. The package
should treat those objects as reusable research artifacts.

Current strategy:

- parallelize **within** a truth build via `bg_reference(..., workers_truth = n_cores)`;
- default truth workflows to a single large truth job at a time rather than
  oversubscribing many opening-roll jobs simultaneously;
- save state-level truths to `.rds` artifacts with:
  - state identifier;
  - rollout-model definition;
  - reference budget;
  - Monte Carlo uncertainty summaries;
  - creation time;
  - seed;
  - package version;
  - cache path.

Recommended opening-truth practice:

- use all 12 cores inside each truth build;
- save one artifact per opening roll;
- save a battery-level manifest after the state-level artifacts exist;
- extend saved truths rather than restarting them when the budget grows.

## Evaluation Philosophy

Do not reduce the project to one regret number.

Primary metrics:

- top-1 agreement;
- simple regret;
- epsilon-optimal selection;
- Spearman and Kendall rank correlation;
- top-k overlap;
- pairwise ordering accuracy;
- weighted rank loss;
- budget share spent on truth-top moves;
- budget spent on moves that remain proxy-Monte-Carlo-screened suboptimal;
- seed stability and selection entropy;
- gap-aware correctness via moves that are not Monte-Carlo-separated from the best.

The core package story should come from these metrics jointly, not from bandit
terminology alone.

## Thompson-Centered Research Agenda

Thompson sampling remains the main character. Comparators exist to sharpen the
interpretation of TS.

Near-term questions:

- When does canonical TS identify the best rollout move quickly?
- When does it become seed-sensitive?
- How much budget does it spend on clearly dominated actions?
- When do TTTS or pure-exploration surrogates improve top-rank recovery?
- How differently does UCB behave when the goal is ranking recovery rather than
  generic optimism?
- Which state classes have small truth gaps and high variance?
- Which state features predict practical difficulty?

High-value TS extensions to prioritize:

1. TTTS and other top-two probability rules.
2. Budget-aware or phase-aware TS.
3. Batched TS semantics that map well to multi-core execution.
4. Common-random-number experiments for pairwise uncertainty reduction.
5. Posterior comparisons:
   - scalar pseudo-Beta default;
   - categorical/Dirichlet outcome model;
   - Student-t scalar model;
   - bootstrap-based TS.

## State Batteries Beyond Openings

Openings should be the first flagship truth battery, but not the last.

The next layer is game-derived states sampled from random or heuristic games and
classified into coarse structural groups:

- opening;
- race;
- bear-in / bear-off;
- mutual contact;
- blitz;
- priming game;
- holding game;
- backgame.

The current classifier is intentionally heuristic. It is a scaffolding for
state-stratified experiments, not a claim of exact strategic labeling.

## Visual and Table Design Rules

Figures should answer one question each.

Preferred figure types:

- proxy-truth interval plots;
- TS allocation-path plots;
- budget-performance curves with uncertainty ribbons;
- estimated-value vs proxy-truth dumbbell plots;
- state-gap vs legal-move-count scatterplots.

Preferred table types:

- top-move truth summary;
- compact truth-cache manifest;
- method-by-budget comparison summary;
- seed-stability summary;
- state-battery difficulty summary.

Avoid:

- giant action tables in vignettes;
- unfiltered legends for all moves;
- multi-metric dashboards that hide the main question.

## C++ / Rcpp Guidance

The current code already uses `Rcpp` and `RcppParallel` in important places.
This first pass deliberately does **not** rewrite the engine.

Safe next profiling targets:

- legal-move generation hotspots;
- rollout inner loops;
- repeated checkpoint reconstruction in large seed sweeps;
- pairwise metric kernels for large truth batteries.

Requirements before any further C++ optimization:

- fixed-seed equivalence checks;
- explicit tests for rule preservation;
- runtime benchmarks on representative opening and contact states.

## Likely Conclusions to Target

The package should be able to support claims like these once the batteries are
fully built:

- TS dominates equal allocation on many states, but not uniformly.
- TTTS helps most on small-gap states where the top two moves are hard to
  separate.
- ranking metrics reveal algorithm differences that top-1 match misses.
- some state classes are intrinsically harder because they combine many legal
  moves, small truth gaps, and heteroskedastic rollout variance.
- cached opening-roll truths make repeated studies practical.
- multi-core proxy-truth construction is viable when the truth objects are
  stored and extended rather than recomputed from scratch.
