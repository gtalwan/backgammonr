# Development Guide

## Package Identity

`backgammonr` is a Thompson-sampling research package first and a backgammon
engine second.

The package should present one local decision problem as its canonical object:

- one board state;
- one realized roll;
- `K` legal root actions;
- rollout-model values under a declared continuation policy;
- a fixed rollout budget;
- an allocation policy that decides how that budget is spent.

## Statistical Non-Negotiables

User-facing code and docs must keep these separate:

1. rollout-model value;
2. proxy truth / proxy reference;
3. unavailable expert or game-theoretic truth.

Do not blur those concepts in code comments, object names, or vignette prose.

## Current Architecture

### Engine layer

- `src/bg_board.cpp`
- `src/bg_move.cpp`
- `src/bg_movegen.cpp`
- `src/bg_rules.cpp`
- `src/bg_game.cpp`
- `src/bg_simulation.cpp`
- `src/bg_allocation.cpp`
- `src/bg_parallel.cpp`

### Public research workflow layer

- `R/ts_frontdoor.R`
- `R/bg_research_frontdoor.R`
- `R/bg_truth_workflows.R`
- `R/bg_eval_workflows.R`
- `R/bg_state_workflows.R`
- `R/bg_research_plots.R`

### Legacy compatibility layer

- `R/allocation_methods.R`
- `R/statistical_api.R`
- `R/statistical_studies.R`
- `R/thompson_research.R`
- `R/visualization.R`

The current direction is to preserve the engine and compatibility layers while
teaching and extending the package through the research workflow layer.

## Sequential Workflow Principle

The vignette system and heavy study functions should reflect this order:

1. setup parallelism, cache paths, and seeds;
2. build and save truth objects;
3. build and save experiment objects;
4. load saved objects in later analytical workflows.

Do not bury caching and performance setup late in the narrative.

## Performance Rules

Prefer:

- caching legal moves and collapsed post-move states;
- minimizing R/C++ boundary crossings;
- deterministic worker-count reproducibility;
- saved study objects for repeated presentation use.

Optional advanced modes such as batched TS and CRN must stay opt-in and clearly
labeled.

## Cleanup Standard

Do not keep:

- scratch notebooks;
- generated figures;
- stale template systems;
- example-output dumps that are not part of the package contract;
- overlapping documentation narratives.

Keep the repo intentional.

## Validation Checklist

Before closing substantial work:

1. `roxygen2::roxygenise(".")` if exports/docs changed.
2. `testthat::test_local(".")` or a focused subset when runtime matters.
3. Verify the TS-first front door still works.
4. Confirm no generated junk is left in the repo root.
