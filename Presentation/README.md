# Presentation Folder

This folder is the main human-facing walkthrough layer for `backgammonr`.

Start with:

- [00_start_here.R](/Users/gabrielalwan/Downloads/backgammonr/Presentation/00_start_here.R)

The scripts are intentionally numbered and build a single story:

1. what the package studies,
2. what the cached master opening truths look like,
3. what one Thompson run looks like,
4. how the Thompson-family methods compare on one opening,
5. how TS compares with equal allocation,
6. how those results look across all 21 openings,
7. how sensitive the story is to the model stack,
8. how to interpret the main metrics,
9. which figures are ready for a talk,
10. how the game mechanics layer works,
11. how all 21 opening rolls look as a battery, and
12. which user-facing functions you actually need,
13. how canonical TS compares quickly with the other TS variants,
14. how Student-t TS compares with equal allocation, and
15. how Dirichlet TS compares with equal allocation.

Outputs are written to:

- [Presentation/output/plots](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/plots)
- [Presentation/output/tables](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/tables)
- [Presentation/output/studies](/Users/gabrielalwan/Downloads/backgammonr/Presentation/output/studies)

The presentation scripts use the preserved master cache in:

- [cache/opening_truths_master](/Users/gabrielalwan/Downloads/backgammonr/cache/opening_truths_master)

They do not rebuild or modify that cache.
