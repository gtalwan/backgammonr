# Artifact Layout

This directory is the recommended home for large local study artifacts that
should **not** be committed as package source.

Suggested layout:

- `artifacts/truth/state/`
- `artifacts/truth/opening/`
- `artifacts/truth/battery/`
- `artifacts/study/opening/`
- `artifacts/study/profile/`
- `artifacts/study/comparison/`
- `artifacts/study/state_battery/`
- `artifacts/study/traces/`
- `artifacts/study/crn/`

The package APIs also default to `tools::R_user_dir("backgammonr", "cache")`
for cached truth and study objects. Use whichever local storage pattern fits
your presentation workflow, but keep heavy `.rds` artifacts out of the tracked
package source tree.
