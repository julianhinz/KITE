# KITE 26.05 — Balmy Cumbuco (Flatwater)

## Breaking changes

`update_equilibrium()` and `process_results()` have been redesigned around
an S3 result type. Existing 24.01 scripts will need a one-time migration.

| Surface | What changed | Migration |
|---|---|---|
| `update_equilibrium()` return | Plain list → S3-classed `kite_result` with a model-specific subclass and an attached `info` block (`convergence`, `criterion`, `iterations`, `elapsed_seconds`). | Top-level element names (`model`, `initial_conditions`, `model_scenario`, `output`, `settings`) are preserved. Code accessing those by name keeps working. |
| `process_results()` | Plain function with internal `if` branches → S3 generic dispatching on the model class set by `update_equilibrium()`. | User-facing call `process_results(result)` is unchanged. Custom downstream methods should adopt S3 (`process_results.<class>`). |
| `settings` list | New required key: `vfactor` (no default). New optional keys: `convergence_method = "root_mean_square"`, `tolerance_expenditure`, `tolerance_output`, `require_inner_convergence = TRUE`. | Add `vfactor = 0.1` (or your tuned value) to existing `settings` lists. The old `tolerance` is still accepted as a fallback for both per-loop tolerances. |
| `cast_variable()` default `variable_order` | The default index ordering has been simplified. | Pass `variable_order = c(...)` explicitly if you relied on the old default. |
| Initial conditions | The 24.01 helper used to iterate baseline wages/expenditures to model consistency. 26.05 expects pre-calibrated initial conditions (the package no longer ships an iteration helper). | Use initial-conditions data shipped with the model or built externally; contact `KITE@kielinstitut.de` for a calibrated baseline. |

## New features

- `array_sum()` and `array_sweep()` are exported helpers for vector / sweep operations across named array margins, matching `base::sweep()` semantics.
- `check_convergence()` and `format_time_diff()` are exported diagnostic helpers.
- `inst/examples/example.R` is a shipped end-to-end tutorial. Locate it via `system.file("examples", "example.R", package = "KITE")`.
- `inst/CITATION` provides two `bibentry` calls — the KITE Whitepaper as the canonical reference and a `Manual` entry for the package version itself. `citation("KITE")` returns both.
- `inst/CITATION.bib` ships the same two entries in plain BibTeX for direct copy-paste.

## Bug fixes

- Convergence diagnostics: better eta prediction, root-mean-square / aggregate / element-wise / sample criterion methods, input validation.
- `process_results` argument validation hardened.
- README, roxygen, and reference documentation refreshed throughout.

## Notes

- The package is now dual-licensed: GPL-3 by default; a commercial licence is available on request from `KITE@kielinstitut.de` for uses that need different terms (e.g. closed-source redistribution).
- More published-paper model variants will be added in upcoming releases.

# KITE 24.01

- Initial public release of the KITE package.
- Two model variants: `caliendo_parro_2015` and `chowdhry_hinz_kamin_wanner_2022`.
