# KITE 26.10

A new model, `mahlkow_wanner_2021`, and bug fixes for `caliendo_parro_2015`
and `chowdhry_hinz_kamin_wanner_2022`. Results of multi-sector runs without
coalition transfers and without export subsidies are bit-identical to 26.09.

- **New model `mahlkow_wanner_2021`: carbon tax, climate club and border
  carbon adjustment.** Extends `caliendo_parro_2015` with a carbon tax that
  the members of a climate club (`countries_climate_club`) levy on the
  carbon content (`carbon_intensity`) of all goods used at home, a border
  carbon tariff on the carbon embodied in imports from non-members
  (`scenario_carbon_tariff`) and an export rebate on exports to non-members
  (`scenario_export_rebate`), both in the sectors `cbam_sector`. The tax is
  per unit of carbon, so the model needs initial price levels (`price`).
  It supports all instruments, trade-balance rules and settings of
  `caliendo_parro_2015`, one-sector models and the same numeraire (world
  value added). Without a carbon tax it reproduces `caliendo_parro_2015`.
  `process_results()` adds tax revenue, border carbon tariff revenue, export
  rebate costs, emissions and price levels, includes the tax revenue in
  income, and reports a tax-inclusive consumer price index.
- **Plain vectors in the initial conditions** (such as a scalar policy
  setting) no longer become model dimensions.

- **One-sector models now solve and process.** With a single sector, array
  slices dropped to vectors: `process_results()` failed for
  `caliendo_parro_2015` and the `chowdhry_hinz_kamin_wanner_2022` solver
  failed in its expenditure and transfer updates. A new internal helper,
  `slice_matrix()`, keeps such slices as matrices. *Behaviour change:*
  country x sector results of one-sector runs (e.g. `price_change`) now keep
  their `sector` column.
- **Coalition transfers in processed income and welfare.** *Behaviour
  change:* for `chowdhry_hinz_kamin_wanner_2022`, `process_results()` now adds
  the solver's coalition `transfer` to `income_new`, so `income_change` and
  `welfare_change` include it. Coalition members now show the common welfare
  change that the transfer rule sets. Runs without a coalition do not change.
- **Export subsidies in CHKW value added.** *Behaviour change:* the
  `chowdhry_hinz_kamin_wanner_2022` solver now computes `value_added_new`
  from trade flows net of tariffs and export subsidies, as
  `caliendo_parro_2015` does. This changes `value_added_new` and, under the
  `fixed_country_share` and `fixed_global_share` trade-balance rules, the
  equilibrium, whenever export subsidies differ from one.

# KITE 26.09

Public release. Merges release/26.05 into main.

- Adds a LICENSE file with the standard GPL-3 notice; `License: GPL-3`
  in `DESCRIPTION`. Alternative or commercial licensing terms remain
  available on request from `KITE@kielinstitut.de`.
- Rewritten README.
- Standardizes `trade_elasticity` as the positive Fréchet parameter theta;
  legacy inverse and sign conventions are converted with a warning.
- Fixes solved trade-balance and population accounting in processed income and
  welfare, CHKW coalition transfers, convergence metadata, tariff-revenue and
  zero-production change fields, deterministic dimension ordering, and the
  CP2015 active-IO convergence criterion near autarky.

# KITE 26.05 — Balmy Cumbuco

## Breaking changes

`update_equilibrium()` and `process_results()` have been redesigned around
an S3 result type. Existing 24.01 scripts will need a one-time migration.

| Surface | What changed | Migration |
|---|---|---|
| `update_equilibrium()` return | Plain list → S3-classed `kite_result` with a model-specific subclass and an attached `info` block (`convergence`, `criterion`, `iterations`, `elapsed_seconds`). | Top-level element names (`model`, `initial_conditions`, `model_scenario`, `output`, `settings`) are preserved. Code accessing those by name keeps working. |
| `process_results()` | Plain function with internal `if` branches → S3 generic dispatching on the model class set by `update_equilibrium()`. | User-facing call `process_results(result)` is unchanged. Custom downstream methods should adopt S3 (`process_results.<class>`). |
| `settings` list | New optional keys: `vfactor` (defaults to `0.1`), `convergence_method` (defaults to `"root_mean_square"`), `tolerance_expenditure`, `tolerance_output`, `require_inner_convergence` (defaults to `TRUE`), `max_inner_iterations` (defaults to `5000`). | Existing scripts continue to work; pass `vfactor` or any other key explicitly only if you want to override the default. The old `tolerance` is still accepted as a fallback for both per-loop tolerances. |
| `cast_variable()` default `variable_order` | The default index ordering has been simplified. | Pass `variable_order = c(...)` explicitly if you relied on the old default. |
| Initial conditions | The 24.01 helper used to iterate baseline wages/expenditures to model consistency. 26.05 expects pre-calibrated initial conditions (the package no longer ships an iteration helper). | Use initial-conditions data shipped with the model or built externally; contact `KITE@kielinstitut.de` for a calibrated baseline. |

## New features

- `array_sum()` and `array_sweep()` are exported helpers for vector / sweep operations across named array margins, matching `base::sweep()` semantics.
- `check_convergence()` and `format_time_diff()` are exported diagnostic helpers.
- `inst/examples/example.R` is a shipped end-to-end tutorial. Locate it via `system.file("examples", "example.R", package = "KITE")`.
- `inst/CITATION` provides two `bibentry` calls — the KITE Whitepaper as the canonical reference and a `Manual` entry for the package version itself. `citation("KITE")` returns both.
- `inst/KITE.bib` ships the same two entries in plain BibTeX for direct copy-paste.

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
