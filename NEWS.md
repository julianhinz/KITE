# KITE 26.10

Bug fixes for `caliendo_parro_2015` and `chowdhry_hinz_kamin_wanner_2022`.
Results of multi-sector `caliendo_parro_2015` runs without export subsidies
are bit-identical to 26.09. Results of `chowdhry_hinz_kamin_wanner_2022` runs
change through its new numeraire (see below); on baselines whose trade
balances sum to zero they change only within solver tolerance.

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
- **No placeholder outputs from CHKW.** *Behaviour change:* the
  `chowdhry_hinz_kamin_wanner_2022` solver no longer returns `income_new`,
  `income_old` or `price_index_change`, also when
  `additional_output_variables` requests them. The solver holds only work
  values for these (zeros, and ones for `price_index_change`). A requested
  `income_new` replaced the processed income, so `income_change` and
  `welfare_change` were 0 for every country. `process_results()` now always
  computes `income_new` and `price_index_change` from the solution.
- **Warning for scenario variables that a model does not use.**
  *Behaviour change:* `update_equilibrium()` now warns when `model_scenario`
  sets a variable that the model does not read. Before, such a variable had
  no effect and no warning: for example, `coalition_member_new` for
  `chowdhry_hinz_kamin_wanner_2022` gave zero coalition transfers. The
  warning has class `kite_unknown_scenario_variable` and the fields `model`
  and `variables`. It names the variable to set instead where one exists
  (`coalition_member`). Both solvers compute `tariff_change` and
  `export_subsidy_change` themselves, so these now warn and point to
  `tariff_new` and `export_subsidy_new`. Model functions outside the package
  are not checked. Results do not change.
- **Numeraire for CHKW.** *Behaviour change:* `caliendo_parro_2015`
  rescales the wage change in every iteration so that world value added
  stays at its baseline level. `chowdhry_hinz_kamin_wanner_2022` had no
  such rescale. Its excess-function wage update keeps the wage level only
  while the world trade balance sums to zero. Under `fixed_country_share`
  after a shock, and under any rule except `zero` on a baseline whose trade
  balances do not sum to zero, the wage level drifted without bound or
  collapsed, and the run did not converge. The solver now applies the
  rescale of `caliendo_parro_2015` after its wage update, under every
  `trade_balance_rule`. On baselines whose trade balances sum to zero,
  results under `fixed`, `fixed_global_share` and `zero` change only within
  solver tolerance (tolerance `1e-10`: wage changes by at most `5e-11`,
  welfare changes by at most `4e-10`). `fixed_country_share` and
  unbalanced-baseline runs change materially: they now converge, and
  without a coalition they stay on the scale of `caliendo_parro_2015`. In
  these runs the trade-balance rule cannot hold for the world as a whole, so
  the two solvers stop at fixed points that differ slightly.
- **No convergence when the trade-balance rule cannot hold for the world.**
  *Behaviour change:* world exports equal world imports, so the solved
  trade balances must sum to zero for the world. When they did not,
  `update_equilibrium()` still reported `convergence = TRUE`, but labour
  markets did not clear and the result depended on `vfactor`. This happens
  with `trade_balance_rule = "fixed_country_share"` after a shock, and on a
  baseline whose trade balances do not sum to zero under every rule except
  `zero`. `update_equilibrium()` now checks the world sum of
  `trade_balance_new` after the solve, with tolerance
  `settings$tolerance_accounting` (default `1e-10`) times world value
  added, and also refuses solutions with non-positive value added. If the
  check fails, `convergence` is `FALSE` and a warning of class
  `kite_world_trade_balance` (fields `model` and `accounting`) explains
  why. The new `results$info$accounting` holds the details for every run.
  The check uses only the solver output, so it applies to every model in
  the package. Results do not change: outputs of all runs are
  bit-identical to before; only `info` changes.

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
