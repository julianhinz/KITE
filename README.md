# KITE Model Suite <img src="man/figures/logo_kite.png" align="right" width="160" alt="KITE logo" />

The KITE Model Suite implements general-equilibrium quantitative trade
models in the *New Quantitative Trade Model* tradition: a multi-sector
Ricardian framework with intra- and international input–output linkages,
extending Eaton & Kortum (2002) along the lines of Caliendo & Parro (2015).
Trade policy enters via tariffs and non-tariff measures; the model returns
welfare, production, and trade-flow changes under user-specified
counterfactuals.

KITE is maintained by the Kiel Institute for the World Economy. For access
to calibrated initial conditions, contact us at
[KITE@kielinstitut.de](mailto:KITE@kielinstitut.de).

## What 26.05 ships

- `caliendo_parro_2015` — Caliendo & Parro (2015) multi-sector Ricardian model.
- `chowdhry_hinz_kamin_wanner_2022` — Chowdhry, Hinz, Kamin & Wanner (2024) sanctions-coalition extension.

More published-paper model variants will be added in upcoming releases.

## Breaking changes from 24.01

Version 26.05 is a **hard-break** architecture upgrade. If you have scripts
written against 24.01, see [`NEWS.md`](NEWS.md) for the full migration
table. The most common change: `update_equilibrium()` now returns an
S3-classed result instead of a plain list, and `process_results()` is an
S3 generic.

## Installing KITE

From GitHub:

```r
remotes::install_github("julianhinz/KITE")
```

From a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

## Usage

Running KITE follows four steps:

1. Load or build initial conditions (input–output table, trade shares, elasticities).
2. Define a counterfactual scenario (tariff or NTM changes).
3. Run `update_equilibrium()` with the chosen model.
4. Process the result with `process_results()`.

A complete worked example is shipped at:

```r
system.file("examples", "example.R", package = "KITE")
```

See `?caliendo_parro_2015` and `?chowdhry_hinz_kamin_wanner_2022` for help pages.

## Citation

Please cite the KITE Whitepaper as the canonical reference, with the
package as a software entry:

```bibtex
@techreport{HinzMahlkowWanner2025,
  author      = {Hinz, Julian and Mahlkow, Hendrik and Wanner, Joschka},
  title       = {The {KITE} Model Suite: A Quantitative Framework for International Trade Analysis},
  institution = {Kiel Institute for the World Economy},
  type        = {KITE White Paper},
  year        = {2025},
  url         = {https://trade.kielinstitut.de/KTTM/KITE_whitepaper.pdf}
}

@Manual{KITE2026,
  title  = {{KITE} Model Suite},
  author = {Hinz, Julian and Mahlkow, Hendrik and Wanner, Joschka},
  year   = {2026},
  note   = {R package version 26.05 -- Balmy Cumbuco (Flatwater)},
  url    = {https://github.com/julianhinz/KITE}
}
```

Use `citation("KITE")` for ready-to-paste entries.

## Authors

* Julian Hinz, Kiel Institute — [julian.hinz@kielinstitut.de](mailto:julian.hinz@kielinstitut.de)
* Hendrik Mahlkow, Kiel Institute — [hendrik.mahlkow@kielinstitut.de](mailto:hendrik.mahlkow@kielinstitut.de)
* Joschka Wanner, Kiel Institute — [joschka.wanner@kielinstitut.de](mailto:joschka.wanner@kielinstitut.de)

## References

- Caliendo, L. and Parro, F. (2015). Estimates of the Trade and Welfare Effects of NAFTA. *The Review of Economic Studies*, 82(1), 1–44.
- Chowdhry, S., Hinz, J., Kamin, K. and Wanner, J. (2024). Brothers in arms: The value of coalitions in sanctions regimes. *Economic Policy*, 39(118), 471–512. [doi:10.1093/epolic/eiae019](https://doi.org/10.1093/epolic/eiae019)
- Eaton, J. and Kortum, S. (2002). Technology, Geography, and Trade. *Econometrica*, 70(5), 1741–1779.

## Licence

KITE is **dual-licensed**.

The default public licence is **GPL-3** (see the package `DESCRIPTION`).
You can use, modify, and redistribute KITE under the terms of GPL-3,
including for commercial work — any redistributed modifications must
also be released under GPL-3.

If you would like to use KITE under terms other than GPL-3 — for
example, to embed it in a closed-source product or to distribute a
modified version without releasing source — contact
[KITE@kielinstitut.de](mailto:KITE@kielinstitut.de) for a commercial
licence.

## Copyright

Copyright 2019–2026 KITE Development Team.
