# KITE <img src="man/figures/logo_kite.png" align="right" width="140" alt="KITE logo" />

[![Version](https://img.shields.io/badge/version-26.09-blue)](NEWS.md)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue)](LICENSE)
[![R >= 3.5.0](https://img.shields.io/badge/R-%3E%3D%203.5.0-276DC3)](https://www.r-project.org/)

KITE is an open-source framework for quantitative trade-policy
counterfactuals, developed at the Kiel Institute for the World Economy.
It implements general-equilibrium trade models in the New Quantitative
Trade Model tradition and solves them in changes with exact hat algebra:
you supply a baseline and a policy scenario (tariffs, non-tariff
measures), and KITE returns the counterfactual changes in welfare,
production, and trade flows.

Website: [kite-model.org](https://kite-model.org) ·
Try it in the browser: [kite-model.org/lab](https://kite-model.org/lab/)

## Install

```r
# install.packages("remotes")
remotes::install_github("julianhinz/KITE")
```

Requires R >= 3.5.0. Imports `data.table` and `cli`.

## Quick start

The package ships a complete synthetic end-to-end example
(3 countries, 2 sectors, a 20% bilateral tariff war), which runs
both shipped models:

```r
library(KITE)

Sys.setenv(KITE_RUN_EXAMPLE = "1")
source(system.file("examples", "example.R", package = "KITE"))
```

Expected output:

```
CP2015 converged in 48 iterations (criterion = 9.523421e-05 ).
CHKW2022 converged in 53 iterations (criterion = 9.778594e-05 ).
```

The example builds initial conditions from scratch, defines a tariff
scenario, calls `update_equilibrium()`, and summarises the result with
`process_results()`. Open the file itself as a template for your own
scenarios.

## Models

| Function | Model | Reference |
|---|---|---|
| `caliendo_parro_2015` | Multi-sector Ricardian model with input–output linkages | Caliendo & Parro (2015), *Review of Economic Studies* |
| `chowdhry_hinz_kamin_wanner_2022` | Sanctions-coalition extension | Chowdhry, Hinz, Kamin & Wanner (2024), *Economic Policy* — function name keeps the 2022 working-paper vintage |

These are the 2 models currently shipped in this package; more are
coming. The full KITE suite behind the [website](https://kite-model.org)
comprises 13 models, available on request via
[kite@kielinstitut.de](mailto:kite@kielinstitut.de).

## How it works

KITE models are multi-sector Ricardian general-equilibrium models with
intra- and international input–output linkages, in the tradition of
Eaton & Kortum (2002) and Caliendo & Parro (2015). Instead of solving
levels, KITE solves in relative changes ("exact hat algebra"): observed
baseline shares and a small set of elasticities are sufficient to
compute the counterfactual equilibrium, with no need to estimate
technology levels or trade costs. `update_equilibrium()` iterates the
equilibrium conditions to convergence; `process_results()` turns the
solution into welfare, production, and trade-flow changes. The
[KITE white paper](https://trade.kielinstitut.de/KTTM/KITE_whitepaper.pdf)
describes the framework in detail.

## Data

Public examples in this package are synthetic. Calibrated real-world
baselines (trade and input–output data, elasticities) are not bundled;
they are available on request via
[kite@kielinstitut.de](mailto:kite@kielinstitut.de).

## Cite

Please cite the KITE white paper as the canonical reference, and the
package as a software entry. `citation("KITE")` returns both; the same
entries ship in BibTeX at `system.file("KITE.bib", package = "KITE")`:

```bibtex
@techreport{HinzMahlkowWanner2025,
  author      = {Hinz, Julian and Mahlkow, Hendrik and Wanner, Joschka},
  title       = {The {KITE} Model Suite: A Quantitative Framework for International Trade Analysis},
  institution = {Kiel Institute for the World Economy},
  type        = {KITE White Paper},
  year        = {2025},
  url         = {https://trade.kielinstitut.de/KTTM/KITE_whitepaper.pdf}
}
```

## License

GPL-3 (see [LICENSE](LICENSE)). Alternative or commercial licensing
terms are available on request via
[kite@kielinstitut.de](mailto:kite@kielinstitut.de).

## Contact

- Julian Hinz — [julian.hinz@kielinstitut.de](mailto:julian.hinz@kielinstitut.de)
- Hendrik Mahlkow — [hendrik.mahlkow@kielinstitut.de](mailto:hendrik.mahlkow@kielinstitut.de)
- Joschka Wanner — [joschka.wanner@kielinstitut.de](mailto:joschka.wanner@kielinstitut.de)

General inquiries: [kite@kielinstitut.de](mailto:kite@kielinstitut.de) ·
Issues: [github.com/julianhinz/KITE/issues](https://github.com/julianhinz/KITE/issues)
