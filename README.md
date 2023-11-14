# KITE — Kiel Institute Trade Policy Evaluation <img src="man/figures/logo_kite.png" align="right" width="200" height="240"/>
***

The KITE model provides a tool for simulating and estimating various types of (trade) policy changes. The underlying model uses a computable general equilibrium (CGE) framework of the type that is commonly described as **"New Quantitative Trade Model"**. The model has originally been developed by [Caliendo & Parro (2015)](#1). They built a multi-sector version of the Ricardian trade model of [Eaton & Kortum (2002)](#2), where countries produce and sell domestically as well as internationally according to their relative copmparative advantage. The model extends this framework by allowing for extensive intra- and international input-output linkages where goods and services may enter as both final and intermediate goods. Trade policy is conducted through the tightening or easing of trade barriers in form of tariffs and non-tariff measures.

By now the model has been extensively used to evaluate (free) trade agreements (e.g. NAFTA, TTIP) and trade disputes (i.e. US-China trade war, Airbus-Boeing case) and economic sanction regimes. It derives the economic consequences for production, value added and welfare based on pre-defined scenarios that specify the policies to be evaluated. It allows various types of data sources that can be used in constructing the underlying model variables and/or parameters.

The model is constantly updated to improve efficiency and/or extend the underlying framework. Current projects include the implementation of CO2 footage in production or the introduction of the factor land in the production function.

## Copyright

Copyright 2019-2023 Kiel Institute & Austrian Institute of Economic Research, KITE Development Team

## Developers

* Julian Hinz, Kiel Institute, Mail: [julian.hinz@ifw-kiel.de](mailto:julian.hinz@ifw-kiel.de)
  
* Hendrik Mahlkow, Austrian Institute of Economic Research, Mail: [hendrik.mahlkow@wifo.ac.at](mailto:hendrik.mahlkow@wifo.ac.at)
  
* Joschka Wanner, Kiel Institute, Mail: [hendrik.mahlkow@ifw-kiel.de](mailto:joschka.wanner@ifw-kiel.de)

## Installing KITE

KITE is a [R](#3) package. To install this package, download the current beta version and install using

```{R}
install.packages("~/path/to/KITE_23.11.tar.gz", repos = NULL, type = "source")
```

## Tutorial

The package comes with a vignette describing different trade policy scenarios. You can load it with the following command (the package needs to be installed):

```r
vignette("KITE") # Trade policy scenarios with KITE
```

## Usage

Running the model requires the user to follow 4 basic steps:

1. Choose an input-output table.
2. Define scenarios that are supposed to be simulated by the model.
3. Feed the data from 1) and 2) into the model and run.
4. Process the results.


The script `example.R` provides a step-by-step algorithm in performing a scenario analysis with KITE. It sources from different scripts and functions that are embedded in the KITE package and will be explained further below. Please use the script for all your calculations.

### Step 1: Choose Input-Output Table

Load the I-O tables from a folder `"input/../initial_conditions.rds"`

```{R}
initial_conditions = read_rds("input/../initial_conditions.rds")
```

### Step 2: Set Model Scenarios

Define and set the scenarios for the model run by specifying the origin and destination countries concerned, as well as the counterfactual policy that is supposed to represent the given policy scenario. 

**Counterfactual Tariff**
```{R}
tariff_war = copy(initial_conditions$tariff)

# US tariffs on Chinese goods and services
tariff_war[origin == "usa" & destination == "chn", value := 1.2]

# Chinese tariffs on US goods and services
tariff_war[destination == "usa" & origin == "chn", value := 1.2]

```

In this example KITE simulates the *US-China trade war*, assuming that both countries simultaneously set up a tariff of 20% across all sectors. "value" specifies the tariff as value = 1 + tariff/100.

The user may specify other policies that are supposed to enter the counterfactual situation, such as non tariff barriers or export subsidies. The procedure works exactly the same as in the case of a tariff change. For more detailed information on the options for policy changes please proceed to Step 3. For more examples see `vignette("KITE")`. 


### Step 3: Run the Model

The function calculates a new equilibrium based on the policy changes the user feeds into the model. The corresponding results are expressed in exact hat algebra, i.e., in discrete changes relating the base-line equilibrium and the counterfactual equilibrium (scenario). 

The function rests on three building blocks, two of which need to be specified.

1. `initial conditions = initial_conditions`

Nothing to specify here

2. `tariff_new = tariff_war`, etc.

Here, the user needs to specify all the variables that represent the desired counterfactual/scenario situation. Please note that these variables need to be defined in "Step 2: Set Model Scenarios", above. Options to specify the counterfactual situation include tariffs (`tariff_new`) and non-tariff barriers (`ntb_change`). The user may even want to recalculate the baseline equilibrium to change the situation a given counterfactual/scenario is supposed to be compared to. Options include tariffs, non tariff barriers, consumption share, factor share, intermediate share, trade elasticity, trade share, value added, trade balance.

3. `verbose = 2` and `tolerance = 1e-4`

Set verbosity levels between 0 and 2 to get more or less detailed output while the model converges. The tolerance level determines the stopping point, 1e-6 being the default and lower numbers leading to more precise results.

The full command should look something like this

```{R}
results = update_equilibrium(initial_conditions = initial_conditions,
                             tariff_new = tariff_war,
                             verbose = 2,
                             tolerance = 1e-4)
```

## References

<i id="#1">Caliendo, Lorenzo, and Fernando Parro.</i> 2015. “Estimates of the Trade and Welfare Effects of NAFTA.” The Review of Economic Studies 82 (1): 1–44.

<i id="#2">Eaton, Jonathan, and Samuel Kortum. 2002.</i> “Technology, Geography, and Trade.” Econometrica 70 (5): 1741–79.

<i id="#3">R Core Team. 2020.</i> "R: A language and environment for statistical computing." R Foundation for Statistical Computing, Vienna, Austria.  https://www.R-project.org/
