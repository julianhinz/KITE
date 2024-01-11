###
# example
# 240101
###

library(KITE)
library(readr)
library(data.table)


# read initial conditions
initial_conditions = read_rds("inst/data/initial_conditions.rds")

# set model scenarios
tariff_war = copy(initial_conditions$tariff)
tariff_war[origin == "USA" & destination == "CHN", value := 1.2]
tariff_war[destination == "USA" & origin == "CHN", value := 1.2]

# compute
results_cp2015 = update_equilibrium(model = caliendo_parro_2015,
                                initial_conditions = initial_conditions,
                                model_scenario = list(tariff_new = tariff_war),
                                settings = list(verbose = 2L,
                                                tolerance = 1e-4))
results_cp2015 = process_results(results_cp2015)

results_chkw2022 = update_equilibrium(initial_conditions = initial_conditions,
                                      model_scenario = list(tariff_new = tariff_war,
                                                            coalition_member = c("usa", "can", "mex")),
                                      model = chowdhry_hinz_kamin_wanner_2022,
                                      settings = list(verbose = 2L,
                                                      vfactor = 0.1,
                                                      tolerance = 1e-4))
results_chkw2022 = process_results(results_chkw2022)
