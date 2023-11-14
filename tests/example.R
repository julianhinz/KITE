###
# example
# 231114
###

library(KITE)
library(readr)
library(data.table)

# read initial conditions ----
initial_conditions_gtap10 = read_rds("data/GTAP_10/initial_conditions.rds")

# set model scenarios
tariff_war = copy(initial_conditions_gtap10$tariff)
tariff_war[origin == "USA" & destination == "CHN", value := 1.2]
tariff_war[destination == "USA" & origin == "CHN", value := 1.2]

# run simulation
results = update_equilibrium(initial_conditions = initial_conditions_gtap10,
                             tariff_new = tariff_war,
                             verbose = 2,
                             tolerance = 1e-4)
