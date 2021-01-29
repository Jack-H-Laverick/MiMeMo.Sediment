
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

library(tidyverse)

scripts <- c(                                            # List scripts in the order you want to run them
#  "./R scripts/sediment.00 DOMAINS.R",
  # "./R scripts/sediment.01 GRID WATER.R",
  # "./R scripts/sediment.02 CROP SINMOD.R", 
  # "./R scripts/sediment.03 CROP WAVES.R",
  # "./R scripts/sediment.04 WATER TS.R",
  # "./R scripts/sediment.05 INTERPOLATE WAVES.R",
  # "./R scripts/sediment.06 EXPAND.R",
  # "./R scripts/sediment.07 SHEARSTRESS.R",
  # "./R scripts/sediment.08 RF PREDICTORS.R",
  # "./R scripts/sediment.09 H2O.R",
  # "./R scripts/sediment.10 SEDIMENT FRACTIONS.R",
  # "./R scripts/sediment.11 WRITE CSV.R",
  # "./R scripts/sediment.12 OMC.R",
  # "./R scripts/sediment.13 PERM PORE.R",
  # "./R scripts/sediment.14 WRITE NETCDF.R",
  # "./R scripts/sediment.15 SEASONAL STRESS.R",
  "./R scripts/sediment.16 DAILY DISTURBANCE.R",
  "./R scripts/sediment.17 RASTERISE DISTURBANCE.R"
  #"./R scripts/sediment.18 SEASONLA DISTURBANCE.R",
  #"./R scripts/sediment.19 ZONAL SUMMARIES.R"
) %>% 
  map(MiMeMo.tools::execute)
