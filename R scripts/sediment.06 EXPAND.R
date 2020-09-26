
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "tictoc", "furrr", "sf")         # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

plan(multiprocess)

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels
pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 200)               # Split into chunks of 1000 rows
chunks <- length(pairs_chunked)

wave_ts <- readRDS("./Objects/Wave_ts.rds")                                 # Read in a list of time series for waves at pixels
print("loaded waves")
tide_ts <- readRDS("./Objects/Tide_ts.rds")                                 # Read in a list of time series for tides at pixles
print("loaded tides")

#### Save out chunks ####

tic()
imap(pairs_chunked, ~{ waves <- wave_ts[.x$wave_entry]  
                       tides <- tide_ts[.x$tide_entry]
                       map2(waves, tides, cbind) %>%  
  saveRDS(glue::glue("./Objects/stress_input/{.y}.rds"))                  # Then save (allows all the data to be held in memory)
  print(glue::glue("{.y}/{chunks} chunks"))})
toc() 

rm(wave_ts, tide_ts)
