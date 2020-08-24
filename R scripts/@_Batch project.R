
## Run batches of R scripts. Handy if you want scripts to run after another finishes while you're away from the machine

#### Set up ####

packages <- c("tidyverse", "callr", "tictoc")           # List packages
lapply(packages, library, character.only = TRUE)        # Load packages

execute <- function(x) {
  tic(x)
  r(function(x) source(x), args = list(x), spinner = TRUE)
  toc(log = T, quiet = T)

  usethis::ui_done("{usethis::ui_field(x)} completed. {praise::praise('${Exclamation}!')}")  } # Run an R script in it's own R session and record how long it takes

#### Batch process scripts ####

scripts <- c(                                            # List scripts in the order you want to run them
   #"./R scripts/sediment.1 GRID WATER.R",
   #"./R scripts/sediment.2 CROP SINMOD.R", 
   #"./R scripts/sediment.3 CROP WAVES.R",
   #"./R scripts/sediment.4 WATER TS.R",
   "./R scripts/sediment.5 SHEARSTRESS.R",
   "./R scripts/sediment.6 RF PREDICTORS.R",
   "./R scripts/sediment.7 H2O.R"
  # "./R scripts/sediment.8 HABITAT TYPES.R",
  # "./R scripts/sediment.9 DISTURBANCE.R"
) %>% 
  map(execute)

#### Plot run times ####

timings <- tic.log(format = F) %>%                                                     # Get the log of timings
  lapply(function(x) data.frame("Script" = x$msg, Minutes = (x$toc - x$tic)/60)) %>%   # Get a dataframe of scripts and runtimes in minutes
  bind_rows() %>%                                                                      # Get a single dataframe
  separate(Script, into = c(NA, "Script"), sep = "/R scripts/") %>% 
  separate(Script, into = c("Type", NA, NA), sep = "[.]", remove = F) %>% 
  mutate(Script = factor(Script, levels = Script[order(rownames(.), decreasing = T)])) # Order the scripts
saveRDS(timings, "./Objects/Run time.rds")

source("./R scripts/@_Script runtimes.R")                                              # Plot run times

   