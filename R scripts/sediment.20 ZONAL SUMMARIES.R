
## Calculate summary statistics for each mapped region to report in the results section of the manuscript

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "raster", "exactextractr", "ncdf4", "sf")        # List handy data packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

domains <- readRDS("./Objects/Domain.rds") %>%                              # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

#### Summaries for static layers ####

file <- "./Output/Greenland_and_barents_sea_shelf_sediments.nc"  

raw <- nc_open(file)                                                        # Open file
vars <- names(raw[["var"]]) %>%                                             # Get variable names
  .[-1]                                                                     # Drop crs as a variable
nc_close(raw)

mean <- map_df(vars, ~{                                                     # For each variable
  data.frame(Region = domains$Region,                                       # Create a dataframe containing the Regions
             Var = .x,                                                      # The variable name
             Value = raster(file, varname = .x) %>%                         
                  exact_extract(domains, fun = "mean"))                     # And the summary statistic
  }) %>% 
  pivot_wider(names_from =Var, values_from = Value)                         # Switch to wide for easier reading.
  
stdev <- map_df(vars, ~{
  data.frame(Region = domains$Region, 
             Var = .x,
             Value = raster(file, varname = .x) %>% 
               exact_extract(domains, fun = "stdev"))
}) %>% 
  pivot_wider(names_from =Var, values_from = Value)

#write.csv(mean, "means")
#write.csv(stdev, "devs")

#### Summaries for movements ####

file <- "./Output/Greenland_and_barents_sea_seasonal.nc"  

raw <- nc_open(file)                                                        # Open file
vars <- names(raw[["var"]]) %>%                                             # Get variable names
  .[-1]                                                                     # Drop crs as a variable
nc_close(raw)

mean <- map_df(vars, ~{                                                     # For each variable
  data.frame(Region = domains$Region,                                       # Create a dataframe containing the Regions
             Var = .x,                                                      # The variable name
             Value = brick(file, varname = .x) %>%                         
               exact_extract(domains, fun = "mean"))                        # And the summary statistic
}) %>% 
  pivot_longer(-c(Region, Var), names_to = "Month", values_to = "Value") %>%# Collect months into a column
  pivot_wider(names_from = Var, values_from = Value) %>%                    # Switch to wide for easier reading.
  separate(Month, into = c(NA, "Month"), sep = "X")                         # Clean month names

stdev <- map_df(vars, ~{
  data.frame(Region = domains$Region, 
             Var = .x,
             Value = brick(file, varname = .x) %>% 
               exact_extract(domains, fun = "stdev"))
}) %>% 
  
  pivot_longer(-c(Region, Var), names_to = "Month", values_to = "Value") %>%                 # Switch to wide for easier reading.
  pivot_wider(names_from = Var, values_from = Value) %>% 
  separate(Month, into = c(NA, "Month"), sep = "X")

#write.csv(mean, "seasonal_means")
#write.csv(stdev, "seasonal_devs")
