
## Calculate summary statistics for each ampped region to report in the results section of the manuscript

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "raster", "exactextractr", "ncdf4", "sf")        # List handy data packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

domains <- readRDS("./Objects/Domain.rds") %>%                              # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

file <- "./Output/Greenland_and_barents_sea_shelf_sediments.nc"  

raw <- nc_open(file)                                                        # Open file
vars <- names(raw[["var"]]) %>%                                             # Get variable names
  .[-1]                                                                     # Drop crs as a vaiable
nc_close(raw)

#### Summaries ####

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


