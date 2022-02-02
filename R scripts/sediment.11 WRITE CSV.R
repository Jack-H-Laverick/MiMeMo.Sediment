
# Combine project data files and write to a csv file

#### Set up ####

rm(list=ls())

library("tidyverse")
library("sf")       

#### Import scattered data ####

Predictors <- readRDS("./Objects/RF_sediment_observations.rds") %>% # Read in data
  drop_na() %>%                                                  # Drop point without all estimates
  dplyr::select(-c(Sed_class, Region)) %>%                       # Drop duplicated sediment values
  MiMeMo.tools::sfc_as_cols() %>%                                # Get x and y coordinates for fast join
  st_drop_geometry()                                             # Drop geometry to speed up join

To_predict <- readRDS("./Objects/RF_sediment_observations.rds") %>% # Read in data
  filter(is.na(Sed_class)) %>%                                   # Limit to points we don't know about sediment
  dplyr::select(-c(Sed_class, Region)) %>%                       # Drop the sediment columns so we can drop NAs
  drop_na() %>%                                                  # Keep only points where we have all the predictors
  MiMeMo.tools::sfc_as_cols() %>%                                # Get x and y coordinates for fast join
  st_drop_geometry() %>%                                         # Drop geometry to speed up join
  rbind(Predictors)                                              # Combine with model training data
  
Predictions <- readRDS("./Objects/Full sediment.rds") %>% 
  dplyr::select(-Sed_class)

#### Join data ####

Everything <- left_join(To_predict, Predictions, by = c("x", "y")) %>% # Join by pixel
  select(-Stress95) %>% 
  rename(Longitude = "x", Latitude = "y", TRI = "tri",                 # Rename variables 
         TPI = "tpi", Roughness = "roughness", Slope = "slope",
         Dxbar = "D50", Rock = "Hard") %>% 
  st_sf()                                                              # Reinstate SF class

apply(Everything, MARGIN = 2, FUN = anyNA)   # Check for NAs, Dxbar has them for Rock pixels

#### Save out ####

saveRDS(Everything, "./Objects/Everything.rds")                                 # Save with geometry for making netcdf later 

data.table::fwrite(st_drop_geometry(Everything), 
                   "./Output/Greenland_and_barents_sea_shelf_sediments.csv")     # Save csv appendix
