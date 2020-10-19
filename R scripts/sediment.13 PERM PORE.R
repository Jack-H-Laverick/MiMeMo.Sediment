
# Calculate permeability based off of Matt Pace's thesis

#### Set up ####

rm(list=ls())

library(MiMeMo.tools)

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

#### Predict for the sediment map ####

csv <- csv %>%
  mutate(Permeability = mud_to_permeability(Silt),                                 # Predict permeability
         Porosity = D50_to_porosity(Dxbar)) %>%                                    # Predict porosity
  mutate(Permeability = ifelse(Silt == 0, mud_to_permeability(0.5), Permeability)) # Overwrite Inifinties with small nominal value

data.table::fwrite(csv, "./Output/Greenland_and_barents_sea_shelf_sediments.csv")  # Save csv appendix

#### Also add to object destined to become netcdf file ####

readRDS("./Objects/Everything.rds") %>% 
  mutate(Permeability = mud_to_permeability(Silt),                                 # Predict permeability
         Porosity = D50_to_porosity(Dxbar)) %>%                                    # Predict porosity
  mutate(Permeability = ifelse(Silt == 0, mud_to_permeability(0.5), Permeability)) %>% # Overwrite Inifinties with small nominal value
  saveRDS("./Objects/Everything.rds")                                              # Save with geometry for making netcdf later 
