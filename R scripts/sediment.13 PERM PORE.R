
# Calculate permeability based off of Matt Pace's thesis

#### Set up ####

rm(list=ls())

library(tidyverse)

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

mud_to_permeability <- function(percent_mud, scalar = -2.171, constant = -10.232) {
  
  permeability <- 10^((scalar*log10(percent_mud)) + constant)
  return(permeability)
}

D50_to_porosity <- function(D50, p1 = -0.435, p2 = 0.302, p3 = -1.035, p4 = -0.314) {
  
  complex <- 1+exp((-(log10(D50)-p3))/p4)
  
  porosity <- p1 + (p2*(1/(complex)))
  
  answer <- 10^porosity
#  porosity <- p1 + (p2*(1/(1+(exp(-log10(D50-p3))/p4))))
                      
                        return(answer)
}

check_por <- data.frame(D50 = seq(0, 1, length.out = 1000)) %>% 
  mutate(Porosity = D50_to_porosity(D50))

ggplot(check_por) + geom_line(aes(x = D50, y = Porosity)) + 
  scale_x_continuous(trans = "log", breaks = c(0.001,0.01,0.1,1))

check_perm <- data.frame(Silt = seq(0.5, 100, length.out = 100)) %>% 
  mutate(Permeability = mud_to_permeability(Silt))

ggplot(check_perm) + geom_line(aes(x = Silt, y = Permeability)) +
scale_y_continuous(trans = "log", breaks = c(0.000000000000001, 0.0000000000001, 0.00000000001)) 

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
