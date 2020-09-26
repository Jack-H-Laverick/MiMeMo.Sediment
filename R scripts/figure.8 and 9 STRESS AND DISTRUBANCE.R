
##**## Make a map of bed shear stress and natural disturbance rates

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "patchwork")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

#### Stress ####

stress <- brick("./Output/Greenland_and_barents_sea_seasonal_stress.nc") %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  pivot_longer(X1:X12, names_to = "Month", values_to = "Stress") %>% 
  mutate(Month = month.name[as.numeric(str_sub(Month, start = 2))])
stress$Month <- factor(stress$Month, levels = month.name)                      # Set facet order

ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = stress, aes(x=x, y=y, fill = Stress)) +                   # Add rasters
  viridis::scale_fill_viridis(name = expression("Bed shear stress (Kg." ~ S^-1 ~ ")"), 
                              option = "B", trans = "log", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 3)) +       # Specify fill
  sediment_aes +                                                               # Use consistent aesthetics
  facet_wrap(vars(Month), ncol = 2) +                                          # Facet by month
  NULL

ggsave("./Figures/Figure 7 stress.png", width = 13, height = 20, units = "cm", dpi = 1500)

#### Disturbance ####

disturbance <- brick("./Output/Greenland_and_barents_sea_seasonal_disturbance.nc") %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  pivot_longer(X1:X12, names_to = "Month", values_to = "Disturbance") %>% 
  mutate(Month = month.name[as.numeric(str_sub(Month, start = 2))])
disturbance$Month <- factor(disturbance$Month, levels = month.name)                      # Set facet order

ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = disturbance, aes(x=x, y=y, fill = Disturbance*100)) +                    # Add rasters
  viridis::scale_fill_viridis(name = "Time disturbed (%)", trans = "sqrt", option = "B") +                      # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(Month), ncol = 2) +                                                      # Facet by month
  NULL

ggsave("./Figures/Figure 8 disturbance.png", width = 13, height = 20, units = "cm", dpi = 1500)
