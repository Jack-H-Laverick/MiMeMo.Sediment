
##**## Make a map of bed shear stress and natural disturbance rates

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "patchwork")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

land <- readRDS("./Objects/Land.rds") %>% st_transform(crs = 3035)            # Import land for plotting

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

#### Stress ####

stress <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Stress") %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(label = "Bed shear stress")

map <- ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = stress, aes(x=x, y=y, fill = Bed.shear.stress)) +                     # Add rasters
  viridis::scale_fill_viridis(name = expression("(Kg." ~ S^-1 ~ ")"), 
                              option = "E", trans = "log", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 3)) +                # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(label)) +                                                                # Hacking a single panel label
  NULL
map

#### Disturbance ####

disturbance <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Stress") %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(label = "Natural disturbance")

map2 <- ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = disturbance, aes(x=x, y=y, fill = Bed.shear.stress)) +                # Add rasters
  viridis::scale_fill_viridis(name = "(%)", option = "A") +                                # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(label)) +                                                                # Hacking a single panel label
  NULL
map2

#### Combine ####

map / map2                                                                                 # Combine the plots

ggsave("./Figures/Figure 7.png", width = 13, height = 14, units = "cm", dpi = 1500)

  