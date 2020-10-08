
##**## Make a map of bed shear stress and natural disturbance rates

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "patchwork")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

domain <- readRDS("./Objects/Domain.rds")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

average <- brick("./Output/Greenland_and_barents_sea_seasonal_disturbance.nc") %>% # Open all layers
  exactextractr::exact_extract(st_union(domain), fun = "sum") %>%             # Total the disturbance for each layer over the whole model domain
  as.numeric()                                                                # Convert to numeric vector

layer <- which(average == max(average))                                       # Which layer has the most stress?

#### Stress ####

stress <- raster("./Output/Greenland_and_barents_sea_seasonal_stress.nc", layer = layer) %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(Var = "Bed shear stress")
names(stress) <- c("x", "y", "Stress", "Var")

S <- ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = stress, aes(x=x, y=y, fill = Stress)) +                   # Add rasters
  viridis::scale_fill_viridis(name = expression("Bed shear stress (Kg." ~ S^-1 ~ ")"), 
                              option = "B", trans = "log", 
                              breaks = c(0.0001, 0.001, 0.01, 0.1, 3)) +       # Specify fill
  sediment_aes +                                                               # Use consistent aesthetics
  facet_wrap(vars(Var)) +                                          # Facet by month
  NULL

#### Disturbance ####

disturbance <- raster("./Output/Greenland_and_barents_sea_seasonal_disturbance.nc", layer = layer) %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(Var = "Natural Disturbance")
names(disturbance) <- c("x", "y", "Disturbance", "Var")
  
#ggplot() +                                                                # Where could we limit the colour scale to make it more readable?
#  geom_freqpoly(data = disturbance, aes(Disturbance*100, colour = Month)) # Chose 10%

D <- ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = filter(disturbance,Disturbance > 0), aes(x=x, y=y, fill = Disturbance*100)) +  # Add rasters
  viridis::scale_fill_viridis(name = "Time disturbed (%)", breaks = c(2, 4, 6, 8, 10), 
                              limits = c(0, 10), labels = c("2", "3", "4", "8", "10+"),
                              option = "B",
                              rescaler = function(x, to = c(0,1), from = NULL) {   # Rescale values larger than 10 to limit colour scale
                                ifelse(x<10,
                                       scales::rescale(x,
                                                       to = to,
                                                       from = c(min(x, na.rm = TRUE), 10)),
                                       1)}) +                      # Limit max colour fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(Var)) +                                          # Facet by month
  NULL


combined <- S/D

ggsave("./Figures/Figure 7 movement.png", width = 13, height = 14, units = "cm", dpi = 1500)
