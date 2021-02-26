
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
  viridis::scale_fill_viridis(name = "Bed shear stress", 
                              option = "E", trans = "log", 
                              breaks = c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 3),
                              labels = c(expression("1" ~ e^-6),
                                         expression("1" ~ e^-5),
                                         expression("1" ~ e^-4),
                                         expression("1" ~ e^-3),
                                         expression("1" ~ e^-2),
                                         expression("1" ~ e^-1),
                                         expression("1Kg." ~ S^-1))) +         # Specify fill
  sediment_aes +                                                               # Use consistent aesthetics
  facet_wrap(vars(Month), ncol = 2) +                                          # Facet by month
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
                                title.hjust = 0.5, title.position = "top"))+
  NULL

ggsave("./Figures/Figure supp stress.png", width = 13, height = 20, units = "cm", dpi = 1500)

#### Disturbance ####

disturbance <- brick("./Output/Greenland_and_barents_sea_seasonal_disturbance.nc") %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  pivot_longer(X1:X12, names_to = "Month", values_to = "Disturbance") %>% 
  filter(Disturbance > 0 ) %>%                                           # Drop areas with no disturbance to help the colour scale 
  mutate(Month = month.name[as.numeric(str_sub(Month, start = 2))])
disturbance$Month <- factor(disturbance$Month, levels = month.name)                      # Set facet order

# ggplot() +                                                                # Where could we limit the colour scale to make it more readable?
#   geom_freqpoly(data = disturbance, aes(Disturbance*100, colour = Month)) # Chose 10%

ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = disturbance, aes(x=x, y=y, fill = Disturbance*100)) +                    # Add rasters
  viridis::scale_fill_viridis(name = "Time disturbed", breaks = c(0, 1, 2, 3), 
                              limits = c(0, 3), labels = c("0%", "1%", "2%", "3%+"),
                              #                              option = "B",
                              option = "E",
                              rescaler = function(x, to = c(0,1), from = NULL) {   # Rescale values larger than 3 to limit colour scale
                                ifelse(x<3,
                                       scales::rescale(x,
                                                       to = to,
                                                       from = c(min(x, na.rm = TRUE), 3)),
                                       1)}) +                      # Limit max colour fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(Month), ncol = 2) +                                                      # Facet by month
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
                                title.hjust = 0.5, title.position = "top"))+
  NULL

ggsave("./Figures/Figure supp disturbance.png", width = 13, height = 20, units = "cm", dpi = 1500)
