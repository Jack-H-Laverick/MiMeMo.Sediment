
##**## Make maps of sediment fractions and D50

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster")                                    # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

raster <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Dxbar") %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(label = "'Mean sediment diameter (D'*bar(x)*')'")

#### Plot ####

ggplot(raster) + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = raster, aes(x=x, y=y, fill = Mean.grain.size)) +                      # Add raster
  viridis::scale_fill_viridis(name = "Grain size", option = "E", trans = "log",
                              breaks = c(0.01, 0.1, 1, 10),
                              labels = c("0.01mm", "0.1mm", "1mm", "10mm")) +    # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(label), labeller = label_parsed) +
  NULL

ggsave("./Figures/Figure 4.png", width = 13, height = 7, units = "cm", dpi = 700)
