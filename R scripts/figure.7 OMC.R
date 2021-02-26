
##**## Make map of Organic Matter Content

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "patchwork")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

#### Nitrogen ####

raster <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "TON") %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(label = "Organic nitrogen content")


N <- ggplot(raster) + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = raster, aes(x=x, y=y, 
              fill = Percent.of.sediment.as.organic.Nitrogen.by.weight)) +                      # Add raster
  viridis::scale_fill_viridis(name = "Sediment composition by weight", option = "E",
                              breaks = seq(0.05, 0.25, by = 0.05),
                              labels= paste0(seq(0.05, 0.25, by = 0.05), "%")) +           # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(label)) +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
                                title.hjust = 0.5, title.position = "top"))+
  NULL

#### Carbon ####

raster <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "TOC") %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(label = "Organic carbon content")


C <- ggplot(raster) + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = raster, aes(x=x, y=y, 
                                 fill = Percent.of.sediment.as.organic.Carbon.by.weight)) +                      # Add raster
  viridis::scale_fill_viridis(name = "Sediment composition by weight", option = "E",
                              breaks = seq(0.5, 2, by = 0.5),
                              labels= paste0(seq(0.5, 2, by = 0.5), "%")) +               # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(label)) +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
                                title.hjust = 0.5, title.position = "top"))+
  NULL

N/C

ggsave("./Figures/Figure 7 OMC.png", width = 13, height = 14, units = "cm", dpi = 1500)
