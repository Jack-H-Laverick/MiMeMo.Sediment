
##**## Make maps of sediment fractions and D50

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster")                                    # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

raster <- stack(#raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Hard"),
                raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Gravel"),
                raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Sand"),
                raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", var = "Silt")) %>% 
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  rename(#Hard = "Areas.of.solid.substrate",      
         Gravel = "Percent.sediment.composition.gravel",
         Sand = "Percent.sediment.composition.sand",
         Silt = "Percent.sediment.composition.silt") %>% 
  pivot_longer(Gravel:Silt, names_to = "Bottom", values_to = "Share") %>% 
  mutate(Share = case_when(Share > 100 ~ 100,                                 # Reprojecting introduces im possible values (doesn't affect our data product)
                           Share < 0 ~ 0,                                     # So for the graph, overwrite the tiny number of over 100 and under 0
                           T ~ Share))

#### Plot ####

ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = raster, aes(x=x, y=y, fill = Share)) +                                # Add rasters
  viridis::scale_fill_viridis(name = "Sediment composition by weight", option = "E",
                              breaks = seq(0,100, by= 25),
                              labels = paste0(seq(0,100, by= 25), "%")) + # Specify fill
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(Bottom), ncol = 1) +                                                     # Facet by sediment fraction
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
                                title.hjust = 0.5, title.position = "top"))+
NULL

ggsave("./Figures/Figure 5.png", width = 10, height = 14, units = "cm", dpi = 700)
