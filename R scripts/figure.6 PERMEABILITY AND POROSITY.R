
##**## Make a map of Permeability and Porosity

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "patchwork")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

domain <- readRDS("./Objects/Domain.rds")

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

#### Porosity ####

Porosity <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "Porosity") %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(Var = "Porosity")

Por <- ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = Porosity, aes(x=x, y=y, fill = Porosity)) +               # Add rasters
  viridis::scale_fill_viridis(name = NULL, option = "E") +        # Specify fill
  sediment_aes +                                                               # Use consistent aesthetics
  facet_wrap(vars(Var)) +                                          # Facet by month
 # guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
#                                title.hjust = 0.5, title.position = "top"))+
  NULL

#### Permeability ####

Permeability <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc", varname = "Permeability") %>%
  projectRaster(crs = proj$prj4) %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  mutate(Var = "Permeability")


Per <- 
ggplot() + 
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_path(data = marks, aes(x=x, y=y, group = graticule), colour = "grey", size = 0.2) + # Add Graticules 
  geom_raster(data = Permeability, aes(x=x, y=y, fill = Permeability)) +  # Add rasters
  viridis::scale_fill_viridis(name = NULL, breaks = c(0.000000000000001,0.00000000000001,0.0000000000001,0.000000000001, 0.00000000001, 0.0000000001), 
                              labels = c(expression("1" ~ e^-14),
                                         expression("1" ~ e^-13),
                                         expression("1" ~ e^-12),
                                         expression("1" ~ e^-11),
                                         expression("1" ~ e^-10),
                                         expression("1" ~ e^-9 ~ m^2)),
                              option = "E", trans = "log")+
  sediment_aes +                                                                           # Use consistent aesthetics
  facet_wrap(vars(Var)) +                                          # Facet by month
#  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1, 
#                                title.hjust = 0.5, title.position = "top"))+
  NULL


Por/Per

ggsave("./Figures/Figure 6 porosity permeability.png", width = 13, height = 14, units = "cm", dpi = 1500)
