
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis", "tictoc")             # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth")                    # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages
source("./R scripts/@_Set up file.R")

domains <- readRDS("./Objects/Domain.rds") %>%                                # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

bathymetry <- readRDS("./Objects/Bathymetry.rds")

#ggplot(bathymetry) + geom_sf(aes(fill = Depth), colour = NA)

points <- st_centroid(bathymetry)

#### Sample sediment ####

with_sediment <- readOGR(dsn="./Data/NGU oversikt", 
                         layer = "KornstorrelseFlate_oversikt") %>%                # Import NGU shapefile
  st_as_sf(crs = 4326) %>%                                                    # Assign CRS                                  
  st_transform(crs = crs) %>%                                                 # Project for accurate GIS operations
  dplyr::select(Sed_class = SEDKORNSTR) %>% 
  st_join(x = points, y = ., left = TRUE) %>%                                              # Sample polygons by bathymetric grid
  distinct(.keep_all = TRUE) %>% 
  st_drop_geometry() %>% 
  left_join(bathymetry) %>% 
  st_sf(crs = crs) %>% 
  distinct(.keep_all = TRUE)

sed_plot <- ggplot(with_sediment) +
  geom_sf(aes(fill = Sed_class), colour = NA) +
  scale_fill_viridis(name = 'Sediment', na.value = "grey" ) +
  theme_minimal() +
  NULL

ggsave("./Figures/Classed sediment.png", plot = sed_plot, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Sample bed shear stress ####

with_stress <- st_join(points,                                              # Join previous data
#                  st_transform(readRDS("./Objects/Stress95_BS.rds"), crs)) %>%                    # to bed shear stress estimates
                    readRDS("./Objects/Stress95.rds")) %>%                    # to bed shear stress estimates
  distinct(.keep_all = TRUE) %>% 
  dplyr::select(-c(Depth.x, Depth.y, Cell_area, Longitude, Latitude)) %>%          # Drop redundant columns
  st_drop_geometry() %>% 
  left_join(with_sediment) %>% 
  st_sf(crs = crs) %>% 
  distinct(.keep_all = TRUE)

stress_plot <- ggplot(with_stress) + 
  geom_sf(aes(fill = Stress95), colour = NA)

ggsave("./Figures/Stress check.png", plot = stress_plot, scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

saveRDS(with_stress, "./Objects/RF_sediment_observations.rds")          
