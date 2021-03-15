
## could speed up the rasterise calls by using st_intersects

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "viridis", "tictoc")             # List handy data packages
Geo_packages <- c("sf", "rgdal", "stars", "rnaturalearth", "raster")           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages

domains <- readRDS("./Objects/Domain.rds") %>%                               # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                    # Transform to Lat/Lon to match other objects

raster <- raster("../Barents Sea/Data/GEBCO_2019.nc")      # Import bathymetry

crop <- as(extent(-80, 90, 60, 85), "SpatialPolygons")     # Create cropping window for land
crs(crop) <- crs(raster)                                   # Match crs to bathymetry

nc_bath <- crop(raster, crop) %>%                                # Crop bathymetry
  st_as_stars()
st_crs(nc_bath) <- st_crs(4326)                                               # set lat-lon crs

world <- ne_countries(scale = "medium", returnclass = "sf")                   # Get a world map

box <- st_bbox(domains) 

star <- st_as_stars(box, dx = 0.01, dy = 0.01, values = 0)

samples <- st_as_sf(star, as_points = TRUE, merge = FALSE) %>%                # Switch the GFW grid into a collection of sampling points
  st_transform(crs = 4326)                                                    # Set CRS

####  Regrid GEBCO ####

star_extract <- function(x, y, fun = NULL, na.rm = FALSE) {
  x = as(x, "Raster")
  y = as(y, "Spatial")
  raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)
}
 
bathymetry <- samples %>%                                                     # Start with the GFW grid
  mutate(elevation = as.numeric(star_extract(nc_bath, ., fun = mean))) %>%                # Sample the Bathymetry at GFW
dplyr::select(-values)                                                      # Drop unneccessary column

#### Compute terrain variables ####

star_terrain <- function (x, y, opt) {
  x = as(x, "Raster")
  x = raster::terrain(x = x,  unit = "degrees", opt)
  y = as(y, "Spatial")
  
  terrain <- raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)

  return(terrain)
}                                    # Wrapper for using terrain function from raster package

bath_star <- st_rasterize(bathymetry, deltax = 0.01, deltay = 0.01)
plot(bath_star)                                                               # Plot as a check, ggplot can't cope with the full file

tic()
Terrain <- star_terrain(x = bath_star,                                        # From rasterised bathymetry 
                        y = bathymetry,                                       # Using samples (which also have bathymetry information appended)
                        c("slope", "TPI", "TRI", "roughness")) %>%            # Calculate variables in raster::terrain function
  data.frame(bathymetry) %>%                                                  # Bind into a dataframe and add bathymetry
  filter(between(elevation, -500, 0)) %>%                                     # Drop pixels outside our depth range 
  mutate(elevation = abs(elevation)) %>%                                      # Convert elevation to depth
  st_as_sf()                                                                  # Reinstate SF class
toc()

#### Sample sediment ####

Sediment <- readOGR(dsn="./Data/NGU oversikt", 
                    layer = "KornstorrelseFlate_oversikt") %>%                # Import NGU shapefile
  st_as_sf(crs = 4326) %>%                                                    # Assign CRS                                   
  dplyr::select(Sed_class = SEDKORNSTR) %>% 
  st_join(Terrain, ., left = TRUE)                                            # Sample polygons by bathymetric grid
  
Sed_class_star <- st_rasterize(Sediment["Sed_class"], deltax = 0.01, deltay = 0.01) # Extract bottom classification

ggplot() +
  geom_stars(data = Sed_class_star["Sed_class"]) +
  scale_fill_viridis(name = 'Sediment', na.value = "red") +
  geom_sf(data = world, fill = "black") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  NULL

ggsave("./Figures/Classed sediment.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

#### Sample be shear stress ####

Stress <- st_join(Sediment,                                                   # Join previous data
                 st_transform(readRDS("./Objects/Stress95.rds"), crs = 4326)) 

ggplot(Stress, aes(x = Longitude, y = Latitude, fill = Sed_class)) +          # Check the bathymetry looks believable
  geom_raster() +
  scale_fill_viridis(name = 'Sediment', na.value = "red")

shear_star <- st_rasterize(Stress["Stress95"], dx = 0.01, dy = 0.01) # Extract bottom classification

ggplot() +
  geom_stars(data = shear_star["Stress95"]) +
  scale_fill_viridis(name = 'Stress', na.value = "red") +
  geom_sf(data = world, fill = "black") +
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  NULL

Stress <- dplyr::select(Stress, -c(Depth, Cell_area, Longitude, Latitude)) %>%  # Drop excess columns
  rename(Depth = elevation)

saveRDS(Stress, "./Objects/RF_sediment_observations.rds")          
