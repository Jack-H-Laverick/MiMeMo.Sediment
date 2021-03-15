
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "stars", "raster", "tictoc", "sf", "data.table", "furrr")        # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

plan(multisession)
source("./R scripts/@_Set up file.R")

#### Import bathymetry ####

raster <- raster("../Barents Sea/Data/GEBCO_2019.nc")      # Import bathymetry

crop <- as(extent(-80, 90, 60, 85), "SpatialPolygons")     # Create cropping window for land
crs(crop) <- crs(raster)                                   # Match crs to bathymetry

raster <- crop(raster, crop)                               # Crop bathymetry

raster[raster < -500] <- NA                                # Blank out points deeper than 500 m
raster[raster > 0] <- 0                                    # Level all land to 0 m for easy selection 

#### Calculate terrain ####

crop <- as(extent(W, E, S, N), "SpatialPolygons")     # Shrink cropping window for calculations
crs(crop) <- crs(raster)                                   # Match crs to bathymetry

raster <- crop(raster, crop)                               # Crop bathymetry

# terrain <- terrain(raster, unit = "degrees", c("slope", "TPI", "TRI", "roughness")) # Calculate bathymetric variables
# terrain[raster == 0] <- NA                                                          # Drop points on land
# 
# plot(terrain)

#### Get an SF polygon for the interesting area ####

area <- raster
area[area < 0] <- 10                                # Level all land to 10 m for easy selection 
area[area == 0] <- NA                               # Match values below sea level so SF objects merge into a polygon

BS <- st_as_stars(area) %>%                         # If the result is large, returns a stars_proxy object
  .[Barents_mask] %>%                               # So clip
  st_as_stars() %>%                                 # And convert to a stars object in memory
  st_as_sf(merge = TRUE) %>%                        # So we can merge into polygons 
  st_union() %>%                                    # And stitch as a single SF object
  st_as_sf(Region = "Barents Sea")                  # Create a full sf object

GL <- st_as_stars(area) %>%                         # If the result is large, returns a stars_proxy object
  .[Greenland_mask] %>%                             # So clip
  st_as_stars() %>%                                 # And convert to a stars object in memory
  st_as_sf(merge = TRUE) %>%                        # So we can merge into polygons 
  st_union() %>%                                    # And stitch as a single SF object
  st_as_sf(Region = "Greenland Sea")                # create a full sf object

domain <- bind_rows(st_transform(BS, crs = 3035), 
                    st_transform(GL, crs = 3035)) 

plot(domain)
saveRDS(domain, "./Objects/Domain.rds")                                        # Import mapped areas for plotting

#### Downsample grid to 0.01 x 0.01 ####

cells <- raster(resolution = 0.01)
crs(cells) <- crs(crop)                                   # Match crs to bathymetry
cells <- crop(cells, crop)
cells$value <- 10

cells <- st_as_stars(cells) %>%
  st_transform(crs = 3035) %>%
  .[domain]

# cells_sf <- st_as_sf(cells, points = FALSE, merge = FALSE)
# 
# saveRDS(cells_sf, "./Objects/Extracters.rds")                     # Import cells for zoom to show domain boundary
# plot(cells)
# 
# #ggplot() + geom_sf(data = land) + geom_sf(data = cells, fill = NA, colour = "orange", lwd = 0.01)
# 
# ## Shrink grid for speed ##
# 
# raster[raster == 0] <- NA                                    # Blank out land 
# 
# terrain <- addLayer(raster, terrain) 
# 
# #### Sample new grid ####
# 
# tic("Numbers no meta-data")   # 1.87 hours # 2.4 for non-aggregated grid
# extracted_quick3 <- exactextractr::exact_extract(terrain, cells_sf) %>% 
#   rbindlist(idcol = "cell_sf") %>% 
#   .[,list(Depth= weighted.mean(layer, coverage_fraction, na.rm = TRUE),
#           Slope= weighted.mean(layer.1, coverage_fraction, na.rm = TRUE),
#           TPI= weighted.mean(layer.2, coverage_fraction, na.rm = TRUE),
#           TRI= weighted.mean(layer.3, coverage_fraction, na.rm = TRUE),
#           Roughness= weighted.mean(layer.4, coverage_fraction, na.rm = TRUE)),
#     keyby = cell_sf] %>% 
#   data.frame(cells_sf) %>% 
#   dplyr::select(-value) %>% 
#   st_sf(geometry = .$geometry)
# toc()  
# 
# saveRDS(extracted_quick3, "./Objects/Bathymetry.rds")
# 
# ggplot(extracted_quick3) + geom_sf(aes(fill = Depth), colour = NA)

point <- data.frame(x= 25, y = 75, region = "inset") %>%          # Choose a point to zoom to for the map showing the model domain
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  st_transform(crs = 3035)

inset <- cells[st_buffer(point, 3000)] %>%                        # Cut the grid within 3km of the point
  st_as_sf(as_points = FALSE, merge = FALSE) %>%
  st_intersection(st_buffer(point, 2000))                         # Cut out a circle with a 2km radius to get a perfect cirlce
saveRDS(inset, "./Objects/inset.rds")

#ggplot() + 
#  geom_sf(data = circle, fill = "lightblue2") + 
#  geom_sf(data = inset, fill = "lightblue3") +
#  theme_void()

#ggplot() + geom_sf(data = land, fill = "black") + geom_sf(data = targets, colour = "orange")
