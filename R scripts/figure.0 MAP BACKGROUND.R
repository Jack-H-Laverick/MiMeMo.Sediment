
##**## hillshading

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "rayshader")                       # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

raster <- raster("../Barents Sea/Data/GEBCO_2019.nc")                         # Import bathymetry

crop <- as(extent(-100, 100, 40, 89), "SpatialPolygons")                      # Create cropping window for land
crs(crop) <- crs(raster)                                                      # Match crs to bathymetry

raster <- crop(raster, crop)                                                  # Crop bathymetry

proj <- rgdal::make_EPSG() %>%                                                # Get proj4 strings from epsg codes
  filter(code == crs)

target <- raster("./Output/Greenland_and_barents_sea_shelf_sediments.nc",     # Use the our data products as a template 
                 var = "Rock") %>% 
  projectRaster(crs = proj$prj4) %>%                                          # Reproject to project projection 
  as.data.frame(xy = T) %>%                                                   # Convert to a data frame
  drop_na() %>%                                                               # Drop NAs
  rasterFromXYZ(crs = proj$prj4)                                              # Back to Raster (with no empty border)
  
raster2 <- projectRaster(raster, to = target)                                 # Project bathymetry onto new grid

plot(raster2)                                                                 # Visual check

#### rayshade ####

mat <- raster_to_matrix(raster2)                                              # Strip class

mat[mat < 0 ] = 0

shaded <- mat %>% 
  sphere_shade(texture = create_texture("white", "gray87", "white",           # Create a pale colour scale 
                                        "white", "white")) %>%  
  add_shadow(ray_shade(mat, multicore = T, progbar = T), 0.9) %>%             # Pale, parallel shading
  add_shadow(ambient_shade(mat, multicore = T, progbar = T), 0.9) %>%         # More shading
  add_shadow(lamb_shade(mat), 0.9) %>%                                        # Last shading
  add_water(detect_water(mat, progbar = T), color = "white")                  # White out the sea floor
  
plot_map(shaded, asp = (raster2@extent@xmax - raster2@extent@xmin) /          # Plot with the correct aspect ratio
                     (raster2@extent@ymax - raster2@extent@ymin))  

save_png(shaded, "./Figures/background.png",
         asp = (raster2@extent@xmax - raster2@extent@xmin) /                  # Save with the correct aspect ratio
               (raster2@extent@ymax - raster2@extent@ymin))  
