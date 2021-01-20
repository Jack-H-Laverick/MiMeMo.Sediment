
##**## hillshading

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf", "raster", "rayshader", "tictoc")             # List handy data packages
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
  
raster2 <- projectRaster(raster, to = target) %>%                             # Project bathymetry onto new grid
  aggregate(fact = 2)

#plot(raster2)                                                                 # Visual check

domain <- readRDS("./Objects/Domain.rds") %>% st_transform(crs = 3035)        # Import model domain

#### rayshade ####

mat <- raster_to_matrix(raster2)                                              # Strip class

shaded <- mat %>% 
  sphere_shade(texture = create_texture("white", "gray87", "white",           # Create a pale colour scale 
                                        "white", "white")) %>%  
  add_overlay(generate_polygon_overlay(domain, linecolor = "white", extent = attr(raster2, "extent"), 
                                      linewidth = 0, palette = "yellow", width = (1252*2.586122), height = (158*8))) %>% 
add_shadow(ray_shade(mat, multicore = T, progbar = T), 0.9) %>%             # Pale, parallel shading
  add_shadow(ambient_shade(mat, multicore = T, progbar = T), 0.9) %>%         # More shading
  add_shadow(lamb_shade(mat), 0.9)                                            # Last shading

#plot_map(shaded, asp = (raster2@extent@xmax - raster2@extent@xmin) /          # Plot with the correct aspect ratio
#                     (raster2@extent@ymax - raster2@extent@ymin))  

plot_3d(shaded, mat, asp = (raster2@extent@xmax - raster2@extent@xmin) /          # Plot with the correct aspect ratio
                     (raster2@extent@ymax - raster2@extent@ymin),
        zscale = 20, fov = 0, theta = 0, phi = 60, 
        windowsize = c(1200, 675), zoom = 0.65,
        water = TRUE, waterdepth = 0, wateralpha = 0.75, watercolor = "lightblue",
        waterlinecolor = "white", waterlinealpha = 0.5)  


tic()
render_highquality(parallel = TRUE, #filename = "./Figures/rayshade-hi.png",
                   scene_elements = bind_rows(rayrender::text3d(label = "Greenland Sea", text_height = (425*5), 
                                                material = rayrender::metal(),
                                                y = (800*5), x = (-2000*5), z = (-1500*5), angle = c(60,0,0)),
                                              rayrender::text3d(label = "Barents Sea", text_height = (425*5), 
                                                material = rayrender::metal(),
                                                y = (800*5), x = (2500*5),z = (-1500*5), angle = c(60,0,0)),
                                              rayrender::sphere(radius = 25, 
                                                material = rayrender::light(intensity = 1000), y = 400)),
                   samples = 2000, min_variance = 0, lightintensity = 0)
toc()