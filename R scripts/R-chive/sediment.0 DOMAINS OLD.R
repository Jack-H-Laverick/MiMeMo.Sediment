
#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "stars", "raster", "tictoc", "sf", "data.table")        # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

furrr::plan(multiprocess)
source("./R scripts/@_Set up file.R")

#### Import bathymetry ####

raster <- raster("../Barents Sea/Data/GEBCO_2019.nc")      # Import bathymetry

crop <- as(extent(-80, 90, 60, 85), "SpatialPolygons")     # Create cropping window for land
crs(crop) <- crs(raster)                                   # Match crs to bathymetry

raster <- crop(raster, crop)                               # Crop bathymetry

raster[raster < -400] <- NA                                # Blank out points deeper than 400 m
raster[raster > 0] <- 0                                    # Level all land to 0 m for easy selection 

#### Get land for plotting ####

land <- raster
land[land != 0] <- NA
land <- st_as_stars(land) %>% 
  st_as_sf(merge = TRUE)

saveRDS(land, "./Objects/Land.rds")                         
plot(st_transform(land, crs = 3035))

#### Calculate terrain ####

crop <- as(extent(W, E, S, N), "SpatialPolygons")     # Shrink cropping window for calculations
crs(crop) <- crs(raster)                                   # Match crs to bathymetry

raster <- crop(raster, crop)                               # Crop bathymetry

terrain <- terrain(raster, unit = "degrees", c("slope", "TPI", "TRI", "roughness")) # Calculate bathymetric variables
terrain[raster == 0] <- NA                                                          # Drop points on land

plot(terrain)

#### Get an SF polygon for the interesting area ####

area <- raster
area[area < 0] <- 10                                # Level all land to 10 m for easy selection 
area[area == 0] <- NA                               # Match values below sea level so SF objects merge into a polygon

area <- st_as_stars(area) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_union()

plot(st_transform(area, crs = 3035))

domain <- rbind(st_intersection(st_transform(Barents_mask, crs = 3035), 
                                st_transform(area, crs = 3035)), 
                st_intersection(st_transform(Greenland_mask, crs = 3035), 
                                st_transform(area, crs = 3035))) 

saveRDS(domain, "./Objects/Domain.rds")                                        # Import mapped areas for plotting
plot(domain)

#### Downsample grid to 0.01 x 0.01 ####


cells <- raster(resolution = 0.01)
crs(cells) <- crs(crop)                                   # Match crs to bathymetry
cells <- crop(cells, crop)
cells$value <- 10

cells <- st_as_stars(cells) %>%
  st_transform(crs = 3035) %>%
  .[domain]

cells_sf <- st_as_sf(cells, points = FALSE, merge = FALSE)

saveRDS(cells_sf, "./Objects/Extracters.rds")                     # Import cells for zoom to show domain boundary
plot(cells)

#ggplot() + geom_sf(data = land) + geom_sf(data = cells, fill = NA, colour = "orange", lwd = 0.01)

## Shrink grid for speed ##

raster[raster == 0] <- NA                                    # Blank out land 

terrain <- addLayer(raster, terrain) #%>% 
#  aggregate(fact = 0.01/res(raster), expand = TRUE)

#### Sample new grid ####

# tic()
# extracted <- as.list(raster, terrain) %>% 
#   map(exactextractr::exact_extract, y = cells_sf, fun = 'mean') %>% 
#   do.call(cbind, .)
# colnames(extracted) <- c("Depth","Slope", "TPI", "TRI", "Roughness")
# extracted <- cbind(cells_sf, extracted) %>% 
#   dplyr::select(-value)
# toc()

## Parallelise rasters
# tic()
# extracted <- as.list(raster, terrain) %>% 
#   future_map(exactextractr::exact_extract, y = cells_sf, fun = 'mean') %>% 
#   do.call(cbind, .)
# colnames(extracted) <- c("Depth","Slope", "TPI", "TRI", "Roughness")
# extracted <- cbind(cells_sf, extracted) %>% 
#   dplyr::select(-value)
# toc()

#saveRDS(extracted, "./Objects/Bathymetry.rds")

## raw
# tic() # 3.15 hours for numbers with meta data
# extracted_quick <- exactextractr::exact_extract(terrain, cells_sf, 
#              include_xy = TRUE, include_cell = TRUE)
# toc()
# 
# 
# tic("Single layer with meta data") # 2.67 hours
# extracted_quick2 <- exactextractr::exact_extract(terrain[[1]], cells_sf, 
#                                                 include_xy = TRUE, include_cell = TRUE)
# toc()

# tic("Numbers no meta-data, single")   # Aggregated 1.3 # non-aggregated 1.36
# extracted_quick3 <- exactextractr::exact_extract(terrain[[1]], cells_sf)
# toc()
# 
# tic()     # Aggregated
# parallel_values <- as.list(terrain) %>% 
#   furrr::future_map(exactextractr::exact_extract, y = cells_sf) 
# toc()
  
tic("Numbers no meta-data")   # 1.87 hours # 2.4 for non-aggregated grid
extracted_quick3 <- exactextractr::exact_extract(terrain, cells_sf) %>% 
  rbindlist(idcol = "cell_sf") %>% 
  .[,list(Depth= weighted.mean(layer, coverage_fraction, na.rm = TRUE),
          Slope= weighted.mean(layer.1, coverage_fraction, na.rm = TRUE),
          TPI= weighted.mean(layer.2, coverage_fraction, na.rm = TRUE),
          TRI= weighted.mean(layer.3, coverage_fraction, na.rm = TRUE),
          Roughness= weighted.mean(layer.4, coverage_fraction, na.rm = TRUE)),
    keyby = cell_sf] %>% 
  data.frame(cells_sf) %>% 
  dplyr::select(-value) %>% 
  st_sf(geometry = .$geometry)
toc()  

saveRDS(extracted_quick3, "./Objects/Bathymetry.rds")

ggplot(extracted_quick3) + geom_sf(aes(fill = Depth), colour = NA)

#tic("Summarised") # 34 hours
#extracted_quick4 <- exactextractr::exact_extract(terrain, cells_sf, fun = 'mean')
#toc()

## raw
#tic()
#extracted_quick <- exactextractr::exact_extract(raster, cells_sf, 
#             include_xy = TRUE, include_cell = TRUE)
#toc()

#raster[extracted[[1]]$cell[11]]
 
point <- data.frame(x= 25, y = 75, region = "inset") %>%          # Choose a point to zoom to for the map showing the model domain
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  st_transform(crs = 3035)

inset <- cells[st_buffer(point, 3000)] %>%                        # Cut the grid within 3km of the point
  st_as_sf(points = FALSE, merge = FALSE) %>%
  st_intersection(st_buffer(point, 2000))                         # Cut out a circle with a 2km radius to get a perfect cirlce
saveRDS(inset, "./Objects/inset.rds")

#ggplot() + 
#  geom_sf(data = circle, fill = "lightblue2") + 
#  geom_sf(data = inset, fill = "lightblue3") +
#  theme_void()

#ggplot() + geom_sf(data = land, fill = "black") + geom_sf(data = targets, colour = "orange")


## Target grid 
target <- st_transform(domain, crs = 4326) %>% 
  st_bbox() %>% 
  round(digits = 2)

xcells <- as.character(((target$xmax - target$xmin)/0.01) + 1)
ycells <- as.character(((target$ymax - target$ymin)/0.01) + 1)


system(str_glue("cat > template << EOF
       gridtype = lonlat
       xsize = {xcells}
       ysize = {ycells}
       xfirst = {target$xmin}
       xinc = 0.01
       yfirst = {target$ymin}
       yinc = 0.01
       grid_mapping_name = latitude_longitude
       EOF
       
       cdo -remapbil,template ./Output/Terrain.nc coarse.nc"))


system(str_glue("cdo -f nc -sellonlatbox,{target$xmin},{target$xmax},{target$ymin},{target$ymax} -random,r{xcells}x{ycells} template.nc"))
##
system(str_glue("cdo remapcon,r{xcells}x{ycells} ./Output/Terrain.nc coarse.nc"))

system("cdo -remapcon,template.nc ./Output/Terrain.nc coarse.nc")



raw <- ncdf4::nc_open("./Output/Terrain.nc")

