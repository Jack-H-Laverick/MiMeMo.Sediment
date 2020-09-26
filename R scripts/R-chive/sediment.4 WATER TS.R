
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "raster", "tidyverse", "sf", "tictoc", "ncdf4", "furrr") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

domains <- readRDS("./Objects/Domain.rds") %>%                              # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                                  # Transform to Lat/Lon to match other objects

# raster <- raster("./Objects/waves.nc")      # Import bathymetry
# 
# ECMWF_points <- exactextractr::exact_extract(raster, domains, include_cell = TRUE) %>% # Get target pixels in new ECMWF grid
#   data.table::rbindlist(idcol = "Region") %>% 
#   mutate(Region = if_else(Region == 1, "Barents Sea", "Greenland"),
#          wave_x = colFromCell(raster, cell),
#          wave_y = rowFromCell(raster, cell),
#          Longitude = xFromCell(raster, cell),
#          Latitude = yFromCell(raster, cell)) %>%
# #  mutate(wave_y = abs(wave_y-max(wave_y+1))) %>%                            # Flip y coordinates in reverse because rasters have a weird convention
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) # Convert to SF

# ECMWF_cells <- rasterFromCells(raster, ECMWF_points$cell) %>% 
#   stars::st_as_stars() %>%  
#   st_as_sf(points = FALSE, merge = FALSE) %>% 
#   filter(layer %in% ECMWF_points$cell) %>% 
#   st_join(ECMWF_points)

#ggplot(ECMWF_cells) + geom_sf(aes(fill = layer), colour =NA)

ECMWF_cells <- readRDS("./Objects/ECMWF Targets.rds") %>%                    # Import locations of traget pixels in ECMWF grid
  mutate(wave_x = x_index - min(x_index) + 1,                               # Adjust indices to sample at now we cropped the files
         wave_y = y_index - min(y_index) + 1) %>% 
  dplyr::select(-c(x_index, y_index))

SINMOD_cells <- readRDS("./Objects/SINMOD Targets.rds") %>%                  # Import target pixels in SINMOD grid
  mutate(tide_x = V1 - min(V1) + 1,                                         # Adjust indices to sample at now we cropped the files
         tide_y = V2- min(V2) + 1) %>% 
  dplyr::select(-c(V1, V2))

#### Align grids ####

## Which ECMWF and SINMOD cells fall on the sediment/Global Fishing Watch grid?

GFW_grid <- st_make_grid(domains, what = "centers", cellsize = 0.1) %>%     # Make a 0.1 degree grid across the model domain
  st_sf(dummy = 1)                                                          # Convert to sf for joining

# targets1 <- st_join(GFW_grid, dplyr::select(ECMWF_cells, wave_x, wave_y, Region))  # Check pixels from our sources match the new grid
# targets2 <- st_join(GFW_grid, dplyr::select(SINMOD_cells, tide_x, tide_y, Depth, Region))
tic()
 targets1 <- st_join(GFW_grid, dplyr::select(ECMWF_cells, wave_x, wave_y, Region),
                     join = nngeo::st_nn, k = 1, parallel = 8 )  # Find pixels from our sources as nearest neighbour to the new grid
toc()
targets2 <- st_join(GFW_grid, dplyr::select(SINMOD_cells, tide_x, tide_y, Depth, Region),
                    join = nngeo::st_nn, k = 1, parallel = 8 )  # Find pixels from our sources as nearest neighbour to the new grid

aligned <- cbind(targets1, targets2[,c("tide_x", "tide_y", "Depth")]) %>%            # Combine the matched pixels from the two sources
  dplyr::select(-c(dummy, ..1)) %>% 
  sfc_as_cols(names = c("Longitude", "Latitude")) %>%                       # Get coordinates 
  st_drop_geometry()                                                        # Drop SF formatting

tide_pixels <- dplyr::select(aligned, tide_x, tide_y) %>%                          # Which are the unique grid cells we need to calculate for?
  distinct()                                                                # We can save time indexing duplicate cells later

wave_pixels <- dplyr::select(aligned, wave_x, wave_y) %>% 
  distinct()

#ggplot(targets1) + geom_sf(size = 0.1, stroke = 0)
#ggplot(targets2) + geom_sf(size = 0.1, stroke = 0)
#ggplot(GFW_grid) + geom_sf(size = 0.1, stroke = 0)
#ggplot(aligned) + geom_raster(aes(x=Longitude, y = Latitude, fill = Depth))

#### Create look up table ####

Water_pairs <- left_join(aligned, mutate(tide_pixels, tide_entry = row_number())) %>% # Add a time series ID to each source 
  left_join(mutate(wave_pixels, wave_entry = row_number())) %>%             # And bind to the alignment object
  select(-ends_with(c("_y", "_x")))                                         # Drop redundant columns          

saveRDS(Water_pairs, "./Objects/Water look up table.rds")                   # Save

#rm(aligned, domains, ECMWF_cells, ECMWF_points, GFW_grid, raster, SINMOD_cells, targets1, targets2)                     # Clear memory
rm(aligned, domains, ECMWF_cells, GFW_grid, SINMOD_cells, targets1, targets2)   # Clear memory

#### Waves ####

raw <- nc_open("./Objects/waves.nc")                               # Open file containing all the data
swh <-  ncvar_get(raw, "swh")                                               # Import significant wave height
mwp <-  ncvar_get(raw, "mwp")                                               # Import mean wave period
mwd <-  ncvar_get(raw, "mwd")                                               # Import mean wave direction
nc_close(raw)                                                               # Close file

empty <- apply(swh, c(1,2), empty)                                          # Which pixels never have waves (probably land), same result for three variables

raw <- nc_open("./Objects/wave_land_mask.nc")                               # Open file containing land mask
land <-  ncvar_get(raw, "lsm")                                              # Import mask
nc_close(raw)                                                               # Close file

empty2 <- land[empty == TRUE & land != 0]                                   # Which are all NA and are not totally ocean

#check <- reshape2::melt(empty)
#ggplot(check) + geom_raster(aes(Var1, Var2, fill = value))

swh[is.na(swh)] <- 0                                                        # replace NA's with 0, ice means no waves, not NA for me
mwp[is.na(mwp)] <- 0
mwd[is.na(mwd)] <- 0

swh[empty2] <- NA                                                          # Put back NA's on pixels which never have waves and coincide with land
mwp[empty2] <- NA                                                          # This lets pixels with a little land keep values if they ever report a wave value
mwd[empty2] <- NA

Wave_step <- seq(ISOdate(2003, 01, 1, 0), by = "3 hours", length.out = length(swh[40,40,])*28) # Calculate time steps for the dataset

day_2_month <- function(vec, chunk_length) {
  
  split(vec, ceiling(seq_along(vec)/chunk_length)) %>%     # Split the cycle into chunks (I have a 12 mean monthly cycles, you can tease these out)
    map(rep, times = 28) %>%                               # Repeat each chunk (mean monthly cycle) 28 times   
    unlist()                                               # Recombine to an annual vector
  }                            # Expand a monthly mean vector to a daily time series                            

wave_ts <- function(swh, mwp, mwd, Wave_step) {
  
  month <- cbind(day_2_month(swh, 8),                           # Expand each variable to a full annual cycle
                 day_2_month(mwp, 8), 
                 day_2_month(mwd, 8))

  wave_ts <- matrix(t(month), ncol = ncol(month), byrow = TRUE) # Duplicate the months to get a daily tace (quickly)
  
  colnames(wave_ts) <- c("swh", "mwp", "mwd")                   # reinstate coumn names

  wave_ts <- zoo::zoo(wave_ts, Wave_step)                       # Add time steps and convert to zoo object
}                         # Expand time series and convert to zoo object

Wave_list <- map2(wave_pixels$wave_x, wave_pixels$wave_y,                   # For each unique pixel
                  ~ wave_ts(swh[.x,.y,], mwp[.x,.y,], mwd[.x,.y,], Wave_step)) # Pull a time series of wave data

saveRDS(Wave_list, "./Objects/Wave_ts.rds")                                 # Save out list of time series

rm(swh, mwp, mwd, land, empty2, Wave_list, Wave_step, wave_pixels)                     # Clear memory

#### Tides ####

raw <- nc_open("./Data/SINMOD/tides.nc")                                    # Open file containing all the data
u <-  ncvar_get(raw, "u_east")                                              # Import u tidal currents
v <-  ncvar_get(raw, "u_east")                                              # Import v tidal currents
nc_close(raw)                                                               # Close file

empty <- apply(u, c(1,2), empty)                                            # Which pixels never have water movement (probably land), same result for two water variables)

u[is.na(u)] <- 0                                                            # replace NA's with 0, unless there's never water movement, a pixel shold be still when NA
v[is.na(v)] <- 0

u[empty] <- NA                                                              # Put back NA's on pixels which never have water movement
v[empty] <- NA

Tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = length(u[40,40,])) # Calculate time steps for the dataset

tide_ts <- function(u, v) {
  
  tide_ts <- zoo::zoo(cbind(                                # Bind 
                      vectors_2_direction(u, v)),           # Convert UV to speed and direction
                      Tide_step)                            # Convert to zoo object to align time series
}                                                           # Convert UV vectors to a time series of speed and direction

Tide_list <- map2(tide_pixels$tide_x, tide_pixels$tide_y,   # For each unique pixel
                  ~ tide_ts(u[.x,.y,],                      # Pull a times series of tide data
                            v[.x,.y,]))                     # Sampling at specific pixels
                                                             

saveRDS(Tide_list, "./Objects/Tide_ts.rds")
