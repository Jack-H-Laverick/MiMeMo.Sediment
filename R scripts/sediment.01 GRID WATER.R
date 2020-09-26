
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "sf", "tictoc", "ncdf4", "data.table", "raster") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

domains <- readRDS("./Objects/Domain.rds") %>%                              # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)

ECMWF_example <- list.files("./Data/ECMWF_Waves/", full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%
  slice(1) %>% 
  rename(File = ".") %>% 
  mutate(File = as.character(File),
         Year = str_sub(File, start = -7, end = -4))

SINMOD_example <- list.files("/mnt/idrive/Science/MS/Shared/CAO/SINMOD", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe/tibble
  separate(".", into = c("path", "file"), sep = "_") %>%                    # Extract the year and month from the file name
  mutate(path = paste0(path, "_")) %>%                                      # Replace the dropped separator
  filter(!file %in% c("200301.nc", "ncdump")) %>%                           # First time step isn't accessible, and don't want the dump file
  slice(1)                                                                  # Take 1 file as an example

#### Functions ####

Window <- function(file, w, e, s, n) {
  
  raw <- ncdf4::nc_open(file)
  lon <- raw$dim$longitude$vals %>% dplyr::between(w, e)
  W <- min(which(lon == TRUE))
  E <- max(which(lon == TRUE))
  
  lat <- raw$dim$latitude$vals %>% between(s, n)
  S <- min(which(lat == TRUE))
  N <- max(which(lat == TRUE))
  
  lons <- raw$dim$lon$vals[W:E]
  lats <- raw$dim$lat$vals[S:N]
  
  Limits <- data.frame("Lon_start" = W, "Lon_count" = E - W + 1, "Lat_start" = S, "Lat_count" = N - S + 1)
  
  Limits <- list(Lats = lats, Lons = lons, Limits = Limits)
  return(Limits)
} # Extract the positions to clip the netcdf file to, and the values for the smaller grid

#### Waves ECMWF ####

Space <- Window(ECMWF_example[1,]$File, w = W+180, e = E+180, s = S, n = N)          # Get values to crop a netcdf file spatially at import. 

voronoi_grid2 <- function(points, area) {
  
  result <- purrr::map(1:nrow(area), ~{                            # For each polygon in area
    
    limited_points <- points %>%                                   # Take the grid points
      st_join(area[.x,]) %>%                                       # Find points in the polygon
      drop_na()                                                    # Drop those outside
    
    voronoi <- limited_points %>%                                  # Take the grid points
      sf::st_geometry() %>%                                        # To get sfc from sf
      sf::st_union() %>%                                           # To get a sfc of MULTIPOINT type
      sf::st_voronoi(envelope = sf::st_geometry(area[.x,])) %>%    # Voronoi polygon for the area
      sf::st_collection_extract(type = "POLYGON") %>%              # A list of polygons
      sf::st_sf() %>%                                              # From list to sf object
      sf::st_join(limited_points) %>%                              # put names back
      sf::st_intersection(area[.x,]) %>%                           # Cut to shape of target area
      dplyr::mutate(Cell_area = units::drop_units(sf::st_area(.))) # Area of each polygon
  }) %>%
    dplyr::bind_rows() %>%                                         # Combine the results from each area
    sf::st_sf(geomc = .$geometry, crs = 4326)                      # Reinstate attributes of the geometry column
  
}

tic()
domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>%  # Get the data grid
  mutate(Longitude = Longitude - 180) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Convert to SF
  st_join(domains) %>%                                                          # which points are in the domain
  drop_na() %>%                                                                 # drop those outside
  mutate(x_index = match(Longitude, Space$Lons - 180) + Space$Limits$Lon_start - 1, # Find x index in the netcdf file each pixel sits at
         y_index = match(Latitude, Space$Lats) + Space$Limits$Lat_start - 1)    # Find y index in the netcdf file each pixel sits at
toc()

ggplot(domains_mask) + geom_sf(aes(colour = Region), size = 0.1)                   # Check

saveRDS(domains_mask, "./Objects/ECMWF Targets.rds")                            # Save

#### Tides SINMOD ####

raw <- nc_open(paste0(SINMOD_example$path[1], SINMOD_example$file[1]))      # Open example file

Lats <- ncvar_get(raw, "gridLats")                                          # Pull latitudes
Lons <- ncvar_get(raw, "gridLons")                                          # Pull longitudes
depth <- ncvar_get(raw, "depth")

coords <- reshape2::melt(Lons) %>%                                          # Reshape the matrix to a 3 column data.frame
  as.data.table() %>%                                                       # Convert to a data.table for speed
  .[as.data.table(reshape2::melt(Lats)),                                    # Bind latitudes treated in the same way
    on = c("Var1", "Var2")] %>%                                             # using the matrix indices
  .[as.data.table(reshape2::melt(depth)),                                   # Bind depths treated in the same way
    on = c("Var1", "Var2")] %>%                                             # using the matrix indices
  setnames(c("V1", "V2", "Longitude", "Latitude", "Depth")) %>%             # Rename
  .[Latitude %between% c(S, N) & Longitude %between% c(W,E)] %>%            # Clip to points within rough crop
  drop_na                                                                   # Drop points on land

voronoi <- st_as_sf(setDF(coords), coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>% # Convert to sf 
  st_join(domains) %>% 
  drop_na()

ggplot(voronoi) + geom_sf(aes(colour = Depth), size = 0.8)                  # Check

saveRDS(voronoi, "./Objects/SINMOD Targets.rds")                            # Save
