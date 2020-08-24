
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

Space <- Window(ECMWF_example[1,]$File, w = W, e = E, s = S, n = N)          # Get values to crop a netcdf file spatially at import. 

new <- stars::read_ncdf(ECMWF_example[1,]$File,
                        ncsub = cbind(start = c(Space$Limits$Lon_start, Space$Limits$Lat_start, 1, 1),
                                      count = c(Space$Limits$Lon_count, Space$Limits$Lat_count, 1, 1))) %>% 
  `st_crs<-`((crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +pm=180 +no_defs"))  %>% 
  .[wrap_domain[2,]]

wrap_domain <- st_transform(domains, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +pm=180 +no_defs")
#plot(wrap_domain)



raster <- raster(ECMWF_example[1,]$File)      # Import bathymetry

test <- stars::st_as_stars(raster) %>% 
  st_as_sf(points= FALSE)
#crop <- as(extent(0, 360, -90, 90), "SpatialPolygons")     # Create cropping window for land
#crs(crop) <- crs(raster)                                   # Match crs to bathymetry

#look <- st_join(test, g) %>% drop_na

x <- domains
xcrs = sf::st_crs(x)
g = (x + c(360, 90)) %% c(360) -c(0,90)
g = st_as_sf(g)
st_crs(g) <- xcrs
g <- mutate(g, Region = c("Barents Sea", "Greenland"))
plot(g)

ggplot() +
  geom_sf(data = test, colour = NA, aes(fill = Significant.height.of.combined.wind.waves.and.swell), show.legend = FALSE) +
  geom_sf(data = g, aes(colour = Region), fill = NA, show.legend = FALSE) +
  theme_minimal()


#test2 <- test[g2]
#plot(test2)
#raster <- crop(raster, crop)                               # Crop bathymetry
  
cells <- exactextractr::exact_extract(raster, g, include_cell = TRUE) %>% 
  data.table::rbindlist(idcol = "Region") %>% 
  mutate(Region = if_else(Region == 1, "Barents Sea", "Greenland"),
         x_index = colFromCell(raster, cell),
         y_index = rowFromCell(raster, cell),
         Longitude = xFromCell(raster, cell),
         Latitude = yFromCell(raster, cell)) %>%
  mutate(y_index = abs(y_index-max(y_index+1)))                    # Flip y coordinates in reverse because rasters have a weird convention
  
ggplot() + geom_raster(data = cells, aes(x= x_index, y= y_index, fill = value))

saveRDS(cells, "./Objects/ECMWF Targets.rds")                            # Save


#x <- domains
#xcrs = sf::st_crs(x)
#g = (x + c(360, 90)) %% c(360) -c(0,90)
#g = st_as_sf(g)
#st_crs(g) <- xcrs
#g <- mutate(g, Region = c("Barents Sea", "Greenland"))
#plot(g)

# g2 = st_as_sf(g - c(180, 0))
# st_crs(g2) <- xcrs
# sf::st_wrap_dateline(g2) + c(180,0)
# sf::st_set_crs(g2, xcrs)
# g2 <- mutate(g2, Region = c("Barents Sea", "Greenland"))

tic()
domains_mask <- expand.grid(Longitude = Space$Lons, Latitude = Space$Lats) %>%  # Get the data grid
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% # Convert to SF
  voronoi_grid(area = domains) %>%                                              # Calculate overlap with domain and share of area per pixel
  mutate(x_index = match(Longitude, Space$Lons) + Space$Limits$Lon_start - 1,   # Find x index in the netcdf file each pixel sits at
         y_index = match(Latitude, Space$Lats) + Space$Limits$Lat_start - 1)    # Find y index in the netcdf file each pixel sits at
toc()

ggplot(domains_mask) + geom_sf(aes(fill = Region), lwd = 0.1)                    # Check

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
  voronoi_grid(st_transform(domains, crs = 4326)) %>%                       # Work out the share of model domain for each SINMOD pixel
  select(-geometry)                                                         # drop repeated geometry column

ggplot(voronoi) + geom_sf(aes(fill = Cell_area), lwd = 0.1)                            # Check

saveRDS(voronoi, "./Objects/SINMOD Targets.rds")                            # Save
