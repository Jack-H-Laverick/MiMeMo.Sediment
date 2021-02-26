
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "raster", "stars")                   # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multisession)

d_silt <- brick("./Output/Daily_disturbance_silt.nc")                       # Import daily disturbance
d_sand <- brick("./Output/Daily_disturbance_sand.nc")
d_gravel <- brick("./Output/Daily_disturbance_gravel.nc")

weights <- map(c("Gravel", "Sand", "Silt"), ~{                              # Import sediment fraction proportions at each pixel
  
  weights <- raster("../Sediment/Output/Greenland_and_barents_sea_shelf_sediments.nc",
                    varname = .x) %>% 
    projectRaster(d_silt)                                                   # Make sure the projections match the disturbance layers
  
  weights[is.na(weights)] <- 0                                              # Nas need a weight of 0 to avoid errors
  
  return(weights)
})

#### Get variable to indicate month ####

raw <- ncdf4::nc_open("./Objects/tides.nc")                                 # Open file containing all the data
time <- raw$dim$time$len                                                    # Pull the length of the time dimension
ncdf4::nc_close(raw)                                                        # Close file

tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = time) %>% # Calculate time steps for the dataset
  format("%Y%m%d") %>%                                                      # Drop hours
  unique() %>%                                                              # Get unique days
  lubridate::ymd() %>%                                                      # Convert to get month easily
  lubridate::month()                                                        # Extract the month each entry is in

#### Average raster layers by month into a seasonal cycle of disturbance ####

s_silt <- future_map(1:12, ~{                                               # For each month in parallel
  d_silt[[which(tide_step == .x)]] %>%                                      # Select layers from the same month
  calc(mean, na.rm = T)}, .progress = T) %>%                                # Create a mean layer over all days
  brick()                                                                   # Combine into a 12 layer object

s_sand <- future_map(1:12, ~{                                               # repeat because rasters are messy
  d_sand[[which(tide_step == .x)]] %>%                                      
    calc(mean, na.rm = T)}, .progress = T) %>%                              
  brick()                                                                   

s_gravel <- future_map(1:12, ~{                                               
  d_gravel[[which(tide_step == .x)]] %>%                                
    calc(mean, na.rm = T)}, .progress = T) %>%                            
  brick()                                                                   

#### Weight by sediment map to get average disturbance per pixel ####

weighted_silt <- s_silt * weights[[3]]                                     # Weight a sediment layer by it's proportion for each pixel
weighted_sand <- s_sand * weights[[2]]
weighted_gravel <- s_gravel * weights[[1]]

whole <- (weighted_silt + weighted_sand + weighted_gravel)/100             # Perform weighted average

#### Regrid data ####

stress_grid <- data.table::fread("./Output/Seasonal.csv") %>%
  transmute(Longitude = round(Longitude, 5),
         Latitude = round(Latitude, 5)) %>% 
  distinct()

test <- st_as_stars(whole) %>%                                      # Make sure the projections match the disturbance layers
  st_as_sf(points = F, merge = F)

new_grid <- st_join(readRDS("./Objects/RF_sediment_observations.rds"),                                                   # Join previous data
                    test)  %>% 
  sfc_as_cols() %>% 
  st_drop_geometry() %>% 
  rename(Longitude = "x", Latitude = "y") %>% 
  mutate(Longitude = round(Longitude, 5),
         Latitude = round(Latitude, 5)) %>% 
  dplyr::select(Region:Latitude) %>% 
  dplyr::select(-Region) %>% 
  drop_na() %>% 
  right_join(stress_grid)

#### Save as csv ####

stress <- data.table::fread("./Output/Seasonal.csv") %>% 
  mutate(Longitude = round(Longitude, 5),
         Latitude = round(Latitude, 5))

new_grid %>%                                
  setNames(c(month.name, "Longitude", "Latitude")) %>% 
  pivot_longer(-c(Longitude, Latitude), names_to = "Month", values_to = "Proportion_time_disturbed") %>% 
  left_join(stress) %>% 
  data.table::fwrite("./Output/Greenland_and_barents_sea_seasonal.csv")

#### Combine with netcdf file ####

layers <- map(1:12, ~{
  raster <- new_grid[,c("Longitude", "Latitude", str_glue('layer.{.x}'))] %>% 
    rasterFromXYZ()}) %>% 
  brick()

#plot(layers)

writeRaster(layers,
            "./Output/Greenland_and_barents_sea_seasonal.nc", overwrite = TRUE, format = "CDF", 
            varname= "Natural_disturbance", 
            longname = "Average time shields value exceeds threshold for the initiation of motion, weighted by sediment fractions", 
            varunit = "Proportion", xname = "Longitude", yname = "Latitude", zname = "Month")


system("ncks -A ./Output/Greenland_and_barents_sea_seasonal_stress.nc ./Output/Greenland_and_barents_sea_seasonal.nc")
