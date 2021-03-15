
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "raster")                            # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

plan(multisession)

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels
pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 200)               # Split into chunks of 1000 rows

#### function ####

calculate_stress <- function (data, depth, percentile = 0.95) {
  
  if(anyNA(data)) { 
  
  stress <- t(data.frame(rep(NA , 12)))                  # If we don't have data get NA, the stress function won't run
  row.names(stress) <- NULL
  stress <- as.data.frame(stress)
  names(stress) <- 1:12
  
  } else {                       # If we do have data calculate bed shear stress
    
  ## CALCULATE BED SHEAR STRESS ##
  
stress <- bedshear::shear_stress(
  bathymetry = depth,                                            # Depth to sea floor
  D50 = 0.02,                                                    # Nominal median grain size
  tidal_velocity = data$uvSpeed,                            # Water movements
  tidal_direction = data$uvDirection,
  wave_height = data$swh,
  wave_period = data$mwp,
  wave_direction = data$mwd/10,
  switch = 0) %>%
  dplyr::select(shear_mean, shields_number, shields_critical) %>%                   # Keep only mean bed shear stress
  mutate(movement = shields_number > shields_critical,            # When is dimensionless shear stresss larger than shear stress required to initiate motion
         month = data$month) %>% 
  group_by(month) %>% 
  summarise(stress = as.numeric(quantile(shear_mean, percentile))) %>%            # Calculate the percentile over the time series
  pivot_wider(names_from = month, values_from = stress)

      }

  return(stress)  

}

#### looping over pixels ####

depths <- map(pairs_chunked, ~{ as.list(.[,"Depth"])})

tic()
stress <- future_map(0:(length(pairs_chunked)-1), ~ { 
#stress <- future_map(0:2, ~ { 
  
  map2(readRDS(glue::glue("./Objects/stress_input/{.x}.rds")),
               depths[[(.x+1)]],
               calculate_stress, percentile = 0.95)}, .progress = T) %>% 
  unlist(recursive = F) %>% 
  data.table::rbindlist()
toc()

#### Save output as netcdf and csv ####

points <- cbind(pairs, stress) %>%                                                 # Append the results to pixels
  drop_na() %>%                                                                    # Remove the influence of pixels without stress 
  dplyr::select(-ends_with("_entry")) %>%                                          # Drop redundant columns
  dplyr::select(-Region) %>%                                                       # We'll recreate the shore column for voronoi pixels
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>%# Convert to sf 
  st_transform(crs = crs)

grid <- st_join(readRDS("./Objects/Stress95.rds"), points) %>% 
  .[,-(1:9)]

ggplot(grid) + geom_sf(aes(fill = log(`8`+1)), size = 0.001, colour = NA) +
  viridis::scale_fill_viridis(option = 2, ) +
  theme_minimal()

#### Match to output rasters ####

new_grid <- st_join(readRDS("./Objects/RF_sediment_observations.rds"),                                                   # Join previous data
                    st_transform(grid, crs = 4326))  %>% 
  sfc_as_cols() %>% 
  st_drop_geometry() %>% 
  rename(Longitude = "x", Latitude = "y") %>% 
  dplyr::select(Region:Latitude) %>% 
  dplyr::select(-Region) %>% 
  drop_na()  

layers <- map(as.list(as.character(1:12)), ~{
    raster <- new_grid[,c("Longitude", "Latitude", .x)] %>% 
      rasterFromXYZ()}) %>% 
  brick()

#plot(layers)

writeRaster(layers,
            "./Output/Greenland_and_barents_sea_seasonal_stress.nc", overwrite = TRUE, format = "CDF", 
            varname= "Stress", longname = "Bed shear stress", 
            varunit = "N.m^-2", xname = "Longitude", yname = "Latitude", zname = "Month")

dplyr::select(new_grid, Longitude, Latitude, as.character(1:12)) %>% 
  setNames(c("Longitude", "Latitude", month.name)) %>% 
  pivot_longer(-c(Longitude, Latitude), names_to = "Month", values_to = "Bed_shear_stress") %>% 
  data.table::fwrite("./Output/Seasonal.csv")

