
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "tictoc", "furrr", "sf")         # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

plan(multiprocess)

wave_ts <- readRDS("./Objects/Wave_ts.rds")                                 # Read in a list of time series for waves at pixels
 
tide_ts <- readRDS("./Objects/Tide_ts.rds")                                 # Read in a list of time series for tides at pixles

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels

#### function ####

align_stress <- function (tide_ts, wave_ts, depth, percentile = 0.95) {
  
  #  align <- merge(tide_ts, wave_ts)                                # Match up time series (identify missing steps)
  
  if(anyNA(tide_ts[,1]) | anyNA(wave_ts[,1])) { 
  
  stress <- NA                   # If we don't have data get NA, the stress function won't run
  
  } else {                       # If we do have data calculate bed shear stress
    
  align <- zoo::merge.zoo(tide_ts, wave_ts, all = c(T,T))          # Match up time series (identify missing steps)
  
  align$mwd <- zoo::na.approx(align$mwd, rule = 2)                 # Interpolate wave direction onto tide time steps
  align$swh <- zoo::na.approx(align$swh, rule = 2)                 # Interpolate wave height onto tide time steps
  align$mwp <- zoo::na.approx(align$mwp, rule = 2)                 # Interpolate wave period onto tide time steps
  
  align <- align[which(zoo::index(align) %in% zoo::index(tide_ts)),] # Keep only tide time steps
  
  ## CALCULATE BED SHEAR STRESS ##
  
stress <- bedshear::shear_stress(
  bathymetry = depth,                                            # Depth to sea floor
  D50 = 0.02,                                                    # Nominal median grain size
  tidal_velocity = align[,"uvSpeed"],                            # Water movements
  tidal_direction = align[,"uvDirection"],
  wave_height = align[,"swh"],
  wave_period = align[,"mwp"],
  wave_direction = align[,"mwd"]/10,
  switch = 0) %>%
  dplyr::select(shear_mean) #%>%                                  # Keep only mean bed shear stress
 #  dplyr::mutate(Time_step = zoo::index(align))                  # Add time
    
  stress <- quantile(stress$shear_mean, percentile)                 # Calculate the percentile over the time series
      }

  return(stress)  

}

#### looping over pixels ####

## Can I split time series to parallelise? I can't just do it straight away because the objects exceed memory limits
# ggplot(pairs) + geom_point(aes(x= tide_entry, y = wave_entry)) # both indices increase together so we should be able to break the data into chunks

pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 500)               # Split into chunks of 1000 rows
tides_chunked <- map(pairs_chunked, ~{tide_ts[.x$tide_entry] })             # For each chunk of the dataframe, copy the appropriate time series
waves_chunked <- map(pairs_chunked, ~{wave_ts[.x$wave_entry] })

chunked <- list(tides = tides_chunked, 
                waves = waves_chunked, 
                depths = map(pairs_chunked, ~{ as.list(.[,"Depth"])}))                  # Collect into a list to map over

rm(pairs_chunked, tides_chunked, waves_chunked, tide_ts, wave_ts)           # Delete big redundant objects

tic()
fast <- pmap(chunked, function(tides, waves, depths) {                      # Over chunks of input sequentially, so that chunks are small enough to be copied
                        future_pmap_dbl(list(tides, waves, depths),         # For each pairing of waves, tides, and depths
                                        align_stress,                       # calculate bed shear stress
                                        .progress = TRUE)
               })
toc()

#### Bind ####

rm(chunked)

points <- mutate(pairs, Stress95 = unlist(fast)) %>%                        # Append the results to pixels
  drop_na() %>%                                                             # Remove the influence of pixels without stress 
  select(-ends_with("_entry")) %>%                                          # Drop redundant columns
  select(-Region) %>%                                                       # We'll recreate the shore column for voronoi pixels
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>%  # Convert to sf 
  st_transform(crs = crs)

domain <- readRDS("./Objects/Domain.rds")                                   # import domain polygons

weighted <- future_map(1:nrow(domain), ~{                          # For each polygon in the model domain
    voronoi <- points %>%                                          # Take the grid points
      sf::st_geometry() %>%                                        # To get sfc from sf
      sf::st_union() %>%                                           # To get a sfc of MULTIPOINT type
      sf::st_voronoi(envelope = sf::st_geometry(domain[.x,])) %>%  # Voronoi polygon for the area
      sf::st_collection_extract(type = "POLYGON") %>%              # A list of polygons
      sf::st_sf() %>%                                              # From list to sf object
      sf::st_join(points) %>%                                      # put names back
      st_make_valid() %>%                                          # Fix some weird intersection error
      sf::st_intersection(domain[.x,]) %>%                         # Cut to shape of NC state
      dplyr::mutate(Cell_area = units::drop_units(sf::st_area(.))) # Area of each polygon
  }) %>%                          # Calculate share of model domain for each time series
  bind_rows() %>%                                                           # Combine the results from each area
  st_sf(geometry = .$geometry, crs = crs)                                   # Reinstate object class

saveRDS(weighted, "./Objects/Stress95.rds")                                 # Save

# ggplot(points) + geom_sf(aes(colour = log(Stress95+1))) +
#  viridis::scale_colour_viridis(option = 2, na.value = "green") +
#  theme_minimal()

ggplot(weighted) + geom_sf(aes(fill = log(Stress95+1)), size = 0.001, colour = NA) +
  viridis::scale_fill_viridis(option = 2, ) +
  theme_minimal()

ggsave_map("./Figures/stress95.png", last_plot())
