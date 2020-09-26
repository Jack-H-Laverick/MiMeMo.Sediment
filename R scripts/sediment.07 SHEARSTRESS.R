
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "tictoc", "furrr", "sf")         # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

plan(multiprocess)

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels
pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 200)               # Split into chunks of 1000 rows

#### Save out chunks ####

#wave_ts <- readRDS("./Objects/Wave_ts.rds")                                 # Read in a list of time series for waves at pixels

#tic()
#future_imap(pairs_chunked, ~{wave_ts[.x$wave_entry] %>%  
#                     saveRDS(glue::glue("./Objects/Time shift/wave{.y}.rds"))
#                      print(glue::glue("{.y}/1290"))}, .progress = TRUE)
#toc()

#rm(wave_ts)

# tic()
# map(pairs_chunked, ~{wave_ts[.x$wave_entry] }) %>% 
#   imap( ~ { saveRDS(.x, glue::glue("./Objects/Time shift/wave{.y}.rds"))})
# 
# rm(wave_ts)
# toc()

#tide_ts <- readRDS("./Objects/Tide_ts.rds")                                 # Read in a list of time series for tides at pixles

#tic()
#imap(pairs_chunked, ~{tide_ts[.x$tide_entry] %>% 
#                      saveRDS(glue::glue("./Objects/Time shift/tide{.y}.rds"))
#print(glue::glue("{.y}/1290"))}, .progress = TRUE)
#toc()

#rm(tide_ts)

#### Bind chunks ####

#tic()
#future_map(0:(length(pairs_chunked)-1), ~ { 
  
#  map2(readRDS(glue::glue("./Objects/Time shift/tide{.x}.rds")),
#       readRDS(glue::glue("./Objects/Time shift/wave{.x}.rds")),
#       cbind) %>%
#  saveRDS(glue::glue("./Objects/stress_input/{.x}.rds"))
#  gc()}, .progress = T)
#toc()

#### function ####

calculate_stress <- function (data, depth, percentile = 0.95) {
  
  if(anyNA(data)) { 
  
  stress <- NA                   # If we don't have data get NA, the stress function won't run
  
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
  dplyr::select(shear_mean)                                     # Keep only mean bed shear stress

stress <- quantile(stress$shear_mean, percentile)                 # Calculate the percentile over the time series
      }

  return(stress)  

}

#### looping over pixels ####

depths <- map(pairs_chunked, ~{ as.list(.[,"Depth"])})

print("Start calculations")

#tic()
#stress <- future_map(0:(length(pairs_chunked)-1), ~ {

#  cbind(readRDS(glue::glue("./Objects/Time shift/tide{.x}.rds")),
#       readRDS(glue::glue("./Objects/Time shift/wave{.x}.rds"))) %>%
#  map2(depths[[(.x+1)]], calculate_stress, percentile = 0.95)}, .progress = T)
#toc()

 tic()
 stress <- future_map(0:(length(pairs_chunked)-1), ~ {
 
   map2(readRDS(glue::glue("./Objects/stress_input/{.x}.rds")),
                depths[[(.x+1)]],
                calculate_stress, percentile = 0.95)}, .progress = T)
 toc()

print("Finished stressing")

rm(depths, pairs_chunked)

#### Create a field ####

points <- mutate(pairs, Stress95 = unlist(stress)) %>%                        # Append the results to pixels
  drop_na() %>%                                                             # Remove the influence of pixels without stress 
  select(-ends_with("_entry")) %>%                                          # Drop redundant columns
  select(-Region) %>%                                                       # We'll recreate the shore column for voronoi pixels
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>%  # Convert to sf 
  st_transform(crs = crs)

domain <- readRDS("./Objects/Domain.rds")                                   # import domain polygons

print("Start voronoi")

tic()
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
toc()
saveRDS(weighted, "./Objects/Stress95.rds")                                 # Save

print("Field successfully generated")

ggplot(weighted) + geom_sf(aes(fill = log(Stress95+1)), size = 0.001, colour = NA) +
  viridis::scale_fill_viridis(option = 2, ) +
  theme_minimal()

ggsave_map("./Figures/stress95.png", last_plot())
