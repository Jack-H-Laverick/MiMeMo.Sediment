
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tictoc", "furrr", "sf", "data.table", "raster")  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Set up file.R")

plan(multiprocess)

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels
pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 200)                # Split into chunks of 1000 rows

#### function ####

calculate_disturbance <- function (data, depth) {
  
  D50s <- c(0.02, 0.0103125, 0.0003174)
  
  if(anyNA(data)) { 
  
  stress <- t(data.frame(rep(NA , 12*length(D50s))))                  # If we don't have data get NA, the stress function won't run
  row.names(stress) <- NULL
  stress <- as.data.frame(stress)
  col_names <- expand.grid(as.character(rev(D50s)), 1:12)
  names(stress) <- paste0(col_names[,2], "_", col_names[,1])
  
  } else {                       # If we do have data calculate bed shear stress
    
  ## CALCULATE BED SHEAR STRESS ##
  
stress <- map_df(c(0.02, 0.0103125, 0.0003174), ~{                 # Calculate disturbance for each mean grain size
  
  bedshear::shear_stress(
  bathymetry = depth,                                              # Depth to sea floor
  D50 = .x,                                                        # Nominal median grain size
  tidal_velocity = data$uvSpeed,                                   # Water movements
  tidal_direction = data$uvDirection,
  wave_height = data$swh,
  wave_period = data$mwp,
  wave_direction = data$mwd/10,
  switch = 0) %>%
  dplyr::select(shields_number, shields_critical) %>%             # Keep only what we need to calculate natural disturbance
  mutate(movement = shields_number > shields_critical,            # When is dimensionless shear stress larger than shear stress required to initiate motion
         month = data$month,
         D50 = .x) }) %>% 
  group_by(month, D50) %>%  
  summarise(stress = mean(movement)) %>%                          # Calculate the proportion of time water moves fast enough to initiate movement
  ungroup() %>% 
  pivot_wider(names_from = c(month, D50), values_from = stress)

      }

  return(stress)  

}

#### looping over pixels ####

depths <- map(pairs_chunked, ~{ as.list(.[,"Depth"])})

tic()
stress <- future_map(0:(length(pairs_chunked)-1), ~ { 

  map2(readRDS(glue::glue("./Objects/stress_input/{.x}.rds")),
               depths[[(.x+1)]],
               calculate_disturbance)}, .progress = T) %>% 
  unlist(recursive = F) %>% 
  rbindlist()
toc()

#### Create a field ####

points <- cbind(pairs, stress) %>%                                          # Append the results to pixels
  drop_na() %>%                                                             # Remove the influence of pixels without stress 
  dplyr::select(-ends_with("_entry")) %>%                                   # Drop redundant columns
  dplyr::select(-Region) %>%                                                # We'll recreate the shore column for voronoi pixels
  sf::st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326) %>%  # Convert to sf 
  st_transform(crs = crs)

voronoi <- st_join(readRDS("./Objects/Stress95.rds"), points) %>% 
  .[,-(1:9)]

#ggplot(voronoi) + geom_sf(aes(fill = log(`1_0.0003174`+1)), size = 0.001, colour = NA) + # visual check
#  viridis::scale_fill_viridis(option = 2) +
#  theme_minimal()

#### Weight by sediment map to get average disturbance per pixel ####

base <- st_join(st_as_sf(fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv"), coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326),                                                   # Join previous data
                 st_transform(voronoi, crs = 4326))  %>%                   # Sample at the points from our final csv file
  st_drop_geometry() %>% 
  dplyr::select(-c(TRI:Stress95, Hard, D50)) %>%                           # Drop unnecessary columns
  drop_na()                                                                # Drop any empty pixels

weights <- dplyr::select(base, Longitude:Silt) %>%                         # Select just the predicted sediment fractions
  pivot_longer(cols = !starts_with("L"), names_to = "Fraction",            # reshape for data join 
               values_to = "Weight") 

bound <- dplyr::select(base, -c(Gravel:Silt)) %>%                        
  pivot_longer(cols = !starts_with("L"),                                   # Reshape disturbance for data join
             names_sep = "_", names_to = c("Month", "Fraction"), values_to = "Disturbance") %>% 
  mutate(Fraction = case_when(Fraction == 0.02 ~ "Gravel",                 # Relabel values for data join
                              Fraction == 0.0103125 ~ "Sand",
                              Fraction == 0.0003174 ~ "Silt")) %>% 
  left_join(weights)                                                    
setDT(bound)                                                               # Convert to a data.table for speed

csv <- bound[, .(Natural_disturbance = weighted.mean(Disturbance, Weight)),# Quickly calculate mean disturbance weighted by sediment fraction
               by = .(Latitude, Longitude, Month)] %>%                     # per pixel and month 
  mutate(Month = month.name[as.numeric(Month)],                            # And get month name
         Natural_disturbance = replace_na(Natural_disturbance, replace = 0)) # Replace NAs with 0 (These come from hard substrate when the other sediment weights are c(0,0,0)) )

fwrite(csv, "./Output/Seasonal.csv")
  
cdf <- pivot_wider(csv, values_from = Natural_disturbance, names_from = Month)  # Get the data for each raster layer as a column

ggplot(cdf)+ geom_raster(aes(x = Longitude, y = Latitude, fill = January)) # Visual check

#### Save output ####

get_raster <- function(var, data) {
  
  raster <- data[,c("Longitude", "Latitude", var)] %>% 
    rasterFromXYZ()
  
}

layers <- map(as.list(month.name), get_raster, cdf) %>%                    # Reformat as raster layers
  brick() 

#plot(layers)                                                              # Visual check
  
writeRaster(layers,
            "./Output/Greenland_and_barents_sea_seasonal_disturbance.nc", overwrite = TRUE, format = "CDF", 
             varname= "Natural_disturbance", 
             longname = "Time shields value exceeds threshold for the initiation of motion, weighted by sediment fractions", 
             varunit = "Proportion", xname = "Longitude", yname = "Latitude", zname = "Month")
