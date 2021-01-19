
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tictoc", "furrr")  # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

pairs <- readRDS("./Objects/Water look up table.rds")                       # Import pairings between wave and tide pixels
pairs_chunked <- split(pairs, (seq(nrow(pairs)) -1) %/% 200)                # Split into chunks of 1000 rows
depths <- map(pairs_chunked, ~{ as.list(.[,"Depth"])})

#### Get variable to indicate day ####

raw <- ncdf4::nc_open("./Objects/tides.nc")                                         # Open file containing all the data
time <- raw$dim$time$len                                                     # Pull the length of the time dimension
ncdf4::nc_close(raw)                                                                # Close file

tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = time) %>%    # Calculate time steps for the dataset
format("%Y%m%d")

#### function ####

calculate_disturbance <- function (data, depth) {
  
  D50s <- c(2, 1.03125, 0.03174)                                        # mm min-gravel size, mid-sand, max-silt
  
  if(anyNA(data)) { 
  
  stress <- t(data.frame(rep(NA , 2891*length(D50s))))                  # If we don't have data get NA, the stress function won't run
  row.names(stress) <- NULL
  stress <- as.data.frame(stress)
  col_names <- expand.grid(as.character(rev(D50s)), unique(tide_step))
  names(stress) <- paste0(col_names[,2], "_", col_names[,1])
  
  } else {                       # If we do have data calculate bed shear stress
    
  ## CALCULATE BED SHEAR STRESS ##
  
stress <- map_df(D50s, ~{                 # Calculate disturbance for each mean grain size
  
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
         tide_step = data$tide_step,
         D50 = .x) }) %>% 
  group_by(tide_step, D50) %>%  
  summarise(disturbed = any(movement == T)) %>%                          # Calculate the proportion of time water moves fast enough to initiate movement
  ungroup() %>% 
  pivot_wider(names_from = c(tide_step, D50), values_from = disturbed)

      }

  return(stress)  

}

#### looping over pixels ####

tic()

stress <- future_map(0:(length(pairs_chunked)-1), ~ {

  map2(readRDS(glue::glue("./Objects/stress_input/{.x}.rds")) %>% map(cbind, tide_step),
       depths[[(.x+1)]],
       calculate_disturbance) %>%
  data.table::rbindlist() %>%
  feather::write_feather(glue::glue("./Objects/disturbance_input/daily_{.x+1}.feather"))
  
  gc()

}, .progress = T)

toc()
