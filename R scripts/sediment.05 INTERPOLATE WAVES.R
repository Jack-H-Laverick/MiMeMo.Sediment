
#### Set up ####

rm(list=ls())                                                                # Wipe the brain

packages <- c("MiMeMo.tools", "raster", "ncdf4", "furrr", "largeList")       # List packages
lapply(packages, library, character.only = TRUE)                             # Load packages

plan(multisession)

#### Tides ####

raw <- nc_open("./Objects/tides.nc")                                         # Open file containing all the data
time <- raw$dim$time$len                                                     # Pull the length of the time dimension
nc_close(raw)                                                                # Close file

tide_step <- seq(ISOdate(2003, 02, 1, 0), by = "2 hours", length.out = time) # Calculate time steps for the dataset

tide_dummy <- zoo::zoo(rep(1, length.out = time), tide_step)                 # Dummy tide time series to align to

rm(time, tide_step)                                                          # Clear memory

#### Waves ####

raw <- nc_open("./Objects/waves.nc")                                         # Open file containing all the data
time <- raw$dim$time$len
nc_close(raw)                                                                # Close file

wave_step <- seq(ISOdate(2003, 01, 1, 0), by = "3 hours", length.out = time*28) # Calculate time steps for the dataset

rm(time, raw)

# Do not map over file names! the chunks must be processed 
# in numerical not alphabetical order. 10 must come after 9 not 1

interp_time <- function(wave_ts, wave_step, tide) {

  if(anyNA(wave_ts[,1])) {                                 # If we don't have data get NA
    
    align <- data.frame(mwd = rep(NA, length(tide)),
                        swh = rep(NA, length(tide)),
                        mwp = rep(NA, length(tide)))
    
  } else {                       # If we do have data calculate bed shear stress
    
  wave_ts <- zoo::zoo(wave_ts, wave_step)                                   # Add time steps and convert to zoo object

  align <- zoo::merge.zoo(tide, wave_ts, all = c(T,T))                      # Match up time series (identify missing steps)

  align$mwd <- zoo::na.approx(align$mwd, rule = 2)                          # Interpolate wave direction onto tide time steps
  align$swh <- zoo::na.approx(align$swh, rule = 2)                          # Interpolate wave height onto tide time steps
  align$mwp <- zoo::na.approx(align$mwp, rule = 2)                          # Interpolate wave period onto tide time steps

  align <- align[which(zoo::index(align) %in% zoo::index(tide)), !names(align) == "tide"] %>%  # Keep only tide time steps
    as.data.frame()
}}                      # Interpolate waves to tides

tic()
zooed <- map(1:7, safely(~ {                                               # For each chunk
  readRDS(stringr::str_glue("./Objects/Time shift/{.x}.rds")) %>%          # Read in the file
  future_map(interp_time, wave_step, tide_dummy, .progress = T) %>% 
  saveRDS(stringr::str_glue("./Objects/Wave_ts_{.x}.rds"))}))              # Align with tide time series in parallel
toc()
 
saveList(object = readRDS("./Objects/Wave_ts_1.rds"), file = "./Objects/Wave_ts.llo", append = FALSE, compress = TRUE)
 
walk(stringr::str_glue("./Objects/Wave_ts_{2:7}.rds"), ~{ 
  saveList(object = readRDS(.x), file = "./Objects/Wave_ts.llo", append = TRUE)})
 