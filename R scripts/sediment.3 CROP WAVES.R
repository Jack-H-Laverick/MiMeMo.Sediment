
#### Set up ####

rm(list=ls())                                                            # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "fs", "tictoc", "furrr")      # List packages
lapply(packages, library, character.only = TRUE)                         # Load packages

plan(multiprocess)                                                       # Set parallel processing

Waves_mask <- readRDS("./Objects/ECMWF Targets.rds") %>%                 # Import locations of target pixels in ECMWF grid
  mutate(Longitude = ifelse(Longitude > 180, Longitude-360, Longitude))

all_files <- list.files("./Data/ECMWF_Waves", recursive = TRUE, full.names = TRUE, pattern = ".nc") %>%
  as.data.frame() %>%                                                    # Turn the vector into a dataframe
  separate(".", into = c("path", "file"), sep = -7) %>%   
  mutate(year = as.integer(str_sub(file, 1, 4))) %>%                     # Extract the year from the file name
  filter(year > 2002)                                                    # We only need data which overlaps with tides from SINMOD

window <- list(xmin = format(min(Waves_mask$Longitude), nsmall = 3),     # Define x boundaries for crop
               xmax = format(max(Waves_mask$Longitude), nsmall = 3),     # Formatted to force 3 dp printing to work with NCO                                            
               ymin = format(min(Waves_mask$Latitude), nsmall = 3),      # Define y boundaries for crop
               ymax = format(max(Waves_mask$Latitude), nsmall = 3))

#### FUNCTION ####

reshape_ECMWF <- function(path, file, window) {
  
#  path <- unique(all_files$path)[1] ; file <- all_files$file[1]               # test
  
  ff_new <- paste0(path, "crop", file)                                      # Create new file name
  
  temp_file1 <- tempfile("dummy", tmpdir = path)
  temp_file2 <- tempfile("dummy", tmpdir = path)
  temp_file3 <- tempfile("dummy", tmpdir = path)
  
  ## Run command line functions
  # str_glue and {} allows us to programmatically build character strings to run in Konsole.
  # Convert true degrees (0:360) to longitude (-180:180)
  system(stringr::str_glue("ncks -O --msa -d longitude,181.,360. -d longitude,0.,180.0 {paste0(path,file)} {temp_file1}"))
  system(stringr::str_glue("ncap2 -O -s 'where(longitude > 180) longitude=longitude-360' {temp_file1} {temp_file2}"))
  
  # Clip longitude and latitude dimensions (names specified in the file), from a file, saving result as new file.
  system(stringr::str_glue("ncea -d longitude,{window$xmin},{window$xmax} -d latitude,{window$ymin},{window$ymax} {temp_file2} {temp_file3}"))
  
  # Average over ensemble members (number), from a file, saving result
  system(stringr::str_glue("ncwa -a number {temp_file3} {ff_new}"))

  unlink(c(temp_file1, temp_file2, temp_file3))                           # Delete the intermediate step
  
  usethis::ui_done("{usethis::ui_field(file)} cropped.")      # Announce finished file
  
}

#### Subset files ####

tic()
future_map(all_files$file, reshape_ECMWF, path = unique(all_files$path), window = window)
toc()

## Concatenate files
# Concatenate along time dimension from the start, from SINMOD_blah.nc files, into a tides file
system(str_glue("ncrcat -d time,0, ./Data/ECMWF_Waves/crop????.nc ./Objects/waves.nc")) 

## Delete redundant subsets
cleaning <- list.files(unique(all_files$path), full.names = TRUE) %>%    # What do we have now?
  .[str_detect(., "crop")] %>%                            # list only the cropped files
  unlink()                                                # Delete

#### Clip land mask ####

system(stringr::str_glue("ncea -d longitude,{window$xmin},{window$xmax} -d latitude,{window$ymin},{window$ymax} ./Data/ECMWF_Land/land_mask.nc ./Objects/wave_land_mask.nc"))
