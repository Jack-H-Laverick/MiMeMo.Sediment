
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "feather", "raster")                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multiprocess)

grid <- readRDS("./Objects/Water look up table.rds") %>%                    # Import pixel meta-data
  dplyr::select(x = Longitude, y = Latitude) %>%                          
  mutate(x = round(x, digits = 2),                                          # Round so that raster cells are of equal size
         y = round(y, digits = 2))

chunks <- length(list.files("./Objects/disturbance_input/"))                # Get the number of cached files to combine again 

columns <- data.frame(column = names(read_feather("./Objects/disturbance_input/daily_1.feather)"))) %>% # Get the column names in a file, we want one raster per column
  separate(column, into = c("Day", "Sediment_fraction"), remove =  F, sep = "_") %>% # Get the variable name out of the column
  group_split(Sediment_fraction)                                            # Split by variable so they each go into their own netcdf file

#### Create a file of silt disturbance ####

silt <- future_map(columns[[1]]$column, ~{                                  # For each column containing silt data
  
  column <- .x
  # Read in all the chunks of data from cached files in the correct order
  data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather)"), columns = column)}) %>% 
                          data.table::rbindlist()  

  raster <- mutate(grid, z = data) %>%                                      # Attach metadata
    rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")                       # Convert to a raster
  }, .progress = T) %>% 
  brick() %>%                                                               # Combine into a single cube
  setNames(columns[[1]]$Day) %>% 
  writeRaster(str_glue("./Output/Daily_disturbance_silt.nc"), overwrite = TRUE, format = "CDF", # Save out as netcdf.
               varname= "Silt_ND", longname = "Is a pixel disturbed during a day", 
               varunit = "logical", xname = "Longitude", yname = "Latitude", zname = "Day")

#### Create a file of sand disturbance ####
 
sand <- future_map(columns[[2]]$column, ~{
   
   column <- .x
   
   data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather)"), columns = column)}) %>% 
     data.table::rbindlist()  
   
   raster <- mutate(grid, z = data) %>% 
     rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
 }, .progress = T) %>% 
   brick() %>% 
   setNames(columns[[2]]$Day) %>% 
   writeRaster(str_glue("./Output/Daily_disturbance_sand.nc"), overwrite = TRUE, format = "CDF", 
               varname= "Sand_ND", longname = "Is a pixel disturbed during a day", 
               varunit = "logical", xname = "Longitude", yname = "Latitude", zname = "Day")

#### Create a file of gravel disturbance ####

gravel <- future_map(columns[[3]]$column, ~{
   
   column <- .x
   
   data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather)"), columns = column)}) %>% 
     data.table::rbindlist()  
   
   raster <- mutate(grid, z = data) %>% 
     rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
 }, .progress = T) %>% 
   brick() %>% 
   setNames(columns[[3]]$Day) %>% 
   writeRaster(str_glue("./Output/Daily_disturbance_gravel.nc"), overwrite = TRUE, format = "CDF", 
             varname= "Gravel_ND", longname = "Is a pixel disturbed during a day", 
             varunit = "Disturbed", xname = "Longitude", yname = "Latitude", zname = "Day")
