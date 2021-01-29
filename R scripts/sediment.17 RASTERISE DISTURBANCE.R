
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "feather", "raster")                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

plan(multisession)

grid <- readRDS("./Objects/Water look up table.rds") %>%                    # Import pixel meta-data
  dplyr::select(x = Longitude, y = Latitude) %>%                          
  mutate(x = round(x, digits = 2),                                          # Round so that raster cells are of equal size
         y = round(y, digits = 2))

chunks <- length(list.files("./Objects/disturbance_input/"))                # Get the number of cached files to combine again 

columns <- data.frame(column = names(read_feather("./Objects/disturbance_input/daily_1.feather"))) %>% # Get the column names in a file, we want one raster per column
  separate(column, into = c("Day", "Sediment_fraction"), remove =  F, sep = "_") %>% # Get the variable name out of the column
  group_split(Sediment_fraction)                                            # Split by variable so they each go into their own netcdf file

check <- future_map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather")) %>% 
      dplyr::select(columns[[1]]$column)}, .progress =T) %>% 
   data.table::rbindlist()  
unique(check$`20030201_0.007826238`) # Does contain trues.

#### Create a file of silt disturbance ####

tictoc::tic()
silt <- future_map(columns[[1]]$column, ~{
   
   column <- as.character(.x)
   
   data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather"), columns = column)}) %>% 
      data.table::rbindlist()

   raster <- mutate(grid, z = data) %>%                                            # Attach metadata
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
   }, .progress = T) %>%                                                             # Convert to a raster
brick() %>% 
writeRaster(str_glue("./Output/Daily_disturbance_silt.nc"), overwrite = TRUE, 
            format = "CDF", varname= "Silt_ND", longname = "Is a pixel disturbed during a day", 
            varunit = "logical", xname = "Longitude", yname = "Latitude", zname = "Day")
tictoc::toc()

#### Create a file of sand disturbance ####

tictoc::tic()
sand <- future_map(columns[[2]]$column, ~{
   
   column <- as.character(.x)
   
   data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather"), columns = column)}) %>% 
      data.table::rbindlist()
   
   raster <- mutate(grid, z = data) %>%                                            # Attach metadata
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
}, .progress = T) %>%                                                             # Convert to a raster
   brick() %>% 
   writeRaster(str_glue("./Output/Daily_disturbance_sand.nc"), overwrite = TRUE, 
            format = "CDF", varname= "Sand_ND", longname = "Is a pixel disturbed during a day", 
            varunit = "logical", xname = "Longitude", yname = "Latitude", zname = "Day")
tictoc::toc()

#### Create a file of gravel disturbance ####

tictoc::tic()
gravel <- future_map(columns[[3]]$column, ~{
   
   column <- as.character(.x)
   
   data <- map(1:chunks, ~{read_feather(glue::glue("./Objects/disturbance_input/daily_{.x}.feather"), columns = column)}) %>% 
      data.table::rbindlist()
   
   raster <- mutate(grid, z = data) %>%                                            # Attach metadata
      rasterFromXYZ(crs = "+proj=longlat +datum=WGS84")
}, .progress = T) %>%                                                             # Convert to a raster
   brick() %>% 
writeRaster(str_glue("./Output/Daily_disturbance_gravel.nc"), overwrite = TRUE, 
            format = "CDF", varname= "Gravel_ND", longname = "Is a pixel disturbed during a day", 
            varunit = "logical", xname = "Longitude", yname = "Latitude", zname = "Day")
tictoc::toc()
