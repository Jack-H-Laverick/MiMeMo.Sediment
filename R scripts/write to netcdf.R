
# Write project data to a netcdf file

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "ncdf4", "stars", "raster", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages

data <- readRDS("./Objects/Everything.rds") %>% 
  st_drop_geometry()

get_raster <- function(var) {
  
  raster <- data[,c("Latitude", "Longitude", var)] %>% 
  rasterFromXYZ()
  
  #rasterFromXYZ(dplyr::select(data, Latitude, Longitude, var))

  }

#### Write one file per variable ####

writeRaster(get_raster("Depth"),
  "./Output/Greenland_and_barents_sea_shelf_sediments.nc", overwrite = TRUE, format = "CDF", 
  varname= "Depth", longname = "GEBCO bathymetry at 0.01 deg resolution", varunit = "m",
  xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Slope"),
            "./Objects/Slope.nc", overwrite = TRUE, format = "CDF", 
            varname="Slope", longname = "Slope", varunit = "Radians",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("TPI"),
            "./Objects/TPI.nc", overwrite = TRUE, format = "CDF", 
            varname= "TPI", longname = "Topographic Position Index", varunit = "m",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("TRI"),
            "./Objects/TRI.nc", overwrite = TRUE, format = "CDF", 
            varname="TRI", longname = "Terrain Ruggedness Index", varunit = "m",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Roughness"),
            "./Objects/Roughness.nc", overwrite = TRUE, format = "CDF", 
            varname= "Roughness", longname = "Roughness", varunit = "m",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Hard"),
            "./Objects/Hard.nc", overwrite = TRUE, format = "CDF", 
            varname= "Hard", longname = "Areas of solid substrate", varunit = "Logical, 1 = True",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Gravel"),
            "./Objects/Gravel.nc", overwrite = TRUE, format = "CDF", 
            varname= "Gravel", longname = "Percent bottom cover as gravel", varunit = "%",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Sand"),
            "./Objects/Sand.nc", overwrite = TRUE, format = "CDF", 
            varname= "Sand", longname = "Percent bottom cover as sand", varunit = "%",
            xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Silt"),
            "./Objects/Silt.nc", overwrite = TRUE, format = "CDF", 
            varname= "Silt", longname = "Percent bottom cover as silt", varunit = "%",
            xname = "Longitude", yname = "Latitude")

 writeRaster(get_raster("D50"),
             "./Objects/D50.nc", overwrite = TRUE, format = "CDF", 
             varname= "D50", longname = "Mean grain size", varunit = "mm",
             xname = "Longitude", yname = "Latitude")

writeRaster(get_raster("Stress95"),
            "./Objects/BSS.nc", overwrite = TRUE, format = "CDF", 
            varname= "Stress", longname = "Bed shear stress", 
            varunit = "N.m^-2", xname = "Longitude", yname = "Latitude")

# writeRaster(get_raster("Disturbance),
#             "./Objects/ND.nc", overwrite = TRUE, format = "CDF", 
#             varname= "Disturbance", 
#             longname = "Time shields value exceeds threshold for the initiation of motion", 
#             varunit = "%", xname = "Longitude", yname = "Latitude")

## writeRaster(get_raster("Porosity"),
##             "./Objects/Porosity.nc", overwrite = TRUE, format = "CDF", 
##             varname= "Porosity", longname = "Porosity", 
##             varunit = "?", xname = "Longitude", yname = "Latitude")

## writeRaster(get_raster("Permeability"),
##             "./Objects/ND.nc", overwrite = TRUE, format = "CDF", 
##             varname= "Permeability", longname = "Permeability", 
##             varunit = "?", xname = "Longitude", yname = "Latitude")

## writeRaster(get_raster("OMC"),
##             "./Objects/OMC.nc", overwrite = TRUE, format = "CDF", 
##             varname= "OMC", longname = "Organic matter content", 
##             varunit = "?", xname = "Longitude", yname = "Latitude")

#### Combine files ####

# vars <- c("Slope", "TPI", "TRI", "Roughness", "Rock",     # List variable names used in file names
#           "Gravel", "Sand", "Silt", "D50", "BSS", "ND", 
#           "Porosity", "Permeability", "OMC") 

vars <- c("Slope", "TPI", "TRI", "Roughness", "Hard",       # List variable names used in file names
          "Gravel", "Sand", "Silt", "D50", "BSS") 
 
walk(vars, ~{
  system(str_glue("ncks -A ./Objects/{.x}.nc ./Output/Greenland_and_barents_sea_shelf_sediments.nc")) # In turn bind a variable to the main file
})                                          # Bind together all the netcdf files

unlink(str_glue("./Objects/{vars}.nc"))                     # Delete the redundant files
unlink(list.files(path = "./Output", pattern = ".tmp",      # Delete temporary files
                  full.names = TRUE))

## system("ncks -A ./Objects/ND.nc ./Output/Sediment map.nc")
### system("ncks -A ./Objects/Porosity.nc ./Output/Sediment map.nc")
### system("ncks -A ./Objects/Permeability.nc ./Output/Sediment map.nc")
### system("ncks -A ./Objects/OMC.nc ./Output/Sediment map.nc")

#raw <- nc_open("./Output/Greenland_and_barents_sea_shelf_sediments.nc")     # Check the file looks sensible
