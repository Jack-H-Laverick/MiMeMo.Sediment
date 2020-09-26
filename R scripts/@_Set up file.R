
## Set repeated commands specific to the project region
## This version is parameterised for the Barents sea

library(sf)

crs <- 3035                                                              # Specify the map projection for the project

W <- -45
E <- 71 
S <- 59
N <- 85

lims <- c(xmin = 4200000, xmax = 6200000, ymin = 5000000, ymax = 7000000)# Specify limits of plotting window, also used to clip data grids

zoom <- coord_sf(xlim = c(lims[["xmin"]], lims[["xmax"]]), ylim = c(lims[["ymin"]], lims[["ymax"]])) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 18, height = 10, units = "cm", dpi = 500)
  
}                             # Set a new default for saving maps in the correct size
pre <- list(scale = 1, width = 12, height = 10, units = "cm", dpi = 500) # The same settings if you need to pass them to a function in MiMeMo.tools

#### bathymetry.5 MODEL DOMAIN ####

shape <- function(matrix) {
  
shape <-  matrix %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Barents Sea",.)
  st_crs(shape) <- st_crs(4326)                                        
  shape <- st_transform(shape, crs = crs)
  return(shape)
  
}                      # Convert a matrix of lat-lons to an sf polygon

Barents_mask <- matrix(c(16.23, 70,
                        20.25, 68.5,
                        41, 66.8,
                        45.3, 65.5,
                        64, 68, 
                        57.492431, 70.736206,
                        52.984071, 71.835129,
                        54.408132, 73.261126,
                        67.9, 76.7,
                        71, 80,
                        68, 83.5,
                        0, 83,
                        0, 75,
                        16.23, 70),
                       ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Barents Sea", geometry = .)
st_crs(Barents_mask) <- st_crs(4326)                                        
#Region_mask <- st_transform(Region_mask, crs = crs)

Greenland_mask <- matrix(c(-44, 77,
                           -44, 59,
                           -42, 59,
                           -41.5, 60,
                           -18, 70,
                           -16, 72.5,
                           0, 75,
                           0, 80,
                           -8, 81.6,
                           -18, 81.3,
                           -44, 77),
                         ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Greenland", geometry = .)
st_crs(Greenland_mask) <- st_crs(4326)                                          
#Greenland_mask <- st_transform(Greenland_mask, crs = 3035)


### Chunking stress #### 

chunk_length <- 200
