
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

Packages <- c("MiMeMo.tools", "tidyverse", "sf", "rnaturalearth", "viridis", "patchwork", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Set up file.R")

domains <- readRDS("./Objects/Domain.rds") %>%                  # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

Sediment <- readRDS("./Objects/modelled_sediment.rds")           # Import full sediment grid

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%  # Import quantitative values for classes
  mutate(Sed_class = as.factor(Sed_class))

world <- ne_countries(scale = "medium", returnclass = "sf")      # Get a world map

#### Transform categorical to continuous ####

Sed_quant <- left_join(Sediment, Translate) %>% 
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  sfc_as_cols() %>%                                              # For quicker plotting
  mutate(D50 = ((Gravel * 2) + (Sand * 1.03125) + (Silt * 0.03174))/ 100) # Weighted mean of the central value from grain size classes

ggplot() + 
  geom_raster(data = Sed_quant, aes(x = x, y = y, fill = Sed_class)) + 
  scale_fill_viridis(name = 'Sediment\nclass (NGU)',  discrete = TRUE) +
  theme_minimal() +
  guides(fill = guide_legend(ncol = 2)) + # Adjustments for poster plotting
  NULL
toc()

saveRDS(Sed_quant, "./Objects/Full sediment.rds")
