
# Convert NGU classess to 8 StrathE2E habitat types

#### Set up ####

rm(list=ls())

library(MiMeMo.tools) # List packages
source("./R scripts/@_Set up file.R")

domains <- readRDS("./Objects/Domain.rds") %>%                   # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

Sediment <- readRDS("./Objects/modelled_sediment.rds")           # Import full sediment grid

Translate <- read.csv("./Data/Sediment nominal values.csv") %>%  # Import quantitative values for classes
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  rowwise() %>% 
  mutate(D50 = 10^weighted.mean(c(log10(11.31371), log10(0.3535534), log10(0.007826238)), 
                                c(Gravel,Sand,Silt))) %>%        # Weighted mean of the central value from grain size classes
ungroup()

#### Transform categorical to continuous ####

Sed_quant <- left_join(Sediment, Translate) %>% 
  mutate(Sed_class = as.factor(Sed_class)) %>% 
  sfc_as_cols()                                                  # For quicker plotting

ggplot() + 
  geom_raster(data = Sed_quant, aes(x = x, y = y, fill = D50)) + 
  viridis::scale_fill_viridis(name = 'mean grain size (NGU)', trans = "log") +
  theme_minimal() +
  NULL
toc()

saveRDS(Sed_quant, "./Objects/Full sediment.rds")
