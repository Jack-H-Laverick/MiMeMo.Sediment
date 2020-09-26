
##**## Make a map of the mapped area for the sediment mapping paper

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf")                                              # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages
source("./R scripts/@_Plotting rules.R")

sea <- readRDS("./Objects/Domain.rds") %>%    # Import mapped areas for plotting
  st_transform(crs = 3035) %>% 
  mutate(label = "Model domain")

marks <- st_as_sf(drop_na(marks), coords = c("x", "y"), crs = crs) %>% 
  group_by(graticule) %>% 
  summarise(do_union = F) %>% 
  st_cast("LINESTRING") 

#### Inset ####

point <- data.frame(x= 25, y = 75, region = "inset") %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  st_transform(crs = 3035)

inset_grid <- readRDS("./Objects/inset.rds")

inset <- ggplot() +
  geom_sf(data = inset_grid, fill = "gold", colour = "black", size = 0.1) +
  theme_void() +
  NULL

#### Main map ####

map <- ggplot() +
  ggpubr::background_image(png::readPNG("./Figures/background.png")) +
  geom_sf(data = marks, aes(), colour = "grey", size = 0.2) + # Add Graticules 
  geom_sf(data = sea, fill = "gold", colour = "gold4", size = 0.05) +
  geom_sf(data = point, colour = "tan3") +                                     # Mark the zoom location
  coord_sf(ylim = c(5137274, 6864274), xlim = c(1689877, 6182403), expand = F) +
  scale_y_continuous(breaks = 60) +
  scale_x_continuous(breaks = c(-30, 0, 30)) +
#  theme_minimal() +
  theme(text = element_text(family = "Avenir", size = 10),
        panel.border = element_rect(colour = "grey", fill = NA)) +
  facet_wrap(vars(label)) +
  NULL
    
map

#### Combine ####
  
light <- data.frame(x = c(5229623, 3393143, 3899352, 5229623),                 # Create a "torch" effect for the zoom
                   y = c(5363572, 4645461, 3750766, 5363572)) %>% 
  ggplot() +                              
  geom_polygon(aes(x = x, y = y), fill = "grey", alpha = 0.5) + 
  theme_void() +
  NULL

x <- 3298965                                                                   # Specify Where we want to position the zoomed grid
y <- 4750000

final <- map +                                                                 # Start with the map
  annotation_custom(ggplotGrob(light), xmin = x, xmax = 4759200,               # Add torch
                                       ymin = y - 250000, ymax = 5804812) +
  annotation_custom(ggplotGrob(inset), xmin = x - 500000, xmax = x + 500000,   # Overlay the cropped grid
                                       ymin = y - 500000, ymax = y + 500000) +
  NULL
final

ggsave("./Figures/Figure 1.png", width = 10, height = 7, units = "cm", dpi = 1500)
