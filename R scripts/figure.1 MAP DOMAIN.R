
##**## Make a map of the mapped area for the sediment mapping paper

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf")                                              # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

land <- readRDS("./Objects/Land.rds") %>% st_transform(crs = 3035)            # Import land for plotting

sea <- readRDS("./Objects/Domain.rds") %>% st_transform(crs = 3035)           # Import mapped areas for plotting

#cells <- readRDS("./Objects/Extracters.rds")                                  # Import cells for zoom to show domain boundary

#### Inset ####

point <- data.frame(x= 25, y = 75, region = "inset") %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  st_transform(crs = 3035)

inset_grid <- readRDS("./Objects/inset.rds")

inset <- ggplot() +
  geom_sf(data = inset_grid, fill = "yellow", colour = "black", size = 0.1) +
  theme_void() +
  NULL

#### Main map ####

box <- st_bbox(sea)
    
map <- ggplot() +
  geom_sf(data = land, fill = "black", size = 0.05) +
  geom_sf(data = sea, fill = "yellow", colour = "yellow3", size = 0.05) +
  geom_sf(data = point, colour = "tan3") +                                     # Mark the zoom location
  coord_sf(xlim = c(box$xmin, box$xmax), ylim = c(box$ymin, box$ymax)) +
  theme_minimal() +
  theme(text = element_text(family = "Avenir", size = 10),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA)
  ) +
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

#ggsave("./Figures/Figure 1.png", width = 10, height = 7, units = "cm", dpi = 1500)
