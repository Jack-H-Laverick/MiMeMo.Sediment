
library(tidyverse)

crs <- 3035

graticule <- function(type, value, crs) {
  
  if(type == "Latitude") points <- data.frame(Longitude = -180:180, Latitude = value, graticule = value)
  if(type == "Longitude") points <- data.frame(Longitude = value, Latitude = -90:90, graticule = value) 
  
  line <- sf::st_as_sf(points, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    sf::st_transform(crs = crs) %>% 
    MiMeMo.tools::sfc_as_cols() %>% 
    sf::st_drop_geometry()
  
  return(line)
}   # Create graticules gor ggplots

marks <- data.frame(type = c(rep("Longitude", 13), rep("Latitude", 10)),           # Specify some coordinates I'd like to highlight
                    value = c(seq(-180, 180, by = 30), seq(0, 90, by = 10))) %>% 
  purrr::pmap_df(graticule, crs = crs)                                            # Create graticules 

sediment_aes <- list(
  scale_y_continuous(breaks = 5403549, labels = "60 N"),
  scale_x_continuous(breaks = c(2402768, 3902768, 5202768), labels = c("30 W", "0", "30 E")),
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(colour = "grey", fill = NA),
        text = element_text(family = "Avenir", size = 10)),
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1)),
  coord_equal(ylim = c(5136897, 6872697),
              xlim = c(1680281, 6182801), expand = F),
  labs(x = NULL, y = NULL))
