
##**## Make maps of sediment fractions and D50

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Packages <- c("tidyverse", "sf")                                              # List handy data packages
lapply(Packages, library, character.only = TRUE)                              # Load packages

##### Shift shapes ####

#cols <- c(-7000000, 3000000, 10000000, 20000000, 30000000)
cols <- c(-10000000, 0, 10000000, 20000000, 30000000)
rows <- c(-12000000, -6000000, 0, 7500000)

domain <- readRDS("./Objects/Domain.rds")

translate <- function(shift) st_sf(st_geometry(domain)+shift, crs = 3035)

shift <- list(c(cols[2],rows[4]), c(cols[2],rows[4]+500000), c(cols[2],rows[4]+1000000), c(cols[2],rows[4]+1500000))
topo <- lapply(shift, translate)

GEBCO <- translate(c(cols[1], rows[4]+1250000))

NGU <- translate(c(cols[3], rows[4]+1250000))

shift <- list(c(cols[4],rows[3]), c(cols[4],rows[3]+500000), c(cols[4],rows[3]+1000000), c(cols[4],rows[3]+1500000))
SED <- lapply(shift, translate)

D50 <- translate(c(cols[4],rows[4]+750000))

POR <- translate(c(cols[5],rows[4]+750000))

PER <- translate(c(cols[5],rows[3]+1250000))

OMC <- translate(c(cols[5], rows[2]))

SINMOD <- translate(c(cols[1], rows[3]+750000))

ECMWF <- translate(c(cols[1], rows[2]))

BSS <- translate(c(cols[2], rows[2]))

DISTURB <- translate(c(cols[4], rows[2]))

centroid <- summarise(domain) %>% st_centroid() %>% st_geometry() %>% unlist() %>% + c(-800000, -200000)

label_position <- centroid + c(0, 2500000)
  
labels <- bind_rows(c(x =label_position[1]+cols[2], y= label_position[2]+rows[4]++1250000, label = "'Terrain Properties'"),
                c(x =label_position[1]+cols[1], y= label_position[2]+rows[4]+1250000, label = "Bathymetry"),
                c(x =label_position[1]+cols[2], y= label_position[2]+rows[2], label = "Stress"),
                c(x =label_position[1]+cols[1], y= label_position[2]+rows[2], label = "Waves"),
                c(x =label_position[1]+cols[1], y= label_position[2]+rows[3]+750000, label = "Velocities"),
                c(x =label_position[1]+cols[4], y= label_position[2]+rows[3]+1250000, label = "'Sediment Fractions'"),
                c(x =label_position[1]+cols[4], y= label_position[2]+rows[4]+1250000, label = "'D'*bar(x)"),
                c(x =label_position[1]+cols[5], y= label_position[2]+rows[4]+1250000, label = "Porosity"),
                c(x =label_position[1]+cols[5], y= label_position[2]+rows[3]+1250000, label = "Permeability"),
                c(x =label_position[1]+cols[5], y= label_position[2]+rows[2], label = "'Organic Matter'"),
                c(x =label_position[1]+cols[3], y= label_position[2]+rows[4]+1250000, label = "'Sediment Classes'"),
                c(x =label_position[1]+cols[4], y= label_position[2]+rows[2], label = "Disturbance")) %>% 
  st_as_sf(coords = c("x", "y"), crs = 3035)

Data_position <- centroid 

data <- bind_rows(c(x =Data_position[1]+cols[2], y= Data_position[2]+rows[4]+1250000, label = "Terrain Properties"),
                  c(x =Data_position[1]+cols[2], y= Data_position[2]+rows[2], label = "Stress"),
                  c(x =Data_position[1]+cols[4], y= Data_position[2]+rows[3]+1250000, label = "Fractions"),
                  c(x =Data_position[1]+cols[4], y= Data_position[2]+rows[4]+1250000, label = "D bar(x)"),
                  c(x =Data_position[1]+cols[5], y= Data_position[2]+rows[4]+1250000, label = "Porosity"),
                  c(x =Data_position[1]+cols[5], y= Data_position[2]+rows[3]+1250000, label = "Permeability"),
                  c(x =Data_position[1]+cols[5], y= Data_position[2]+rows[2], label = "'Organic Matter'"),
                  c(x =Data_position[1]+cols[4], y= Data_position[2]+rows[2], label = "Disturbance")) %>% 
  st_as_sf(coords = c("x", "y"), crs = 3035)

RF <- data.frame(x =centroid[1]+cols[3], y= centroid[2]+rows[3]+750000, 
                   label = "Random\nForest\nModel") %>% 
  st_as_sf(coords = c("x", "y"), crs = 3035)

x_adj <- 3200000
y_adj <- 2800000

links <- bind_rows(c(x = centroid[1]+cols[1]+x_adj, y = centroid[2]+rows[4]+750000, curve =0, label = "Bath-Rel",
                     xend =centroid[1]+cols[2]-x_adj, yend= centroid[2]+rows[4]+750000),
                   c(x =centroid[1]+cols[2]+x_adj, y= centroid[2]+rows[4]+750000, label = "Rel-RFM",
                     xend = centroid[1]+cols[3]-(x_adj/2), yend = centroid[2]+rows[3]+y_adj+1250000, curve = -0.5),
                   c(x = centroid[1]+cols[1]+x_adj, y= centroid[2]+rows[4]+750000, label = "Bath-RFM",
                     xend = centroid[1]+cols[3]-x_adj, yend = centroid[2]+rows[3]+1250000, curve =0.5),
                   c(x =centroid[1]+cols[1]+x_adj, y= centroid[2]+rows[4]+750000, label = "Bath-Stres",
                     xend =centroid[1]+cols[2]-(x_adj/2), yend = centroid[2]+rows[2]+y_adj, curve =0.5),
                   c(x =centroid[1]+cols[2]+x_adj, y= centroid[2]+rows[2], label = "Stres-RMF",
                     xend = centroid[1]+cols[3], yend = centroid[2]+rows[3]-y_adj, curve = 0.5),
                    c(x =centroid[1]+cols[1]+x_adj, y= centroid[2]+rows[2], label = "Waves-stres",
                     xend =centroid[1]+cols[2]-x_adj, yend = centroid[2]+rows[2], curve =0),
                   c(x = centroid[1]+cols[1], y = centroid[2]+rows[3]+750000-(y_adj/2), label = "Vel-stres",
                     xend = centroid[1]+cols[2]-x_adj, yend = centroid[2]+rows[2] +(y_adj/2), curve =0.5),
                   c(x =centroid[1]+cols[3]+x_adj, y= centroid[2]+rows[3]+1250000, label = "RFM-frac",
                     xend = centroid[1]+cols[4]-x_adj, yend = centroid[2]+rows[3]+1250000, curve =0),
                   c(x =centroid[1]+cols[4], y= centroid[2]+rows[3]+(y_adj*1.2)+1500000, label = "Frac-D50",
                     xend =centroid[1]+cols[4], yend = centroid[2]+rows[4]+1250000-(y_adj/2), curve =0),
                   c(x = centroid[1]+cols[4]+x_adj, y = centroid[2]+rows[4]+750000, curve =0, label = "D50-POR",
                     xend =centroid[1]+cols[5]-x_adj, yend= centroid[2]+rows[4]+750000),
                   c(x =centroid[1]+cols[4]+x_adj, y= centroid[2]+rows[3]+1250000, label = "frac-PER",
                     xend = centroid[1]+cols[5]-x_adj, yend = centroid[2]+rows[3]+1250000, curve =0),
                   c(x =centroid[1]+cols[4], y= centroid[2]+rows[3]-(y_adj/2), label = "Frac-dist",
                     xend =centroid[1]+cols[4], yend = centroid[2]+rows[2]+(y_adj*1.2), curve =0),
                   c(x =centroid[1]+cols[4]+x_adj, y= centroid[2]+rows[3]+1250000, label = "frac-OMC",
                     xend = centroid[1]+cols[5]-x_adj, yend = centroid[2]+rows[2] +(y_adj/2), curve = 0.5),
                   c(x =centroid[1]+cols[3], y= centroid[2]+rows[4]+750000-(y_adj/2), label = "Class-RFM",
                     xend =centroid[1]+cols[3], yend = centroid[2]+rows[3]+y_adj+1250000, curve =0),
                   c(x =centroid[1]+cols[2]+x_adj, y= centroid[2]+rows[2], label = "Stres-dist",
                     xend =centroid[1]+cols[4]-x_adj, yend = centroid[2]+rows[2], curve =0))
                   
#### Plot with data as colour ####

sed2 <- do.call(rbind, SED)
topo2 <- do.call(rbind, topo)
all <- rbind(GEBCO, D50, SINMOD, BSS, DISTURB, ECMWF, sed2, topo2, POR, PER, OMC, NGU) %>% 
  mutate(Data = c("GEBCO","GEBCO", "D50","D50", "SINMOD","SINMOD", 
                  "BSS","BSS", "DISTURB","DISTURB", "ECMWF","ECMWF",
                  "SED","SED","SED","SED","SED","SED","SED","SED",
                  "TOPO","TOPO","TOPO","TOPO","TOPO","TOPO","TOPO","TOPO", 
                  "POR", "POR", "PER", "PER", "OMC", "OMC", "NGU", "NGU"),
         Given = ifelse(Data %in% c("GEBCO", "SINMOD", "ECMWF", "NGU"), "Input", "Calculated")) %>% 
  mutate(Given = factor(Given, levels = c("Input", "Calculated")))


ggplot() + 
   geom_curve(data = filter(links, curve == "0"), aes(x=as.numeric(x), y=as.numeric(y), xend=as.numeric(xend), yend=as.numeric(yend)),
              curvature = 0, size = 0.3, arrow = arrow(length= unit(1, "mm"), angle = 20, type = "closed")) +
   geom_curve(data = filter(links, curve == "0.5"), aes(x=as.numeric(x), y=as.numeric(y), xend=as.numeric(xend), yend=as.numeric(yend)),
              curvature = 0.3, size = 0.3, arrow = arrow(length= unit(1, "mm"), angle = 20, type = "closed")) +
   geom_curve(data = filter(links, curve == "-0.5"), aes(x=as.numeric(x), y=as.numeric(y), xend=as.numeric(xend), yend=as.numeric(yend)),
              curvature = -0.3, size = 0.3, arrow = arrow(length= unit(1, "mm"), angle = 20, type = "closed")) +
  ## Data objects
  geom_sf(data = all, aes(fill = Given, alpha = Given), size = 0.1) +         
  ## RF
  geom_sf_label(data = RF, aes(label = label), size = 3, fontface = "bold", 
                label.r = unit(1, "lines"), label.padding = unit(0.5, "lines"), label.size = 1.5) +
  scale_fill_manual(values = c("midnightblue", "goldenrod")) +
  scale_alpha_manual(values = c(1, 0.5)) +
  guides(alpha = "none") +
  geom_sf_text(data = labels, aes(label = label), size = 3, parse = T) +
  theme_void() +
  labs(fill = NULL) +
  theme(text = element_text(family = "Avenir", size = 10),
        legend.position = "bottom")+
  NULL

ggsave("./Figures/Figure 2.png", width = 16, height = 8, units = "cm", dpi = 1500)
