
# Calculate Organic Matter Content

#### Set up ####

rm(list=ls())

library(tidyverse)
library(betareg)
library(sf)
library(patchwork)

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

data <- read.table("./Data/OMC.csv", skip = 218, header = T, sep = ",") %>%        # Import OMC skipping meta data
  .[,c(2,3,6,11)]                                                                  # Select columns of interest

names(data) <- c("Latitude", "Longitude", "Sand", "N_org")                         # Rename sensibly

relationship <- mutate(data, Silt = 1-Sand,                                        # Convert "Sand and larger" to "Silt or smaller"
                       N_org = N_org/100) %>%                                      # Convert % to proportion for betareg
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 3035) %>% 
  st_join(readRDS("./Objects/Domain.rds")) %>% 
  drop_na() %>%                                                                        # Drop incomplete cases
  st_drop_geometry()

model <- betareg(N_org ~ Silt, relationship)                                       # Fit a beta regression

summary(model)

fit <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(N_org = as.numeric(predict(model, newdata=., type='response')))           # Create a line of expected values

## North sea

NS <- readxl::read_excel("./Data/Combined porosity POC PON data extended.xlsx") %>% 
  filter(Latitude > 51, Longitude > -2.5) %>% 
  transmute(Silt = as.numeric(Sieve_mud_content)/100,                              # Convert percent to proportion for betareg
    N_org = as.numeric(TON)/100) %>% 
  drop_na()

model_NS <- betareg(N_org ~ Silt, NS)                                              # Fit a beta regression

summary(model_NS)

fit_NS <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(N_org = as.numeric(predict(model_NS, newdata=., type='response')))           # Create a line of expected values


readxl::read_excel("./Data/Combined porosity POC PON data extended.xlsx") %>% 
  select(Latitude, Longitude) %>% 
  distinct() %>% 
  filter(Latitude > 51, Longitude > -2.5) %>% 
  ggplot() +
  geom_point(aes(x=Longitude, y= Latitude))

## 90s data ##

GL <- read.table("./Data/surface_sedimentology.tab", skip = 67, header = T, sep = "\t") %>% # Import OMC skipping meta data
  .[,c(2,3,6, 7, 13,14)]                                                          # Select columns of interest

names(GL) <- c("Latitude", "Longitude", "clay", "Silt", "TOC", "C/N")             # Rename sensibly

GLrelationship <- mutate(GL, Silt = (clay+Silt)/100,                                    # Convert "Sand and larger" to "Silt or smaller"
                       N_org = (TOC/`C/N`)/100) %>%                               # Convert % to proportion for betareg, and TOC to TON
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 3035) %>% 
  st_join(readRDS("./Objects/Domain.rds")) %>% 
  drop_na() %>%                                                                        # Drop incomplete cases
  st_drop_geometry()
  
model_GL <- betareg(N_org ~ Silt, GLrelationship)                                 # Fit a beta regression

summary(model_GL)

fit_GL <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(N_org = as.numeric(predict(model_GL, newdata=., type='response')))       # Create a line of expected values

#### All? ####

all <- bind_rows(mutate(relationship, Region = "Barents Sea"),
             mutate(GLrelationship, Region = "Greenland Sea"),
             mutate(NS, Region = "North Sea"))

model_all <- betareg(N_org ~ Silt + Region, all)                                   # Fit a beta regression

summary(model_all)

fit_all <- data.frame(Silt = rep(seq(0,1,0.01), each = 3), Region = c("Barents Sea", "Greenland Sea", "North Sea")) %>% 
  mutate(N_org = as.numeric(predict(model_all, newdata=., type='response')))       # Create a line of expected values

#### plot ####

N_plot <- ggplot() +                                       # Plot
  geom_point(data = all, aes(x=Silt, y= N_org * 100, colour = Region), size = 1, shape = 21, fill = NA) +
  geom_line(data = fit_all, aes(x=Silt, y=N_org * 100, colour = Region)) +
  annotate("text", x = 0.01, y = 0.48, colour = "firebrick", hjust = 0,
           label = str_glue("North Sea*, P < 1.64 e-06")) +
  annotate("text", x = 0.01, y = 0.43, colour = "navy", hjust = 0, 
           label = str_glue("Greenland Sea, P = 0.41")) +
  theme_minimal() +
  labs(x = NULL, y = "Organic nitrogen (%)") +
  scale_colour_manual(values = c("grey", "navy", "firebrick")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

##**## Repeat for Carbon

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

data <- read.table("./Data/OMC.csv", skip = 218, header = T, sep = ",") %>%        # Import OMC skipping meta data
  .[,c(2,3,6,7)]                                                                   # Select columns of interest

names(data) <- c("Latitude", "Longitude", "Sand", "C_org")                         # Rename sensibly

relationshipC <- mutate(data, Silt = 1-Sand,                                        # Convert "Sand and larger" to "Silt or smaller"
                       C_org = C_org/100) %>%                                      # Convert to proportion for betareg
  drop_na()                                                                        # Drop incomplete cases

modelC <- betareg(C_org ~ Silt, relationshipC)                                       # Fit a beta regression

summary(modelC)

fit <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(C_org = as.numeric(predict(modelC, newdata=., type='response')))           # Create a line of expected values

## North sea

NSC <- readxl::read_excel("./Data/Combined porosity POC PON data extended.xlsx") %>% 
  transmute(Silt = as.numeric(Sieve_mud_content)/100,                              # Convert to proportion of mud
            C_org = as.numeric(TOC)/100) %>%                                               # Convert to proportion for betareg
  drop_na()

model_NSC <- betareg(C_org ~ Silt, NSC)                                       # Fit a beta regression

summary(model_NSC)

fit_NSC <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(C_org = as.numeric(predict(model_NSC, newdata=., type='response')))           # Create a line of expected values

## 90s data ##

GL <- read.table("./Data/surface_sedimentology.tab", skip = 67, header = T, sep = "\t") %>% # Import OMC skipping meta data
  .[,c(2,3,6, 7, 13,14)]                                                          # Select columns of interest

names(GL) <- c("Latitude", "Longitude", "clay", "Silt", "TOC", "C/N")             # Rename sensibly

GLrelationshipC <- mutate(GL, Silt = (clay+Silt)/100,                                    # Convert "Sand and larger" to "Silt or smaller"
                         C_org = TOC/100) %>%                               # Convert % to proportion for betareg, and TOC to TON
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = 3035) %>% 
  st_join(readRDS("./Objects/Domain.rds")) %>% 
  drop_na() %>%                                                                        # Drop incomplete cases
  st_drop_geometry()

model_GLC <- betareg(C_org ~ Silt, GLrelationshipC)                                 # Fit a beta regression

summary(model_GLC)

fit_GLC <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(C_org = as.numeric(predict(model_GLC, newdata=., type='response')))       # Create a line of expected values


#### All Carbon ####

allC <- bind_rows(mutate(relationshipC, Region = "Barents Sea"),
                 mutate(GLrelationshipC, Region = "Greenland Sea"),
                 mutate(NSC, Region = "North Sea"))

model_allC <- betareg(C_org ~ Silt + Region, allC)                                   # Fit a beta regression

summary(model_allC)

fit_allC <- data.frame(Silt = rep(seq(0,1,0.01), each = 3), Region = c("Barents Sea", "Greenland Sea", "North Sea")) %>% 
  mutate(C_org = as.numeric(predict(model_allC, newdata=., type='response')))       # Create a line of expected values

#### plot ####

C_plot <- ggplot() +                                       # Plot
  geom_point(data = allC, aes(x=Silt, y= C_org * 100, colour = Region), size = 1, shape = 21, fill = NA) +
  geom_line(data = fit_allC, aes(x=Silt, y= C_org * 100, colour = Region)) +
  annotate("text", x = 0.01, y = 5.8, colour = "firebrick", hjust = 0,
           label = "North Sea*, P < 2 e-16") +
  annotate("text", x = 0.01, y = 5.3, colour = "navy", hjust = 0, 
           label = "Greenland Sea, P = 0.661") +
  theme_minimal() +
  labs(x = "Proportion of sediment as silt or finer", y = "Organic carbon (%)") +
  scale_colour_manual(values = c("grey", "navy", "firebrick"))

N_plot / C_plot + plot_layout(guides = "collect")

ggsave("./Figures/OMC valid.png")
