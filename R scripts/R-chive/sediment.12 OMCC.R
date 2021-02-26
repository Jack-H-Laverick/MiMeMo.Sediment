
# Calculate Organic Matter Content

#### Set up ####

rm(list=ls())

library(tidyverse)
library(betareg)

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

data <- read.table("./Data/OMC.csv", skip = 218, header = T, sep = ",") %>%        # Import OMC skipping meta data
  .[,c(2,3,6,7)]                                                                   # Select columns of interest

names(data) <- c("Latitude", "Longitude", "Sand", "C_org")                         # Rename sensibly

relationship <- mutate(data, Silt = 1-Sand) %>%                                    # Convert "Sand and larger" to "Silt or smaller"
  drop_na()                                                                        # Drop incomplete cases

#### Fit a model for proportional data ####

model <- lm(C_org ~ Silt, relationship)                                       # Fit a beta regression

summary(model)

fit <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(C = as.numeric(predict(model, newdata=., type='response')))           # Create a line of expected values

ggplot(relationship, aes(x=Silt, y=C)) +                                       # Plot
  geom_point() +
  geom_line(data = fit) +
  theme_minimal() +
  labs(x = "Proportion silt or finer", y = "Organic carbon (%)")

#ggsave("./Figures/Carbon fit.png")

#### Predict for the sediment map ####

csv <- csv %>%
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(C = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt*100,                                                          # Convert back to percentages
         C = C*100)

data.table::fwrite(csv, "./Output/Greenland_and_barents_sea_shelf_sediments.csv")  # Save csv appendix

#### Also add to object destined to become netcdf file ####

readRDS("./Objects/Everything.rds") %>% 
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(C = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt*100,                                                          # Convert back to percentages
         C = C*100) %>% 
  saveRDS("./Objects/Everything.rds")                                              # Save with geometry for making netcdf later 
