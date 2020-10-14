
# Calculate Organic Matter Content

#### Set up ####

rm(list=ls())

library("tidyverse")

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

data <- read.table("./Data/OMC.csv", skip = 218, header = T, sep = ",") %>%        # Import OMC skipping meta data
  .[,c(2,3,6,11)]                                                                  # Select columns of interest

names(data) <- c("Latitude", "Longitude", "Sand", "N_org")                         # Rename sensibly

relationship <- mutate(data, Silt = 1-Sand) %>%                                    # Convert "Sand and larger" to "Silt or smaller"
  drop_na()                                                                        # Drop incomplete cases

#### Fit a model for proportional data ####

model <- betareg(N_org ~ Silt, relationship)                                       # Fit a beta regression

summary(model)

fit <- data.frame(Silt = seq(0,1,0.01)) %>% 
  mutate(N_org = as.numeric(predict(model, newdata=., type='response')))           # Create a line of expected values

ggplot(relationship, aes(x=Silt, y=N_org)) +                                       # Plot
  geom_point() +
  geom_line(data = fit) +
  theme_minimal()

#### Predict for the sediment map ####

csv <- csv %>%
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(OMC = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt*100,                                                          # Convert back to percentages
         OMC = OMC*100)

data.table::fwrite(csv, "./Output/Greenland_and_barents_sea_shelf_sediments.csv")  # Save csv appendix

#### Also add to object destined to become netcdf file ####

readRDS("./Objects/Everything.rds") %>% 
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(OMC = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt*100,                                                          # Convert back to percentages
         OMC = OMC*100) %>% 
  saveRDS("./Objects/Everything.rds")                                              # Save with geometry for making netcdf later 
