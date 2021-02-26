
# Calculate Organic Matter Content

#### Set up ####

rm(list=ls())

library(tidyverse)
library(betareg)

csv <- data.table::fread("./Output/Greenland_and_barents_sea_shelf_sediments.csv") # Reimport output file

data <- read.table("./Data/OMC.csv", skip = 218, header = T, sep = ",") %>%        # Import OMC skipping meta data
  .[,c(2,3,6,11)]                                                                  # Select columns of interest

names(data) <- c("Latitude", "Longitude", "Sand", "N_org")                         # Rename sensibly

relationship <- mutate(data, Silt = 1-Sand,                                        # Convert "Sand and larger" to "Silt or smaller"
                       N_org = N_org/100) %>%                                      # Convert % to proportion for betareg
  drop_na()                                                                        # Drop incomplete cases

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





#### plot ####

ggplot() +                                       # Plot
  geom_point(data = relationship, aes(x=Silt, y=N_org * 100), colour = "blue", alpha = 0.5, size = 1) +
  geom_line(data = fit, aes(x=Silt, y=N_org * 100), colour = "blue") +
  geom_point(data = NS, aes(x=Silt, y=N_org * 100), colour = "red", alpha = 0.5, size = 1) +
  geom_line(data = fit_NS, aes(x=Silt, y=N_org * 100), colour = "red") +
  annotate("text", x = 0.01, y = 0.45, colour = "red", hjust = 0,
           label = str_glue("North Sea, 
           Int = {round(model_NS$coefficients$mean[1],2)}, 
           'slope' = {round(model_NS$coefficients$mean[2],2)}
           R2 = {round(model_NS$pseudo.r.squared, 3)}")) +
  annotate("text", x = 0.2, y = 0.45, colour = "blue", hjust = 0, 
           label = str_glue("Barents Sea, 
                            Int = {round(model$coefficients$mean[1],2)}, 
                            'slope' = {round(model$coefficients$mean[2],2)}
                             R2 = {round(model$pseudo.r.squared, 3)}")) +
  theme_minimal() +
  labs(x = "Proportion silt or finer", y = "Organic nitrogen (%)")

#ggsave("./Figures/ONC fit.png")

#### Predict for the sediment map ####

csv <- csv %>%
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(ONC = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt * 100,                                                        # Convert back to percentages
         ONC = ONC * 100) 

data.table::fwrite(csv, "./Output/Greenland_and_barents_sea_shelf_sediments.csv")  # Save csv appendix

#### Also add to object destined to become netcdf file ####

look <- readRDS("./Objects/Everything.rds") %>% 
  mutate(Silt = Silt/100) %>%                                                      # Convert silt to a proportion for the model
  mutate(ONC = as.numeric(predict(model, newdata=., type='response'))) %>%         # Make predictions
  mutate(Silt = Silt*100,                                                          # Convert back to percentages
         ONC = ONC*100) %>%
  saveRDS("./Objects/Everything.rds")                                              # Save with geometry for making netcdf later 
