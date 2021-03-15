
# Create a random forest model to predict NGU sediment classes from bathymetry and shear stress

#### Set up ####

rm(list=ls())

Packages <- c("tidyverse", "sf", "h2o", "viridis", "patchwork", "tictoc") # List packages
lapply(Packages, library, character.only = TRUE)                 # Load packages
source("./R scripts/@_Set up file.R")

h2o.init(max_mem_size = "30G")                                   # Start running h2o in the background

domains <- readRDS("./Objects/Domain.rds") %>%                   # Load SF polygons of the MiMeMo model domains
  st_transform(crs = 4326)                                       # Transform to Lat/Lon to match other objects

 Data <- readRDS("./Objects/RF_sediment_observations.rds") %>%    # Read in data
   drop_na() %>%                                                  # Drop point without all estimates
   mutate(Sed_class = as.factor(Sed_class)) 

#### check by plotting ####

ggplot(Data) +
  geom_sf(aes(colour = Stress95), size = 0.1)

#### Split into training and validation ####

Training <- Data %>% 
  st_drop_geometry %>% 
  group_by(Sed_class) %>% 
  sample_frac(0.7) %>% 
  ungroup %>% 
  dplyr::select(-c(Region))

Validation <- Data %>% 
  st_drop_geometry %>% 
  dplyr::select(-c(Region)) %>% 
  anti_join(Training)

#### Points to predict for ####

To_predict <- readRDS("./Objects/RF_sediment_observations.rds") %>%    # Read in data
  filter(is.na(Sed_class)) %>%                                         # Limit to points we don't know about sediment
  dplyr::select(-Sed_class) %>%                                        # Drop the sediment columns so we can drop NAs
  drop_na() 

#### Build model ####

tic()
RF <- h2o.randomForest(y = "Sed_class",                         # Predict Sediments
                       training_frame = as.h2o(Training),                  
                       validation_frame = as.h2o(Validation),
                       ntrees = 500,
                       seed = 5678)                             # Set seed to ensure reproducibility
toc()

#h2o.saveModel(RF, "./Objects/Sediment modelh2o")

#### Predict at unknown locations ####

tic()
prediction <- h2o.predict(RF, as.h2o(dplyr::select(st_drop_geometry(To_predict), -Region))) %>%  # Predict in h2o
  as.data.frame() %>%                                                    # Copy results into R
  bind_cols(dplyr::select(To_predict, Region)) %>%                              # Add point geometries
  st_as_sf() %>%                                                         # Reinstate SF class
  rename(Sed_class = predict)
toc()

#### Importance of predictors ####

Importance <- h2o.varimp(RF)

ggplot() + 
  geom_point(data = Importance, aes(y = scaled_importance, x = reorder(variable, scaled_importance)), colour = "grey", 
             shape = 21, size = 5, fill = "white", stroke = 0.5) +
  theme_minimal() +
  labs(y = "Scaled Importance", x = "Predictor") +
  ylim(0,1) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey", size = 0.25), axis.text.x = element_text(angle = 90),
        text = element_text(family = "Avenir", size = 10)) +
  NULL

ggsave("./Figures/Importance.png")

#### Condense sediment classes ####

Hard_levels <- list(Hard = c(1, 175, 180, 185, 300), 
                    Soft = c(10, 20, 40, 80, 100, 115, 120, 130, 150, 160, 170))

Grav_levels <- list("0" = c(175, 180, 185, 206, 300, 1),
                    "<2" = c(10, 20, 40, 80, 100),
                    "<10" = 160,
                    "2-30" = c(115, 120, 130),
                    "30-80" = 150,
                    ">80" = 170)

Sand_levels <- list("0" = c(170, 175, 180, 185, 206, 300, 1),
                    "<10" = c(10, 20),
                    ">22.5" = 150,
                    "<45" = 115,
                    ">45" = 120,
                    "<50" = c(40, 80),
                    ">76" = 130,
                    "<80" = 160,
                    ">90" = 100)

Silt_levels <- list("0" = c(170, 175, 180, 185, 206, 300, 1),
                    "<9" = 130,
                    "<10" = c(100,160),
                    "<22.5" = 150,
                    "<45" = 120,
                    ">45" = 115,
                    "<50" = 80,
                    ">50" = 40,
                    ">90" = c(10, 20))

#### Condensed errors ####

Fraction_error <- function(Confusion, coding, Fraction) {
  
  Condensed_table <- Confusion %>% 
    dplyr::select(-c(Error, Rate)) %>% 
    rownames_to_column(var = "Actual") %>% 
    filter(Actual != "Totals") %>% 
    pivot_longer(-Actual, names_to = "Predicted", values_to = "Count") %>%
    mutate(Actual = as.factor(Actual), 
           Predicted = as.factor(Predicted))

  levels(Condensed_table$Actual) <- coding 
  levels(Condensed_table$Predicted) <- coding

  Condensed_table %>% 
    group_by(Actual, Predicted) %>% 
    summarise(count = sum(Count)) %>% 
    ungroup() -> Condensed_table
    
  Fraction_error <- uncount(Condensed_table, count) %>% 
    mutate(Match = Actual == Predicted)              # Proportion of times right (estimate of error rate)                  
    
  
  Condensed_table <- mutate(Condensed_table,
                            Fraction = Fraction,
                            Error = mean(Fraction_error$Match))
    } # Extract the sediment fraction error from a confusion matrix

errors <- map2_dfr(list(Hard_levels, Grav_levels, Sand_levels, Silt_levels),# For each set of levels contributing to a fraction
               c("Hard", "Gravel", "Sand", "Silt"), Fraction_error,         # Calculate the condensed error
               Confusion = h2o.confusionMatrix(RF, valid = TRUE))           # From the random forest output

new <- group_by(errors, Actual, Fraction) %>% 
  transmute(Predicted, Percent = count/sum(count), Error) %>%
  ungroup() %>% 
  mutate(Fraction = ifelse(Fraction == "Hard", "Rock", Fraction)) %>% 
  mutate(Fraction = str_glue("{Fraction} ({round(Error*100, digits = 1)}%)"),
         Actual = paste0(Actual, "%"),
         Predicted = paste0(Predicted, "%")) %>%
  mutate(Actual = ifelse(Actual == "Soft%", "False", ifelse(Actual == "Hard%", "True", Actual)),
         Predicted = ifelse(Predicted == "Soft%", "False", ifelse(Predicted == "Hard%", "True", Predicted))) 
  

new$Predicted <- factor(new$Predicted, levels = c("True", "False", "0%", "<2%", "<9%", "<10%", "2-30%", 
                                                  "<22.5%", ">22.5%", "30-80%",  "<45%", ">45%", "<50%", ">50%",  
                                                  ">76%",  "<80%", ">80%", ">90%"),
                        labels = c("True", "False", "0%", "<2%", "<9%", "<10%", "2-30%", 
                                   "<22.5%", ">22.5%", "30-80%",  "<45%", ">45%", "<50%", ">50%",  
                                   ">76%",  "<80%", ">80%", ">90%"))

new$Actual <- factor(new$Actual, levels = c("True", "False", "0%", "<2%", "<9%", "<10%", "2-30%", 
                                            "<22.5%", ">22.5%", "30-80%",  "<45%", ">45%", "<50%", ">50%", 
                                            ">76%",  "<80%", ">80%", ">90%"),
                        labels = c("True", "False", "0%", "<2%", "<9%", "<10%", "2-30%", 
                                   "<22.5%", ">22.5%", "30-80%",  "<45%", ">45%", "<50%", ">50%",  
                                   ">76%",  "<80%", ">80%", ">90%"))

ggplot() +
  geom_raster(data = new, aes(y = Actual, x = Predicted, fill = Percent * 100)) +
  viridis::scale_fill_viridis(name = "Percent of predictions", option = "E",
                              trans = "sqrt", breaks = c(0, 25, 50, 75), labels = c("0%", "25%", "50%", "75%")) +               # Specify fill
  facet_wrap(vars(Fraction), nrow = 2, scales = "free") +
  theme(panel.background = element_rect(fill = NULL),
        text = element_text(family = "Avenir", size = 10),
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5, barwidth = 15, title.vjust = 1)) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) + # remove white space around rasters
  scale_y_discrete(expand = expansion(mult = c(0, 0))) +
  NULL

ggsave("./Figures/Figure 3 fit.png", plot = last_plot(), scale = 1, width = 14, height = 13, units = "cm", dpi = 1500)

#### Full sediment map ####

Sediment <- rbind(                                       # Bind
  dplyr::select(Data, Sed_class),                        # Sediment observations
  dplyr::select(prediction, Sed_class))                  # To sediment predictions

saveRDS(Sediment, "./Objects/modelled_sediment.rds")

h2o.shutdown(prompt = FALSE)                                                # Close h2o, otherwise you'll hit memory limits later

