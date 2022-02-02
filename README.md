<img src='./Figures/rayshade-hi.png' align="center" width="100%" />

# MiMeMo.Sediment

This page details at a broad level the code written to create synthetic
sediment maps of the Barents and Greenland sea. These are going to be
used to parameterise StrathE2E*Polar* as part of MiMeMo, but other
people may find them useful too\!

In brief, wave action, water velocities, and bathymetry are fed into a
random forest model. The model is trained on an overview document of the
Barents Sea sediments, created by the Norwegian geological survey in partnership 
with the Russian federal state unitarian research and production company for 
geological sea survey (SEVMORGEO). NGU-SEVMORGEO sediment classes are then 
predicted for the mapped area, before calculating percent of different sediment 
fractions, mean grain size, porosity, permeability, organic matter content, 
and mean daily disturbance by month.

## Script Objectives

| Script                       | Purpose                                                                      |
| ---------------------------- | ---------------------------------------------------------------------------- |
| 00 DOMAINS                   | Define the extent of the mapped area.                                        |
| 01 GRID WATER                | Build masks to subset SINMOD and ECMWF data.                                 |
| 02 CROP SINMOD               | Clip data to model domain and concatenate files in time.                     |
| 03 CROP WAVES                | Clip data to model domain and concatenate files in time.                     |
| 04 WATER TS                  | Align ECMWF and SINMOD in space, convert to time series.                     |
| 05 INTERPOLATE WAVES         | Align unique wave time series with water velocities.                         |
| 06 EXPAND                    | Copy wave time series so they are paired with each velocity time series.     |
| 07 SHEARSTRESS               | Calculate 95th percentile of bed shear stress over the grid through time.    |
| 08 RF PREDICTORS             | Sample predictor fields at points to predict for the random forest model.    |
| 09 H2O                       | Fit a random forest model to predict NGU sediment classes.                   |
| 10 SEDIMENT FRACTIONS        | Translate sediment classes into percentages of sediment fractions.           |
| 11 WRITE CSV                 | Save out predictions static data products as a .csv file.                    |
| 12 NITROGEN CONTENT          | Predict organic nitrogen content of sediments using % silt.                  |
| 13 CARBON CONTENT            | Predict organic carbon content of sediments using % silt.                    |
| 14 PERM PORE                 | Predict sediment porosity and permeability using mean sediment diameter.     |
| 15 WRITE NETCDF              | Save static layers as a NetCDF file.                                         |
| 16 SEASONAL STRESS           | Calculate mean 95th percentile of bed shear stress per month.                |
| 17 DAILY DISTURBANCE         | Calculate whether pixels are physically disturbed per day.                   |
| 18 RASTERISE DISTURBANCE     | Collect distributed estimates of disturbance into a NetCDF file.             |
| 19 SEASONAL DISTURBANCE      | Calculate mean proportion of days in a month pixels are disturbed, by month. |
| 20 ZONAL SUMMARIES           | Calculate summary statistics for each region to report in the manuscript.    |
| FIGURES                      |                                                                              |
| 01 MAP DOMAIN                | Plot the mapped area.                                                        |
| 02 SCHEMATIC                 | Plot the relationships between data objects and project outputs.             |
| 04 D50                       | Map mean grain size.                                                         |
| 05 SEDIMENT FRACTIONS        | Map the proportions of different sediment fractions.                         |
| 06 PERMEABILITY AND POROSITY | Map porosity and permeability.                                               |
| 07 OMCS                      | Map organic carbon and nitrogen content.                                     |
| 08 STRESS AND DISTURBANCE    | Map bed shear stress and sediment disturbance.                               |
| supp OMC                     | Compare organic matter field data for the Barents, North, and Greenland Seas.|
| supp SEASONAL                | Map the seasonal cycles of bed shear stress and sediment disturbance.        |

