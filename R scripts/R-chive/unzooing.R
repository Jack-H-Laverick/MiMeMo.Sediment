
library(tidyverse)
library(tictoc)
library(furrr)

plan(multiprocess)

wave_ts <- readRDS("./Objects/Wave_ts2.rds")                                # Read in a list of time series for waves at pixels

chunked <- split(wave_ts, (seq(length(wave_ts)) -1) %/% 500) %>%   # Split into chunks of a specified length
  imap( ~ { saveRDS(.x, glue::glue("./Objects/Time shift/{.y}.rds"))})  # Save each chunk as a file so I can parallelise later
rm(chunked, wave_ts)

tic()
unzooed <- map(0:84, ~ {                                                     # For each chunk
  readRDS(glue::glue("./Objects/Time shift/{.}.rds")) %>%                   # Read in the file
  future_map(~{
      new <- as.data.frame(.x)
#      rownames(new) <- NULL
      return(new)
  },  .progress = T)
})             # Align with tide time series in parallel
toc()

saveRDS(unlist(unzooed, recursive = F), "./Objects/Wave_ts2.rds")                         # Save out list of time series

## clean up

unlink(list.files("./Objects/Time shift", full.names = T))

#### Tides ####

# tide_ts <- readRDS("./Objects/Tide_ts.rds")                                # Read in a list of time series for waves at pixels
# 
# chunked <- split(tide_ts, (seq(length(tide_ts)) -1) %/% 500) %>%   # Split into chunks of a specified length
#   imap( ~ { saveRDS(.x, glue::glue("./Objects/Time shift/{.y}.rds"))})  # Save each chunk as a file so I can parallelise later
# rm(chunked, tide_ts)
# 
# tic()
# unzooed <- map(0:8, ~ {                                                     # For each chunk
#   readRDS(glue::glue("./Objects/Time shift/{.}.rds")) %>%                   # Read in the file
#     future_map(~{
#       new <- as.data.frame(.x)
#       rownames(new) <- NULL
#       return(new)
#     },  .progress = T)
# }) %>%              # Align with tide time series in parallel
#   unlist(recursive = F)
# toc()
# 
# saveRDS(unzooed, "./Objects/Tide_ts.rds")                         # Save out list of time series
# 
# ## clean up
# 
# unlink(list.files("./Objects/Time shift", full.names = T))
# 
# 

