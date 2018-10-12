
# DEPENDENCIES ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(sf)



# LOAD DATA ---------------------------------------------------------------
# Read in data
wapo <- read_csv("data/wapo_homicides.csv")

# Load police beats and major roads
bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")



# TIDY AND AGGREGATE ------------------------------------------------------
# Coerce integer date to actuall date via character coercion
wapo <- wapo %>%
  mutate(reported_date = as.Date(as.character(reported_date), format = "%Y%M%d"))

# Combine x,y coordinates and sf geometry column
wapo <- wapo %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326), na.fail = FALSE) %>%
  {
    bind_cols(., as_tibble(st_coordinates(.))) %>%
      rename(lat = Y, lon = X)
  }

# Find those in Houston
wapo_hou <- wapo %>%
  st_join(st_sf(st_union(bp)), left = FALSE)



# WRITE TO FILE -----------------------------------------------------------
save(wapo_hou, file = "data/wapo_hou.RData")
