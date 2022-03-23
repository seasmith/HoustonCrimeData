library(tidyverse)
library(lubridate)
library(sf)
library(rlang)



# IMPORT ------------------------------------------------------------------

# load("data/hou_orig.RData") ???
load("data/hou.RData")
bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")
hou_add <- "data/US_South_Addresses/us/tx" %>%
 list.files("\\.csv$", full.names = TRUE) %>%
 grep("Greater_Houston", ., ignore.case = TRUE, value = TRUE) %>%
 read_csv()



# TIDY --------------------------------------------------------------------

# Need to do geo ops
hou_add <- hou_add %>%
 st_as_sf(crs = st_crs(4326), coords = c("LON", "LAT"), agr = "identity")

orig_cols <- names(hou_add)

# Any reduction in size helps
uni_loc <- hou_orig %>%
 filter(year(Date) > 2009) %>%
 distinct(Beat, `Block Range`, `Street Name`, Type)

# Need projections (Albers, in this case)
hou_add <- hou_add %>% st_transform(crs = st_crs(102003))
bp <- bp %>% st_transform(crs = st_crs(102003))



# JOIN --------------------------------------------------------------------

# It really helps to reduce size
hou_add <- hou_add %>%
 st_join(bp, left = FALSE)



# RE-TIDY -----------------------------------------------------------------

# Just use unprojected (for rasters)
hou_add <- hou_add %>% st_transform(crs = st_crs(4326))
bp <- bp %>% st_transform(crs = st_crs(4326))

save(hou_add, file = "data/hou_add.RData")
save(uni_loc, file = "data/uni_loc.RData")
save(orig_cols, file = "data/orig_cols.RData")
