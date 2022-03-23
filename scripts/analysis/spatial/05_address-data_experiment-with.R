library(tidyverse)
library(sf)
library(rlang)

load("data/hou_add.RData")
load("data/uni_loc.RData")
load("data/orig_cols.RData")
bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")


# TIDY --------------------------------------------------------------------

# Cut down on dupes by getting finer detail
zip_beats <- hou_add %>%
 as_data_frame() %>%
 distinct(POSTCODE, Beats)

# # All possible combinations (there are a lot)
# uni_loc <- uni_loc %>%
#  right_join(zip_beats, by = c("Beat" = "Beats"))

# # Back to the original columns
# hou_add <- hou_add %>%
#  select(!!!syms(orig_cols)) %>%
#  as_tibble()

# No 'Block Range'.
# Must either extract actuall address number,
# geocode the intersection, or use some other
# method to obtain location.
no_block_range <- uni_loc %>%
 filter(!str_detect(`Block Range`, "-") | is.na(str_detect(`Block Range`, "-")))

# Need to input each value into between()
uni_loc <- uni_loc %>%
 separate(`Block Range`, c("Number_1", "Number_2"), sep = "-", remove = FALSE)
uni_loc <- uni_loc %>%
 mutate(Number_1 = if_else(Number_1 == "UNK", NA_character_, Number_1),
        Number_1 = as.integer(Number_1),
        Number_2 = as.integer(Number_2))
uni_loc <- uni_loc %>%
 mutate(Type = if_else(Type == "-", NA_character_, Type))


# Long format
uni_loc_long <- uni_loc %>%
 gather(... = Number_1:Number_2)

# MATCH TYPES -------------------------------------------------------------

uni_loc_types <- uni_loc %>% count(Type)
hou_add_types <- hou_add %>%
 mutate(TYPE = str_extract(STREET, "[A-Z]*$")) %>%
 count(TYPE)



# AN EXAMPLE:

library(ggmap)




synotts <- uni_loc %>% filter(grepl("SYNOTT", `Street Name`))
hou_add %>%
 filter(between(NUMBER, synotts[1, ]$Number_1, synotts[1, ]$Number_2) &
         grepl(synotts[1, ]$`Street Name`, STREET))

hou_add %>%
 filter(between(NUMBER, 12600, 12700) & grepl("BROOKGLADE", STREET))

# NEED TO GET THE 'CENTROID' OF THE ABOVE POINTS




# hou_add %>%
#  st_coordinates() %>%
#  as_tibble() %>%
#  bind_cols(hou_add, .) %>%
#  ggplot() +
#  ggalt::stat_bkde2d(aes(X, Y, fill = ..density..), geom = "raster", contour = FALSE, grid_size = c(100, 100)) +
#  geom_sf(data = bp, fill = "#00000000", color = "black") +
#  scale_fill_viridis_c()
