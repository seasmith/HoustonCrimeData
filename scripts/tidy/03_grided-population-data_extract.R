library(sf)
library(tidyverse)
library(units)

# Use Houston Police beats for get necessary population points
hp_beats <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")
hp_beats <- mutate(hp_beats, Area_sq_mi = set_units(Area_sq_mi, mi^2))
# There are a lot of points (500k+) in the population data.
ref <- "data/GPW_TX"
is_east_shp <- grepl("_east$", st_layers(ref)[["name"]])
etx <- st_layers(ref)[["name"]][is_east_shp] %>% 
  paste0(".shp") %>%
  file.path(ref, .) %>%
  st_read()

hou_pop <- hp_beats %>%
 st_join(etx, left = FALSE)

fill_gaps <- function(y1, y2, var1, var2) {
 var1 <- enquo(var1)
 var2 <- enquo(var2)
 
 yrs <- seq(y1 + 1, y2 - 1)
 t_diff <- y2 - y1
 
 # functions
 r <- function(p1, p2, t) log(p2 / p1) / t
 est_pop <- function(p2, r, t) p2 * exp(r * t)
 
 yrs %>%
  map( ~quo(est_pop(p2 = !!var2,
                    r = r(!!var1, !!var2, t_diff),
                    t = (!!.x) - y1)
            )) %>%
  set_names(sprintf("UN_%s_E", yrs))
 
}

hou_pop <- hou_pop %>%
 mutate(!!!fill_gaps(2010, 2015, UN_2005_E, UN_2010_E),
        !!!fill_gaps(2015, 2020, UN_2010_E, UN_2015_E))


hou_pop <- hou_pop %>%
 as_tibble() %>%  # previously as_data_frame()
 select(-geometry) %>%
 group_by(Beats) %>%
 summarize_at(vars(matches("^UN_20[0-9]+_E$")), sum, na.rm = TRUE)

hou_pop <- hou_pop %>%
 inner_join(select(as_data_frame(hp_beats), Beats, Area_sq_mi), by = "Beats")

hou_pop <- hou_pop %>%
 pivot_longer(cols = -c(Beats, Area_sq_mi), names_to = "key", values_to = "value") %>%
 mutate(pop_den = value / Area_sq_mi) %>%
 mutate(key = str_replace(key, "_E$", "_DS")) %>%
 select(-value) %>%
 spread(key, pop_den) %>%
 select(-Area_sq_mi) %>%
 left_join(hou_pop,., by = "Beats")

save(hou_pop, file = "data/hou_pop.RData")

rm(etx)
rm(hp_beats)
rm(hou_pop)
