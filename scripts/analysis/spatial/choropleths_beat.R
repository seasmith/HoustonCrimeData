library(sf)
library(rlang)
library(leaflet)


#   -----------------------------------------------------------------------
# BEATS -------------------------------------------------------------------
#   -----------------------------------------------------------------------

b_file <- "data/Houston_Police_Beats/Houston_Police_Beats.shp"
bp <- st_read(b_file)


#   -----------------------------------------------------------------------
# MAJOR ROADS -------------------------------------------------------------
#   -----------------------------------------------------------------------

r_file <- "data/Major_Roads/Major_Roads.gdb"
roads <- st_read(r_file)
roads <- st_transform(roads, crs = st_crs(4269))
roads <- st_transform(roads, crs = st_crs(4326))

#   -----------------------------------------------------------------------
# DAILY AND YEARLY BEAT STATS ---------------------------------------------
#   -----------------------------------------------------------------------

load("data/hou_pop.RData")

hpb_daily_summ <- hou %>%
  mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses)) %>%
  group_by(Date = as.Date(Date),
           `Offense Type`,
           Beat) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup()

# Summarize the number of offense by year,
# offense type, and police beat.
hpb_yearly <- hpb_daily_summ %>%
  group_by(year = year(Date), `Offense Type`, Beat) %>%
  summarize(n_offenses = sum(n_offenses)) %>%
  ungroup()

# Get proportion of offenses in each beat.
hpb_yearly <- hpb_yearly %>%
  group_by(year, `Offense Type`) %>%
  mutate(prop_off = n_offenses / sum(n_offenses)) %>%
  ungroup()

beat_join <- expand.grid(Beat = unique(hpb_yearly$Beat),
                         year = 2010:2017,
                         `Offense Type` = unique(hpb_yearly$`Offense Type`)) %>%
  as_tibble()

pop_long <- hou_pop %>%
 gather(... = matches("^UN_20[0-9]+_E$")) %>%
 select(Beats, key, value) %>%
 rename(year = key, pop = value) %>%
 mutate(year = str_extract(year, "[0-9]+"),
        year = as.integer(year))

den_long <- hou_pop %>%
 gather(... = matches("^UN_20[0-9]+_DS$")) %>%
 select(Beats, key, value) %>%
 rename(year = key, den = value) %>%
 mutate(year = str_extract(year, "[0-9]+"),
        year = as.integer(year))

hou_pop_long <- pop_long %>%
 inner_join(den_long, by = c("Beats", "year"))

unknowns <- hpb_yearly %>%
 anti_join(hou_pop_long, by = c("Beat" = "Beats", "year")) %>%
 count(Beat)

hpb_yearly <- hpb_yearly %>%
 right_join(beat_join) %>%
 left_join(hou_pop_long, by = c("Beat" = "Beats", "year")) %>%
 mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses),
        prop_off = if_else(is.na(prop_off), 0, prop_off)) %>%
 group_by(year) %>%
 mutate(rate = (n_offenses / pop) * 10^5) %>%
 ungroup() %>%
 left_join(select(bp, Beats, geometry), by = c("Beat" = "Beats")) %>%
 st_sf()


#   -----------------------------------------------------------------------
# YEARLY VIOLENT CRIME ----------------------------------------------------
#   -----------------------------------------------------------------------

# violence <- hpb_yearly %>%
#   group_by(violent = `Offense Type` %in% c("Aggravated Assaults", "Murders",
#                                            "Rapes", "Robberies"),
#            Beat, year) %>%
#   summarize(n_offenses = sum(n_offenses, na.rm = TRUE),
#             beat_pop = mean(beat_pop),
#             area_sq_mi = mean(area_sq_mi),
#             pop_ds = mean(pop_ds),
#             geometry = geometry[1L]) %>%
#   ungroup()
# 
# violence <- violence %>%
#   mutate(rate = (n_offenses / beat_pop) * 10^5)


#   -----------------------------------------------------------------------
# ANOMALIES ---------------------------------------------------------------
#   -----------------------------------------------------------------------

# Airports:
#   * 23J50 = 
#   * 21I50 =
#
# Zero-population beats:
#   * 23J40
#   * 21I10
#   * 21I20
#   * 21I30
#   * 21I40
#   * 21I60
#   * 21I70

# Here are the above beats combined:
rm_beats <- function(threshold, measure = c("population", "density")) {
 
 if (measure[1] == "density") {
  
  filter(hou_pop,
         UN_2010_DS < !!enquo(threshold),
         UN_2017_DS < !!enquo(threshold)) %>%
   pull(Beats) %>%
   as.character()
  
  } else {
   
   filter(hou_pop,
          UN_2010_E < !!enquo(threshold),
          UN_2017_E < !!enquo(threshold)) %>%
    pull(Beats) %>%
    as.character()
   
  }
 }

# This beat is next to Hobby and skews the 'Auto Thefts': 13D30

rm_hobby <- "13D30"


#   -----------------------------------------------------------------------
# MAPS AND OTHER PLOTS ----------------------------------------------------
#   -----------------------------------------------------------------------

# ALL YEARS ---------------------------------------------------------------

## MAPS
o_type <- "Aggravated Assaults"

hpb_yearly %>%
  filter(`Offense Type` == o_type) %>%
  mutate(rate = if_else(Beat %in% c(rm_beats(100), rm_hobby), NA_real_, rate)) %>%
  ggplot() +
  geom_sf(aes(fill = rate), color = "gray10", size = 0.1) +
  geom_sf(data = roads, color = "#f5deb377", size = 0.6) +
  facet_wrap(~year) +
  scale_fill_viridis_c(na.value = "gray50") +
  coord_sf(xlim = st_bbox(hpb_yearly)[c(1, 3)],
           ylim = st_bbox(hpb_yearly)[c(2, 4)],
           datum = NA) +
  theme_dk()

## SCATTERPLOTS

hpb_yearly %>%
  filter(`Offense Type` == "Auto Thefts") %>%
  filter(!Beat %in% c(rm_beat(100), rm_hobby)) %>%
  ggplot() +
  geom_point(aes(pop_ds, rate, group = Beat)) +
  facet_wrap(~year)


# EXPERIMENTAL ------------------------------------------------------------


# population --------------------------------------------------------------


# Population per police beat

# hpb_ds %>%
#   ggplot() +
#   geom_sf(aes(fill = beat_pop)) +
#   geom_sf(data = roads, color = "#f5deb377", size = 0.6) +
#   scale_fill_viridis_c(breaks = pretty_breaks(3)) +
#   coord_sf(xlim = st_bbox(hpb_yearly)[c(1, 3)],
#            ylim = st_bbox(hpb_yearly)[c(2, 4)],
#            datum = NA) +
#   guides(fill = guide_colorbar(title.position = "top", title = "Beat Population")) +
#   theme_dk() +
#   theme(legend.position = c(0.2, 0.9),
#         legend.direction = "horizontal")



# mapbox ------------------------------------------------------------------

# choro_data_json <- geojsonio::geojson_write(choro_data %>% filter(year == 2017), file = "violence/houston/data/choro.json")
o_type <- "Aggravated Assaults"

choro_data <- hpb_yearly %>%
 filter(`Offense Type` == o_type) %>%
 mutate(rate = if_else(Beat %in% c(rm_beats(100), rm_hobby), NA_real_, rate))

leafdata <- choro_data %>% filter(year == 2017)

fills <- unique(leafdata$rate)
pal <- colorNumeric("viridis", domain = fills, na.color = "white")

pretty <- function(x) prettyNum(x, big.mark = ",")
labels <- sprintf(paste0("Police Beat: %s<br/>Offense Rate: %s<br/>",
                         "Offense Total: %s<br/>Offense Percent: %s<br/>",
                         "Population Total: %s<br/>Population Density: %s / sq mi"),
                  leafdata$Beat, pretty(round(leafdata$rate, 0)),
                  pretty(leafdata$n_offenses),
                  percent(leafdata$prop_off),
                  pretty(round(leafdata$pop, 0)),
                  pretty(round(leafdata$den, 0))) %>%
  lapply(htmltools::HTML)

(m <- leaflet(leafdata) %>%
  setView(-95.35, 29.8, 10) %>%
  # addProviderTiles("MapBox", options = providerTileOptions(
  #   id = "mapbox.dark", accessToken = getOption("mapbox_access_token")
  # ))
  addProviderTiles(providers$Esri) %>%
  addPolygons(stroke = TRUE, weight = 1, color = "gray10",
              fillOpacity = 0.5, fillColor = ~pal(rate),
              highlightOptions = highlightOptions(
                weight = 1.4, fillOpacity = 0.8, bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              )))

# google maps -------------------------------------------------------------



# Convert to kml for GoogleMaps.
#
#   May also consider leaflet: http://rstudio.github.io/leaflet/

o_type <- "Rapes"
beat_choro_data <- hpb_yearly %>%
  filter(`Offense Type` == o_type) %>%
  filter(!Beat %in% c(rm_beats(100), rm_hobby)) %>%
  filter(year == 2017)

rm_choro_data <- bp %>%
  filter(Beats %in% c(rm_beats(100), rm_hobby))

x <- beat_choro_data %>%
  ggplot() +
  geom_sf(aes(fill = rate), color = "gray10", size = 0.1) +
  geom_sf(data = rm_choro_data, fill = "gray50", color = "gray10", size = 0.1) +
  geom_sf(data = roads, color = "#f5deb377", size = 0.6) +
  # facet_wrap(~year) +
  scale_fill_viridis_c() +
  coord_sf(xlim = st_bbox(hpb_yearly)[c(1, 3)],
           ylim = st_bbox(hpb_yearly)[c(2, 4)],
           datum = NA) +
  theme_dk()

x_data <- ggplot_build(x)

x_data$data[[1]] %>%
  select(fill, geometry) %>%
  st_sf() %>%
  as("Spatial") %>%
  plotKML::kml(file = "beats.kml", colour = fill)
