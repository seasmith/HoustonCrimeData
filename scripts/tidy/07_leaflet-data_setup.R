library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(sf)
library(units)
library(data.table)

load("data/hou.RData")
load("data/hou_pop.RData")
load("data/map_pol_beat_simp.RData")


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
  pivot_longer(cols = matches("^UN_20[0-9]+_E$"), names_to = "key", values_to = "value") %>%
  select(Beats, key, value) %>%
  rename(year = key, pop = value) %>%
  mutate(year = str_extract(year, "[0-9]+"),
         year = as.integer(year))

den_long <- hou_pop %>%
  pivot_longer(cols = matches("^UN_20[0-9]+_DS$"), names_to = "key", values_to = "value") %>%
  select(Beats, key, value) %>%
  rename(year = key, den = value) %>%
  mutate(year = str_extract(year, "[0-9]+"),
         year = as.integer(year))

hou_pop_long <- pop_long %>%
  inner_join(den_long, by = c("Beats", "year"))

unknowns <- hpb_yearly %>%
  anti_join(hou_pop_long, by = c("Beat" = "Beats", "year")) %>%
  count(Beat)


# INDIVIDUAL BEAT SUMMARIES -----------------------------------------------

hpb_yearly <- hpb_yearly %>%
  right_join(beat_join, by = c("year", "Offense Type", "Beat")) %>%
  left_join(hou_pop_long, by = c("Beat" = "Beats", "year")) %>%
  mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses),
         prop_off = if_else(is.na(prop_off), 0, prop_off)) %>%
  group_by(year) %>%
  mutate(rate = (n_offenses / pop) * 10^5) %>%
  ungroup() %>%
  left_join(select(map_pol_beat_simp, Beats, geometry), by = c("Beat" = "Beats")) %>%
  st_sf()

save(hpb_yearly, file = "data/hpb_yearly.RData")


# WHOLE CITY SUMMARY ------------------------------------------------------

hpb_yearly_summed <- hpb_yearly %>%
  mutate(area = st_area(geometry),
         area = set_units(area, mi^2)) %>%
  group_by(year, `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE),
            pop        = sum(pop, na.rm = TRUE),
            prop_off   = sum(prop_off, na.rm = TRUE),
            den        = pop / sum(area),
            rate       = sum((n_offenses / pop) * 10^5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Beat = "Houston") %>%
  st_set_geometry(NULL) %>%
  setDT()

save(hpb_yearly_summed, file = "data/hpb_yearly_summed.RData")


# HOURLY BEAT DATA --------------------------------------------------------

hpb_hourly <- hou %>%
  group_by(Date, hour = hour(Date), Beat, `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year(Date),
         week_day = lubridate::wday(Date, label = TRUE, week_start = 1))

save(hpb_hourly, file = "data/hpb_hourly.RData")

# SUMMARIZE HOURLY BEAT DATA BY YEAR

missing_weekdays <- expand.grid(Beat = unique(hpb_hourly$Beat),
                                `Offense Type` = unique(hpb_hourly$`Offense Type`),
                                year = 2010:2017, hour = seq(0, 23),
                                week_day = levels(hpb_hourly$week_day)) %>%
  as_tibble() %>%
  mutate(week_day = factor(week_day, levels = levels(hpb_hourly_summed$week_day), ordered = TRUE))

hpb_hourly_summed <- hpb_hourly %>%
  group_by(Beat, `Offense Type`, year, hour, week_day) %>%
  summarize(n_offenses = mean(n_offenses, na.rm = TRUE)) %>%
  ungroup()

hpb_hourly_summed <- missing_weekdays %>%
  right_join(hpb_hourly_summed, .) %>%
  mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses))

save(hpb_hourly_summed, file = "data/hpb_hourly_summed.RData")
