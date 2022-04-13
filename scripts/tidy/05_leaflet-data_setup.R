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

CURRENT_YEAR <- 2022


hpb_daily_summ <- hou %>%
  mutate(offense_count = if_else(is.na(offense_count), 0, offense_count)) %>%
  group_by(occurrence_date,
           offense_type,
           beat) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
  ungroup()

# Summarize the number of offense by year,
# offense type, and police beat.
hpb_yearly <- hpb_daily_summ %>%
  group_by(year = year(occurrence_date), offense_type, beat) %>%
  summarize(offense_count = sum(offense_count)) %>%
  ungroup()

# Get proportion of offenses in each beat.
hpb_yearly <- hpb_yearly %>%
  group_by(year, offense_type) %>%
  mutate(prop_off = offense_count / sum(offense_count)) %>%
  ungroup()

beat_join <- expand.grid(beat = unique(hpb_yearly$beat),
                         year = 2010:CURRENT_YEAR,
                         offense_type = unique(hpb_yearly$offense_type)) %>%
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
  anti_join(hou_pop_long, by = c("beat" = "Beats", "year")) %>%
  count(beat)


# INDIVIDUAL BEAT SUMMARIES -----------------------------------------------

hpb_yearly <- hpb_yearly %>%
  right_join(beat_join, by = c("year", "offense_type", "beat")) %>%
  left_join(hou_pop_long, by = c("beat" = "Beats", "year")) %>%
  mutate(offense_count = if_else(is.na(offense_count), 0, offense_count),
         prop_off = if_else(is.na(prop_off), 0, prop_off)) %>%
  group_by(year) %>%
  mutate(rate = (offense_count / pop) * 10^5) %>%
  ungroup() %>%
  left_join(select(map_pol_beat_simp, Beats, geometry), by = c("beat" = "Beats")) %>%
  st_sf()

save(hpb_yearly, file = "data/hpb_yearly.RData")


# WHOLE CITY SUMMARY ------------------------------------------------------

hpb_yearly_summed <- hpb_yearly %>%
  mutate(area = st_area(geometry),
         area = set_units(area, mi^2)) %>%
  group_by(year, offense_type) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE),
            pop        = sum(pop, na.rm = TRUE),
            prop_off   = sum(prop_off, na.rm = TRUE),
            den        = pop / sum(area),
            rate       = sum((offense_count / pop) * 10^5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(beat = "Houston") %>%
  st_set_geometry(NULL) %>%
  setDT()

save(hpb_yearly_summed, file = "data/hpb_yearly_summed.RData")


# HOURLY BEAT DATA --------------------------------------------------------

hpb_hourly <- hou %>%
  group_by(occurrence_date, hour = hour(occurrence_date), beat, offense_type) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year(occurrence_date),
         week_day = lubridate::wday(occurrence_date, label = TRUE, week_start = 1))

save(hpb_hourly, file = "data/hpb_hourly.RData")

# SUMMARIZE HOURLY BEAT DATA BY YEAR

hpb_hourly_summed <- hpb_hourly %>%
  group_by(beat, offense_type, year, hour, week_day) %>%
  summarize(offense_count = mean(offense_count, na.rm = TRUE)) %>%
  ungroup()

missing_weekdays <- expand.grid(beat = unique(hpb_hourly$beat),
                                offense_type = unique(hpb_hourly$offense_type),
                                year = 2010:CURRENT_YEAR, hour = seq(0, 23),
                                week_day = levels(hpb_hourly$week_day)) %>%
  as_tibble() %>%
  mutate(week_day = factor(week_day, levels = levels(hpb_hourly_summed$week_day), ordered = TRUE))

hpb_hourly_summed <- missing_weekdays %>%
  right_join(hpb_hourly_summed, .) %>%
  mutate(offense_count = if_else(is.na(offense_count), 0, offense_count))

save(hpb_hourly_summed, file = "data/hpb_hourly_summed.RData")
