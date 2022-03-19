
# DEPENDENCIES ------------------------------------------------------------
library(pals)
library(tidyverse)
library(lubridate)
library(scico)
library(vapeplot)
library(sf)
library(magick)
library(extrafont); loadfonts("win", quiet = TRUE)
library(scales)


# LOAD DATA ---------------------------------------------------------------
# Load WaPo Houston data
load("data/wapo_hou.RData")

# Load City of Houston data
load("data/hou.RData")

# Load police beats and major roads
bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")
mr <- st_read("data/Major_Roads/Major_Roads.gdb")


# TIDY --------------------------------------------------------------------
# Return only murders
hou <- hou %>%
  filter(offense_count > 0 & `Offense Type` == "Murders")

# Coerce `Date` to class Date
hou <- hou %>%
  mutate_at(vars(Date), as.Date)

# Remove uneeded columns
bp <- bp %>%
  select(Beats, geometry)

wapo_hou_summ <- bp %>%
  st_join(wapo_hou, left = FALSE) %>%
  st_set_geometry(NULL) %>%
  group_by(Beats) %>%
  summarize(n_total = n(),
            n_black = sum(victim_race == "Black"),
            n_hisp  = sum(victim_race  == "Hispanic") / n_total,
            n_white = sum(victim_race == "White") / n_total,
            n_asian = sum(victim_race == "Asian") / n_total,
            n_other = sum(victim_race == "Other") / n_total,
            pct_black = n_black  / n_total,
            pct_hisp = n_hisp  / n_total,
            pct_white = n_white  / n_total,
            pct_asian = n_asian  / n_total,
            pct_other = n_other  / n_total) %>%
  left_join(bp, ., by = "Beats")

# Return only 'Interstate' and 'Beltway'
mr <- mr %>%
  filter(Road_Category %in% c("Interstate", "Beltway")) %>%
  st_transform(crs = st_crs(4326)) %>%
  st_crop(st_bbox(bp))

# PLOTS -------------------------------------------------------------------
title_lookup <- c(pct_asian = "Asian", pct_black = "Black", pct_hisp = "Hispanic",
                  pct_other = "Other", pct_white = "White")

plts <- wapo_hou_summ %>%
  select(-starts_with("n_")) %>%
  gather(... = pct_black:pct_other, "key", "value") %>%
  split(.$key) %>%
  map2(names(.), ~{
    ggplot(.x) +
      geom_sf(aes(fill = value)) +
      geom_sf(data = mr, color = "gray75", size = 0.9, alpha = 0.8) +
      scale_fill_viridis_c(NULL, labels = scales::percent) +
      scale_x_continuous(expand = expand_scale()) +
      scale_y_continuous(expand = expand_scale()) +
      labs(title = title_lookup[[.y]]) +
      theme_void() +
      theme(plot.background = element_rect(fill = "gray10"),
            panel.background = element_rect(fill = "gray10"), 
            text = element_text(color = "white"),
            legend.position = c(0.2, 0.85),
            legend.direction = "horizontal") +
      coord_sf(datum = NA)
  })

cowplot::plot_grid(plotlist = plts)

title_lookup2 <- c(n_asian = "Asian", n_black = "Black", n_hisp = "Hispanic",
                   n_other = "Other", n_white = "White")

plts2 <- wapo_hou_summ %>%
  select(Beats, n_black:n_other) %>%
  gather(... = -c(Beats, geometry), "key", "value") %>%
  group_by(key) %>%
  mutate(key_total = sum(value, na.rm = TRUE),
         key_pct   = value / key_total) %>%
  ungroup() %>%
  split(.$key) %>%
  map2(names(.), ~{
    ggplot(.x) +
      geom_sf(aes(fill = key_pct)) +
      geom_sf(data = mr, color = "gray75", size = 0.9, alpha = 0.8) +
      scale_fill_viridis_c(NULL, labels = scales::percent) +
      scale_x_continuous(expand = expand_scale()) +
      scale_y_continuous(expand = expand_scale()) +
      labs(title = title_lookup2[[.y]]) +
      theme_void() +
      theme(plot.background = element_rect(fill = "gray10"),
            panel.background = element_rect(fill = "gray10"), 
            text = element_text(color = "white"),
            legend.position = c(0.2, 0.85),
            legend.direction = "horizontal") +
      coord_sf(datum = NA)
  })

cowplot::plot_grid(plotlist = plts2)


bp %>%
  st_join(wapo_hou, left = FALSE) %>%
  st_set_geometry(NULL) %>%
  group_by(Beats, victim_race, disposition) %>%
  summarize(n = n()) %>%
  mutate(n_total = sum(n, na.rm = TRUE),
         n_pct = n / n_total) %>%
  ungroup() %>%
  left_join(bp, ., by = "Beats") %>%
  filter(disposition == "Closed by arrest") %>%
  ggplot() +
  geom_sf(data = bp, fill = "gray50") +
  geom_sf(aes(fill = n_pct)) +
  scale_fill_viridis_c() +
  facet_wrap(vars(victim_race)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10"),
        panel.background = element_rect(fill = "gray10"), 
        text = element_text(color = "white"),
        legend.position = c(0.2, 0.85),
        legend.direction = "horizontal") +
  coord_sf(datum = NA)
