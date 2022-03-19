library(sf)



# POLICE DISTRICTS --------------------------------------------------------


d_file <- "data/Houston_Police_Districts/Houston_Police_Districts.gdb"
hpd <- st_read(d_file)
hpd <- st_transform(hpd, crs = st_crs(4269))

hpd_monthly <- hou %>%
  group_by(`Offense Type`,
           Date = as.yearmon(Date),
           DISTRICT) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
  ungroup()

hpd_monthly <- hpd_monthly %>%
  mutate(year = year(as.Date(Date))) %>%
  left_join(hou_pop, by = "year") %>%
  mutate(rate = (offense_count / pop) * 10^5)



# MAJOR ROADS -------------------------------------------------------------


r_file <- "data/Major_Roads/Major_Roads.gdb"
roads  <- st_read(r_file)
roads  <- st_transform(roads, crs = st_crs(4269))



# DAILY -------------------------------------------------------------------


hpd_daily <- hou %>%
  group_by(Date = as.Date(Date),
           DISTRICT,
           `Offense Type`) %>%
  summarize(offense_count = sum(offense_count)) %>%
  ungroup()



# YEARLY ------------------------------------------------------------------


hpd_yearly <- hpd_daily %>%
  group_by(year = year(Date),
           DISTRICT,
           `Offense Type`) %>%
  summarize(offense_count = sum(offense_count)) %>%
  ungroup() %>%
  left_join(hou_pop, by = "year") %>%
  mutate(rate = (offense_count / pop) * 10^5)

hpd_yearly <- hpd_yearly %>%
  right_join(expand.grid(year = 2010:2017,
                         DISTRICT = c(1:21, 23:24),
                         `Offense Type` = unique(hpd_yearly$`Offense Type`)),
             by = c("year", "DISTRICT", "Offense Type")) %>%
  mutate_at(vars(offense_count, rate), function(x) if_else(is.na(x), 0, x))

choro_yearly <- hpd_yearly %>%
  left_join(hpd, by = "DISTRICT") %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    ggplot(.x) +
      geom_sf(aes(fill = offense_count, geometry = Shape)) +
      scale_fill_viridis_c() +
      scale_x_continuous(expand = expand_scale()) +
      scale_y_continuous(expand = expand_scale()) +
      coord_sf(datum = NA) +
      facet_wrap(~year) +
      theme_dk() +
      theme(panel.grid = element_blank())
  })

tmp <- tempdir()

choro_2016 <- hpd_yearly %>%
  filter(year == 2016) %>%
  left_join(hpd) %>%
  split(.$`Offense Type`) %>%
  map2_chr(seq_along(.), ~{
    this_level <- levels(.x$`Offense Type`)[.y]
    
    p <- ggplot(.x) +
      geom_sf(aes(fill = offense_count, geometry = Shape), color = "gray10") +
      geom_sf(aes(geometry = Shape), roads, color = "#a5872a", alpha = 0.3) +
      scale_fill_viridis_c(NULL,
                           labels = comma, breaks = pretty_breaks(3)) +
      scale_color_viridis_c(begin = 0.1) +
      scale_x_continuous(expand = expand_scale()) +
      scale_y_continuous(expand = expand_scale()) +
      coord_sf(xlim = st_bbox(hpd)[c(1, 3)], ylim = st_bbox(hpd)[c(2, 4)], datum = NA) +
      guides(fill = guide_colorbar(raster = FALSE, ticks = FALSE,
                                   barheight = 0.05, barwidth = 0.48,
                                   title.vjust = 1, title.position = "left",
                                   default.unit = "npc"),
             color = FALSE) +
      labs(title = paste0(this_level, "\n")) +
      theme_dk() +
      theme(panel.grid = element_blank(),
            legend.direction = "horizontal",
            legend.text = element_text(margin = margin(0.1, 0, 0, 0, "lines")),
            legend.position = "top",
            legend.margin = margin(0, 0, -3, 0, "lines"),
            legend.justification = "left")
    
    
    ggsave(fp <- file.path(tmp, sprintf("choro_2016_%s.png", .y)), plot =  p,
           width = 325 / 100, height = 300 / 100, dpi = 100)
    fp
  })

choro_2016 %>%
  rep(5) %>%
  sort() %>%
  map(image_read) %>%
  map(~image_annotate(.x,
                      "Source: City of Houston; Houston-Galveston Area Council",
                      location = geometry_point(0, 290),
                      color = "gray50", size = 7, font = "Open Sans")) %>%
  map(~image_annotate(.x,
                      "Luke Smith (@lksmth)",
                      location = geometry_point(0, 280),
                      color = "gray90", size = 10, font = "Open Sans")) %>%
  map(~image_annotate(.x,
                      "Year: 2016",
                      location = geometry_point(5, 60),
                      color = "gray90", size = 14, font = "Open Sans")) %>%
  image_join() %>%
  image_animate(2) %>%
  image_write("plots/choro_2016.gif")



# BEATS -------------------------------------------------------------------


bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")
sch <- st_read("data/Houston_Schools/Schools.shp")

sch_bp <- st_within(sch, st_union(bp), sparse = FALSE)
ggplot(bp) + geom_sf() + geom_sf(data = sch[sch_bp, ], size = 0.3, color = "brown")


schools_in_beat <- bp %>%
  split(.$OBJECTID_1) %>%
  map_int(~sum(st_within(sch, .x, sparse = FALSE)))

bp$schools_in_beat <- schools_in_beat


choro_beat_2016 <- hpd_yearly %>%
  filter(year == 2016) %>%
  left_join(bp, by = c("Beat", "Beats")) %>%
  split(.$`Offense Type`) %>%
  map2_chr(seq_along(.), ~{
    this_level <- levels(.x$`Offense Type`)[.y]
    
    p <- ggplot(.x) +
      geom_sf(aes(fill = offense_count), color = "gray10") +
      geom_sf(aes(geometry = Shape), roads, color = "#a5872a", alpha = 0.3) +
      scale_fill_viridis_c(NULL,
                           labels = comma, breaks = pretty_breaks(3)) +
      scale_color_viridis_c(begin = 0.1) +
      scale_x_continuous(expand = expand_scale()) +
      scale_y_continuous(expand = expand_scale()) +
      coord_sf(xlim = st_bbox(hpd)[c(1, 3)], ylim = st_bbox(hpd)[c(2, 4)], datum = NA) +
      guides(fill = guide_colorbar(raster = FALSE, ticks = FALSE,
                                   barheight = 0.05, barwidth = 0.48,
                                   title.vjust  = 1, title.position = "left",
                                   default.unit = "npc"),
             color = FALSE) +
      labs(title = paste0(this_level, "\n")) +
      theme_dk() +
      theme(panel.grid = element_blank(),
            legend.direction = "horizontal",
            legend.text = element_text(margin = margin(0.1, 0, 0, 0, "lines")),
            legend.position = "top",
            legend.margin   = margin(0, 0, -3, 0, "lines"),
            legend.justification = "left")
    
    
    ggsave(fp <- file.path(tmp, sprintf("plots/choro_beat_2016_%s.png", .y)), plot =  p,
           width = 325 / 100, height = 300 / 100, dpi = 100)
    fp
  })
