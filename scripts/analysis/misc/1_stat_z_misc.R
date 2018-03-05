
# DISTRIBUTION ------------------------------------------------------------

## DAILY
hou_daily_summ %>%
  ggplot() +
  geom_point(aes(Date, n_offenses, color = `Offense Type`), alpha = 0.1) +
  geom_smooth(aes(Date, n_offenses, group = `Offense Type`, color = `Offense Type`), method = "lm") +
  scale_color_viridis_d(begin = 0.4) +
  facet_wrap(~`Offense Type`, scales = "free_y") +
  guides(color = FALSE) +
  theme_dk()

## MONTHLY
hou_monthly %>%
  ggplot() +
  geom_point(aes(Date, n_offenses, color = `Offense Type`), alpha = 0.3) +
  geom_smooth(aes(Date, n_offenses, group = `Offense Type`, color = `Offense Type`), method = "lm", se = FALSE) +
  scale_color_viridis_d(begin = 0.4) +
  facet_wrap(~`Offense Type`, scales = "free_y") +
  guides(color = FALSE) +
  theme_dk()


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# YEAR TILES --------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


year_summary <- hou %>%
  group_by(year = year(Date),
           `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(hou_pop) %>%
  mutate(rate = (n_offenses / pop) * 10^5) %>%
  mutate(rate = if_else(year == 2017, rate * (12 / 11), rate)) %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    ggplot(.x, aes(factor(year), 5)) +
      geom_tile(aes(fill = rate), color = "gray10", size = 1.5) +
      scale_fill_viridis_c(expand = expand_scale(),
                           breaks = pretty_breaks(3)) +
      scale_x_discrete(labels = if (.y == 1) waiver() else NULL,
                       position = "top",
                       expand = expand_scale()) +
      scale_y_continuous(NULL) +
      guides(fill = guide_colorbar(title = this_level,
                                   title.position = "top", direction = "horizontal",
                                   barwidth = 8, barheight = 1,
                                   ticks = FALSE, raster = FALSE)) +
      theme_dk() +
      theme(panel.grid.major = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_text(size = 12),
            plot.margin = unit(c(0,0,1,0), "lines"))
    
  })

gb <- ggplot() +
  geom_blank() +
  labs(title = paste0("Houston Crime Overview"),
       subtitle = paste0("Annual Crime Rates (2010-2017*)"),
       x = NULL, y = NULL,
       caption = paste0("Source: City of Houston", "\n", "Luke Smith (@lksmith)")) +
  # caption = paste0("Luke Smith*Excludes December 2017", "\n ")) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16, color = "white"),
        plot.title = element_text(size = 22, color = "white"),
        plot.subtitle = element_text(size = 16, margin = margin(0, 0, 2, 0, "lines")),
        plot.background = element_rect(fill = "gray10", color = "gray10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white"),
        plot.caption = element_text(size = 10, color = "gray70"),
        axis.text = element_text(size = 11, color = "white"),
        plot.margin = margin(0.1, 0.2, 0, 0.7, "lines"))

x <- multi_plot(year_summary, gb, 1, FALSE)
grid.newpage()
grid.draw(x)


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END YEAR TILES ----------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# RASTER? -----------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

tmp <- tempdir()

rasters <- hou_monthly %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    g <- ggplot(.x) +
      geom_raster(aes(Date, `Offense Type`, fill = rate), interpolate = TRUE) +
      scale_fill_viridis_c() +
      scale_x_yearmon(format = "%b %y") +
      scale_y_discrete(labels = "") +
      # labs(title = paste0(this_level, "\n")) +
      guides(fill = FALSE) +
      theme_dk() +
      theme(panel.grid = element_blank())
    
    if (.y != 7) {
      g <- g + theme(plot.margin = unit(c(-0.1, 0.6, -0.1, 0.1), "lines"),
                     axis.text.x = element_blank())
    } else {
      g <- g + theme(plot.margin = unit(c(0, 0.6, 0.2, 0.1), "lines"))
    }
    
    g
  })

raster_grobs <- rasters %>% map(ggplotGrob)

# Get and set widths
widths <- raster_grobs %>% map(~.x$widths[2:5])
minWidth <- widths %>% pmap(unit.pmin)
class(minWidth) <- c("unit.list", "unit")

raster_grobs <- raster_grobs %>% map(~{
  .x$widths[2:5] <- as.list(minWidth)
  .x
})

# Get and set heights
heights <- raster_grobs %>% map(~.x$heights[2:5])
minHeight <- heights %>% pmap(unit.pmin)
class(minHeight) <- c("unit.list", "unit")

raster_grobs <- raster_grobs %>% map(~{
  .x$heights[2:5] <- as.list(minHeight)
  .x
})

p <- raster_grobs %>% map2(seq_along(.), ~{
  g <- grid.arrange(arrangeGrob(.x))
  if (.y != 7) {
    ggsave(fpath <- file.path(tmp, sprintf("raster%s.png", .y)), plot = g,
           width = 400 / 100, height = 25 / 100, dpi = 100)  
  } else {
    ggsave(fpath <- file.path(tmp, sprintf("raster%s.png", .y)), plot = g,
           width = 400 / 100, height = 45 / 100, dpi = 100)
  }
  
  fpath
})


raster_img <- p %>%
  map(image_read) %>%
  image_join() %>%
  image_append(stack = TRUE)

raster_info <- image_info(raster_img)

title_box <- image_blank(raster_info$width, 60, "gray10")

raster_img <- title_box %>%
  image_annotate("Houston Crime Through The Years",
                 location = geometry_point(3, -4),
                 color = clr, font = fnt, size = 24) %>%
  image_annotate("Monthly Crime Crate From 2010 to 201*",
                 location = geometry_point(3, 22),
                 color = clr, font = fnt, size = 18) %>%
  image_join(raster_img) %>%
  image_append(stack = TRUE)

y_label_r <- image_blank(120, image_info(raster_img)$height, "gray10")
y_labels <- as.character(unique(hou_monthly$`Offense Type`))

(function() {
  rs <- y_label_r %>%
    image_join(raster_img,.) %>%
    image_append()
  y_off <- 70
  for (i in seq_len(7)) {
    rs <- image_annotate(rs, text = y_labels[i],
                         location = geometry_point(375, y_off),
                         font = "Open Sans Bold", color = clr, size = 14)
    y_off <- y_off + 25
  }
  rs
}
)()

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END RASTER? -------------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# SEASONALITY -------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

hou_cr_seasonal <- hou_monthly %>%
  select(1,2,3) %>%
  group_by(`Offense Type`, month = month(Date)) %>%
  summarize(n_offenses = mean(n_offenses, na.rm = TRUE)) %>%
  ungroup()


hou_monthly2 <- hou_monthly %>%
  select(1,2,3) %>%
  mutate(month = month(Date),
         year  = year(Date))

hou_cr_ribbon <- hou_monthly2 %>%
  group_by(`Offense Type`, month) %>%
  filter(n_offenses == max(n_offenses) |
           n_offenses == min(n_offenses)) %>%
  arrange(n_offenses) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  mutate(cat = if_else(n_offenses == max(n_offenses), "max", "min")) %>%
  ungroup() %>%
  select(1, 3, 4, 6)

hou_cr_ribbon <- hou_cr_ribbon %>%
  split(.$`Offense Type`) %>%
  map(~split(.x, .x$month)) %>%
  modify_depth(2, ~spread(select(.x, -month), cat, n_offenses)) %>%
  map(~bind_rows(.x, .id = "month")) %>%
  bind_rows() %>%
  mutate_at(vars(month), as.integer)

ribbon_breaks <- c(1, 7)

k <- sort(unique(hou_cr_ribbon$`Offense Type`))

ribs <- map2(k, picked_colors, ~{
  
  d <- filter(hou_cr_ribbon, `Offense Type` == .x)
  e <- filter(hou_cr_seasonal, `Offense Type` == .x)
  
  p <-  d %>%
    ggplot() +
    geom_ribbon(aes(month, ymin = min, ymax = max), fill = .y, color = "gray80") +
    geom_line(aes(month, n_offenses),
              e, size = 0.8, color = "gray30") +
    scale_x_continuous(NULL,
                       breaks = ribbon_breaks,
                       minor_breaks = c(),
                       labels = if (any(grepl("Robberies|Aggravated Assaults|Burglaries|Other Thefts", .x))) month.name[ribbon_breaks] else NULL,
                       expand = expand_scale()) +
    scale_y_continuous(NULL,
                       expand = expand_scale(),
                       breaks = scales::pretty_breaks(3),
                       labels = scales::comma) +
    guides(fill = FALSE) +
    labs(title = paste0(.x, "\n")) +
    theme_dk()
  
  p
})

gb <- ggplot() +
  geom_blank() +
  labs(title = paste0("Houston Crime Seasonality"),
       subtitle = paste0("Monthly Highs, Lows, And Means (2010-2016)"),
       x = NULL, y = NULL,
       caption = paste0("Luke Smith (@lksmith)", "\n", "Source: City of Houston")) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16, color = "white"),
        plot.title = element_text(size = 22, color = "white"),
        plot.subtitle = element_text(size = 14),
        plot.background = element_rect(fill = "gray10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white"),
        plot.caption = element_text(size = 10, color = "gray70"),
        axis.text = element_text(size = 11, color = "white"),
        plot.margin = margin(0.1, 0.3, 0, 0.7, "lines"))

grid.newpage()
x <- multi_plot(ribs, gb,1,F)
grid.draw(x)

ggsave("ribbons_fat.png", x, width = 600 / 100, height = 1800 / 100, dpi = 100)

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END OF SEASONALITY ------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


hou_same_month %>%
  filter(`Offense Type` == "Aggravated Assaults") %>%
  ggplot(aes(factor(Date), 5)) +
  geom_tile(aes(fill = rate), color = "gray10", size = 1) +
  scale_fill_viridis_c(option = "D") +
  scale_x_discrete(expand = expand_scale(),
                   labels = m) +
  scale_y_continuous(NULL) +
  coord_cartesian() +
  facet_grid(year ~ .) +
  labs(title = "Aggravated Assaults\n") +
  guides(fill = guide_colorbar(title = NULL,
                               title.position = "left",
                               barwidth = 6, barheight = .3,
                               ticks = FALSE, raster = FALSE,
                               draw.ulim = TRUE, draw.llim = TRUE, label.hjust = 1)) +
  theme_dk() +
  theme(plot.title = element_text(size = 10, margin = margin(0.3, 0, -0.7, 0, "lines")),
        plot.margin = unit(c(0,0,0.4,0), "lines"),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(size = 7, vjust = 0.5, margin = margin(0,0,0,0,"lines")),
        strip.text.y = element_text(angle = 0, size = 7,
                                    margin = margin(0,0,0,0, "lines")),
        panel.spacing.y = unit(1, "pt"),
        legend.direction = "horizontal", legend.position = c(0.8, 1.05),
        legend.margin = margin(0,0,0,0, "lines"),
        legend.text = element_text(size = 7, margin = margin(0,0,0,0,"lines")))
