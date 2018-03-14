#   -----------------------------------------------------------------------
# PER HOUR SPLINES --------------------------------------------------------
#   -----------------------------------------------------------------------

hou_hourly <- hou %>%
  group_by(Date, hour = hour(Date), `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup()

hou_same_hour <- hou_hourly %>%
  mutate(Date = update(Date, yday = 1),
         Date = update(Date, year = 2017)) %>%
  group_by(Date, `Offense Type`) %>%
  summarize(n_offenses = mean(n_offenses, na.rm = TRUE)) %>%
  ungroup()

mpop <- hou_pop %>%
  summarize(pop = mean(pop, na.rm = TRUE)) %>%
  pull(pop)

hou_same_hour <- hou_same_hour %>%
  add_column(mpop = mpop) %>%
  mutate(drate = (n_offenses / mpop) * 10^5)

splines_hourly <- hou_same_hour %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    ggplot(.x) +
      geom_xspline(aes(Date, n_offenses),
                   color = picked_colors[.y], size = 1) +
      scale_x_datetime(expand = expand_scale(), date_labels = "%I %p") +
      scale_y_continuous(expand = expand_scale()) +
      labs(title = paste0(levels(.x$`Offense Type`)[.y], "\n")) +
      theme_dk()
  })

gb <- ggplot() +
  geom_blank() +
  labs(title = paste0("Houston Crime"),
       subtitle = paste0("Hourly Crime Averages (2010-2017)"),
       x = NULL, y = NULL,
       caption = paste0("Luke Smith (@lksmith)", "\n", "Source: City of Houston")) +
       # caption = paste0("*Excludes December 2017", "\n",)) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16, color = "white"),
        plot.title = element_text(size = 22, color = "white"),
        plot.subtitle = element_text(size = 16),
        plot.background = element_rect(fill = "gray10", color = "gray10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white"),
        plot.caption = element_text(size = 10, color = "gray70"),
        axis.text = element_text(size = 11, color = "white"),
        plot.margin = margin(0.1, 0.3, 0, 1.2, "lines"))

x <- multi_plot(splines_hourly, gb,1,F)
ggsave("plots/splines_hourly.png", x, width = 400 / 100, height = 1200 / 100, dpi = 100)


