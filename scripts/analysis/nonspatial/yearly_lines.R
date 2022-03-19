#   -----------------------------------------------------------------------
# BARS/SPLINES ------------------------------------------------------------
#   -----------------------------------------------------------------------

yearly_bars <- hou_yearly %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    this_level <- levels(.x$`Offense Type`)[.y]
    ggplot(filter(.x, `Offense Type` == this_level)) +
      geom_col(aes(year, rate, group = `Offense Type`, fill = rate)) +
      facet_wrap(~`Offense Type`, scales = "free_y") +
      scale_fill_viridis_c() +
      theme_dk()
  })

yearly_lines <- hou_yearly %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    this_level <- levels(.x$`Offense Type`)[.y]
    ggplot(filter(.x, `Offense Type` == this_level)) +
      geom_line(aes(year, rate, group = `Offense Type`, color = rate)) +
      facet_wrap(~`Offense Type`, scales = "free_y") +
      scale_color_viridis_c() +
      theme_dk()
  })
  
hou_yearly %>%
  ggplot() +
  geom_xspline(aes(year, rate, group = `Offense Type`)) +
  facet_wrap(~`Offense Type`, scales = "free_y")

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# BARS --------------------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# YEARLY LINES ------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

yearly_lines <- hou_daily_summ %>%
  arrange(`Offense Type`, Date) %>%
  group_by(year = year(Date), `Offense Type`) %>%
  mutate(csum = cumsum(offense_count)) %>%
  ungroup() %>%
  mutate(same_date = update(Date, year = 2017)) %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    ggplot(.x, aes(same_date, csum)) +
      geom_line(data = filter(.x, year == 2017), color = "white") +
      geom_line(aes(group = factor(year), alpha = rev(year)),
                color = picked_colors[.y]) +
      scale_alpha_continuous(range = c(0.4, 1)) +
      scale_x_date(expand = expand_scale(), minor_breaks = NULL) +
      scale_y_continuous(expand = expand_scale(),
                         breaks = pretty_breaks(3), minor_breaks = NULL) +
      labs(title = paste0(levels(.x$`Offense Type`)[.y], "\n")) +
      guides(alpha = FALSE) +
      theme_dk()
  })

gb <- ggplot() +
  geom_blank() +
  labs(title = paste0("Houston Crimes By Year"),
       subtitle = paste0("Cumulative Crime Counts (2010-2017)"),
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
x <- multi_plot(yearly_lines, gb,1,F)
grid.draw(x)
ggsave("plots/yearly_lines.png", x, width = 400 / 100, height = 1200 / 100, dpi = 100)

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END YEARLY LINES --------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
