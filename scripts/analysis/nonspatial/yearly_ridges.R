#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# RIDGES ------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


hou_daymon <- hou_monthly %>% 
  mutate(Date = as.Date(Date)) %>%
  add_column(facet = "Monthly") %>%
  bind_rows(add_column(hou_daily_summ, facet = "Daily"), .)


ridges <- hou_daymon %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    ggplot(.x) +
      geom_density_ridges(aes(n_offenses, factor(year(Date)), height = ..density..),
                          panel_scaling = TRUE, fill = picked_colors[.y],
                          alpha = 0.7, color = "#00000000", rel_min_height = 0.01,
                          stat = "density") +
      scale_y_discrete(NULL,
                       expand = expand_scale()) +
      scale_x_continuous(NULL,
                         label = comma,
                         breaks = pretty_breaks(3),
                         minor_breaks = NULL,
                         expand = expand_scale()) +
      facet_wrap(~facet, scales = "free_x", ncol = 2) +
      labs(title = paste0(this_level, "\n")) +
      theme_dk() +
      theme(strip.text = element_blank(),
            panel.spacing = unit(1, "lines"))
  })


gb <- ggplot() +
  geom_blank() +
  labs(title = paste0("Houston Crime Distribution", "\n",
                      "(2010-2017)"),
       subtitle = paste0("  Daily                                   Monthly"),
       x = NULL, y = NULL,
       caption = paste0("Luke Smith (@lksmith)", "\n",
                        "Source: City of Houston")) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16, color = "white"),
        plot.title = element_text(size = 22, color = "white", margin = margin(0,0,0.2,0, "lines")),
        plot.subtitle = element_text(size = 16, hjust = 0.5,
                                     margin = margin(0, 0, 0, 0, "lines")),
        plot.background = element_rect(fill = "gray10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray50"),
        plot.caption = element_text(size = 10, color = "gray60"),
        axis.text = element_text(size = 11, color = "gray90"),
        plot.margin = margin(0.1, 0.3, 0, 0.6, "lines"))

# Grobify
gb_grob <- ggplotGrob(gb)

gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] <- gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] %>%
  addGrob(grid.arrange(arrangeGrob(grobs = ridges, ncol = 1)))

grid.newpage()
grid.draw(gb_grob)
ggsave("plots/ridges_fat.png", gb_grob, width = 600 / 100, height = 1800 / 100, dpi = 100)

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END OF RIDGES -----------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
