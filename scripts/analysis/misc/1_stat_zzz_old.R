hou_plots <- list()
plt_colors <- c("#984807", "#e15c39", "#008348", "#4c7093", "#d6a300", "#ce1141", 
                "#8E3694")
purp00 <- "#bdadb8"
purp0 <- "#a6819c"
purp1 <- "#814b72"
purp2 <- "#5a344f"
purp3 <- "#361f2f"



#   -----------------------------------------------------------------------
# MISSING DAYS FUNCTIONS --------------------------------------------------
#   -----------------------------------------------------------------------

make_dates_m <- function(y, m, d, num_days) {
  
  missing_days <- setdiff(seq.int(1, num_days, 1), d)
  lubridate::make_date(y, m, missing_days)
  
}

insert_missing_days <- function(df) {
  
  y <- df$year[1]
  m <- df$month[1]
  num_days <- df$days[1]
  
  df <- select(df, Date, `Offense Type`, day)
  add_row(df,
          `Offense Type` = df$`Offense Type`[1],
          day = NA,
          Date = make_dates_m(y, m, df$day, num_days))
}

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END OF MISSING DAYS FUNCTIONS -------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# ROSE PLOTS --------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


# Rose function
rose_diagrams <- function(var, clr) {
  p <- ggplot(filter(hou_Hour_count, `Offense Type` == var)) +
    geom_bar(aes(as.integer(Hour), n_Hour), fill = clr, color = "black",
             stat = "identity") +
    labs(title = paste0(var)) +
    scale_y_continuous(name = NULL,
                       labels = NULL) +
    scale_x_continuous(name = NULL,
                       breaks = c(0, 6, 12, 18),
                       labels = c("12:00 am", "6:00 am",
                                  "12:00 pm", "6:00 pm")) +
    coord_polar() +
    theme_minimal() +
    theme(text = element_text(family = "Open Sans", size = 14),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  pg <- ggplotGrob(p)
  
  pg$layout$clip[[which(pg$layout$name == "panel")]] <- "off"
  
  return(pg)
}

# Aggregate data for roses
hou_Hour_count <- hou %>%
  group_by(Hour,
           `Offense Type`) %>%
  summarize(n_Hour = sum(offense_count))

# Roses
rose_garden <- Map(rose_diagrams, rev(as.character(unique(reorder(hou$`Offense Type`, hou$Hour, length)))), plt_colors)
gridExtra::grid.arrange(roses <- gridExtra::arrangeGrob(grobs = rose_garden, ncol = 2, padding = unit(-1, "lines"), top = ""))


blanky <- ggplot() +
  geom_blank() +
  labs(title = "When do crimes most often occur during the day?",
       subtitle = paste0("Violent crimes occur most often during the night hours and",
                         "\ngeneraly peak around midnight when people are socially",
                         "\nactive at bars and restaurants; while burglary occurs",
                         "\nduring the day when most people are away from home",
                         "\n"),
       caption = "Source: City of Houston; Houston Police Department") +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16))

bg <- ggplotGrob(blanky)

widths <- bg

bg$grobs[[which(bg$layout$name == "panel")]] <- addGrob(bg$grobs[[which(bg$layout$name == "panel")]], roses)

hou_plots$roses <- bg

grid.newpage()
grid.draw(bg)



panel_layout <- grid.layout(nrow = 4, ncol = 3,
                            widths = c(4, 1, 4),
                            heights = c(4, 4, 4, 4),
                            default.units = "cm")

vp_layout <- function(...) {
  grid.newpage()
  pushViewport(viewport(layout = panel_layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

mmplot <- function(p = rose_garden) {
  
  vp_layout()
  
  pushViewport(subplot(1, 1))
  grid.draw(p[[1]])
  
  upViewport()
  pushViewport(subplot(1, 3))          
  grid.draw(p[[2]])
  
  upViewport()
  pushViewport(subplot(2, 1))
  grid.draw(p[[3]])
  
  upViewport()
  pushViewport(subplot(2, 3))
  grid.draw(p[[4]])
  
  upViewport()
  pushViewport(subplot(3, 1))
  grid.draw(p[[5]])
  
  upViewport()
  pushViewport(subplot(3, 3))
  grid.draw(p[[6]])
  
  upViewport()
  pushViewport(subplot(4, 1))          
  grid.draw(p[[7]])
  
  
}


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# ROSE PLOTS --------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------



# New addition 2018-01-14 -------------------------------------------------


hou_yearly <- hou_monthly %>%
  filter(year(as.Date(Date)) >= 2010) %>%
  group_by(`Offense Type`,
           year = year(as.Date(Date))) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE))




# -- Lines
hou_plots$lines <- hou_monthly %>%
  ggplot(aes(Date)) +
  geom_line(aes(y = mva_6, group = `Offense Type`),
            color = "gray50", size = 0.7) +
  geom_line(aes(y = offense_count, color = offense_count), size = 0.9) +
  facet_wrap(~forcats::fct_rev(reorder(`Offense Type`, offense_count, sum)),
             scales = "free_y", ncol = 1) +
  scale_x_yearmon(expand = expand_scale()) +
  scale_y_continuous(expand = expand_scale()) +
  # scale_color_manual(values = plt_colors) +
  scale_color_viridis_c(option = "C", begin = 0.2, end = 1) +
  guides(color = FALSE) +
  labs(title = "Crime in Houston from July 2009 to December 2016",
       subtitle = paste0("Monthly totals and six-month lagging averages (gray line) show a drop",
                         "\nin burglary but a rise in rape and murder."),
       caption = "\nSource: City of Houston; Houston Police Department") +
  theme_minimal() +
  theme_dk() +
  theme(text = element_text(family = "Open Sans", size = 16),
        panel.grid.minor = element_blank())


#   -----------------------------------------------------------------------

# -- PER DAY
hou_plots$ridges_per_day <- hou %>%
  filter(year(Date) >= 2010) %>%
  group_by(Date, `Offense Type`) %>%
  summarize(n = sum(offense_count, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot() +
  geom_density_ridges(aes(n, factor(year(Date)), fill = `Offense Type`),
                      alpha = 0.8, color = "gray80") +
  scale_fill_manual(values = viridis::magma(70, begin = 0.2, end = 1)[seq.int(1, 70, 7)][repick_colors]) +
  scale_x_continuous(expand = expand_scale(),
                     breaks = pretty_breaks(3)) +
  scale_y_discrete(expand = expand_scale()) +
  theme_dk() +
  guides(fill = FALSE) +
  labs(title = "Daily Crime Activity") +
  facet_wrap(~`Offense Type`, scales = "free_x", ncol = 1)  +
  theme(strip.text = element_text(color = "white", hjust = 0),
        plot.title = element_text(color = "white", size = 18))

# -- PER MONTH
hou_plots$ridges_per_month <- hou_monthly %>%
  filter(year(Date) >= 2010) %>%
  ggplot() +
  geom_density_ridges(aes(offense_count,
                          factor(year(as.Date(Date))), fill = `Offense Type`),
                      alpha = 0.8, color = "gray80") +
  scale_y_discrete(NULL,
                   labels = NULL,
                   expand = expand_scale()) +
  scale_x_continuous(NULL,
                     breaks = pretty_breaks(3),
                     expand = expand_scale()) +
  # scale_fill_brewer(palette = "Accent") +
  scale_fill_manual(values = viridis::magma(70, begin = 0.2, end = 1)[seq.int(1, 70, 7)][repick_colors]) +
  facet_wrap(~`Offense Type`, scales = "free_x", ncol = 1) +
  labs(title = "Monthly Crime Activity") +
  guides(fill = FALSE) +
  theme(strip.text = element_text(hjust = 0),
        axis.text = element_text(size = 10)) +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.title = element_text(color = "white", size = 18),
        axis.text = element_text(color = "white"),
        strip.text = element_text(color = "white", hjust = 0))

# -- Bring them together
gb <- ggplot() +
  geom_blank() +
  labs(title = "Houston Crime Overview",
       # subtitle = paste0(""),
       x = NULL, y = NULL,
       caption = paste0("Source: City of Houston")) +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans", size = 16, color = "white"),
        plot.title = element_text(size = 22, color = "white"),
        plot.subtitle = element_text(size = 14),
        plot.background = element_rect(fill = "gray10"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white"),
        plot.caption = element_text(size = 10, color = "gray70"),
        axis.text = element_text(size = 11, color = "white"), plot.margin = margin(0.1, 0.2, 0, 0.7, "lines"))

# Grobify
gb_grob <- ggplotGrob(gb)

ridge_grobs <- list(hou_plots$ridges_per_day, hou_plots$ridges_per_month) %>%
  map(ggplotGrob)

# widths
grob_widths <- ridge_grobs %>% map(~.x$widths[2:5])

minWidth <- list(grob_widths) %>% pmap(unit.pmin)

class(minWidth) <- c("unit.list", "unit")

ridge_grobs <- ridge_grobs %>% map(~{
  .x$widths[2:5] <- as.list(minWidth)
  .x
})

# heights
grob_heights <- ridge_grobs %>% map(~.x$heights[2:5])

minHeight <- list(grob_heights) %>% pmap(unit.pmin)

class(minHeight) <- c("unit.list", "unit")

ridge_grobs <- ridge_grobs %>% map(~{
  .x$heights[2:5] <- as.list(minHeight)
  .x
})

gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] <- gb_grob$grobs[[which(gb_grob$layout$name == "panel")]] %>%
  addGrob(arrangeGrob(grobs = ridge_grobs,
                      ncol = 2
  ))

grid.newpage()
grid.draw(gb_grob)
# test 2 ------------------------------------------------------------------


