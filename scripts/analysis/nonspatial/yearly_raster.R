w = 700 / 100
h = 80 / 100

yearly_rasters <- hou_monthly %>%
  mutate(mrate = ((n_offenses / pop) * 10^5),
         norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>%
  split(.$`Offense Type`) %>%
  map2_chr(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    x <- ggplot(.x, aes(factor(Date), `Offense Type`)) +
      geom_raster(aes(fill = norm_mrate, color = norm_mrate), interpolate = TRUE) +
      scale_x_discrete(expand = expand_scale()) +
      scale_y_discrete(expand = expand_scale()) +
      scale_fill_viridis_c(breaks = pretty_breaks(4)) +
      scale_color_viridis_c() +
      guides(color = FALSE,
             fill = guide_colorbar(title = paste0(this_level),
                                   title.position = "top", direction = "horizontal",
                                   ticks = FALSE, barwidth = 6.9, raster = TRUE)) +
      theme_dk() +
      theme(axis.text = element_blank(),
            legend.position = "left",
            plot.margin = unit(c(0,0.3,0,0), "lines"))
    
    
    ggsave(fpath <- sprintf("plots/yearly_rasters_%s.png", .y),
           x, width = w, height = h, dpi = 100)
    fpath
  })

i <- yearly_rasters %>% image_read()
i %>%
  image_append(stack = TRUE)

axis_plot <- hou_monthly %>%
  mutate(mrate = ((n_offenses / pop) * 10^5),
         norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>%
  filter(`Offense Type` == "Aggravated Assaults") %>%
  ggplot(aes(Date, `Offense Type`)) +
  geom_raster(aes(fill = norm_mrate, color = norm_mrate), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  scale_x_continuous(expand = expand_scale()) +
  # scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = paste0("Aggravated Assaults"),
                               title.position = "top", direction = "horizontal",
                               ticks = FALSE, barwidth = 6.9, raster = FALSE),
         color = FALSE) +
  theme_dk() +
  theme(legend.position = "left",
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 1),
        axis.line.x = element_line(color = "gray50"),
        axis.ticks = element_line(color = "gray50"),
        plot.margin = unit(c(0,0.3,2,0), "lines"))

ggsave(apath <- "plots/yearly_raster_axis.png", axis_plot,
       width = w, height = h*1.7, dpi = 100)

a <- apath %>% image_read()
a <- a %>%
  image_crop("x40+0+78") %>%
  image_join(image_blank(700, 5, "gray10"), .) %>%
  image_append(stack = TRUE)

r_plot <- i %>%
  image_crop("x70+0+5") %>%
  image_append(stack = TRUE) %>%
  image_join(a) %>%
  image_append(stack = TRUE)

r_plot <- r_plot %>%
  image_annotate("Source: City of Houston", location = geometry_point(4, 515),
                 font = "Open Sans", color = "gray50", size = 12) %>%
  image_annotate("Luke Smith (@lksmth)", location = geometry_point(565, 515),
                 font = "Open Sans", color = "springgreen", size = 13)


r_title <- image_blank(700, 45, "gray10") %>%
  image_annotate("Crime in Houston: 2010-2017",
                 location = geometry_point(4, -5),
                 font = "Open Sans", color = "gray90", size = 40)

r_subtitle <- image_blank(700, 35, "gray10") %>%
  image_annotate("Crimes Per Month Per 100,000 Persons",
                 location = geometry_point(6, -5),
                 font = "Open Sans", color = "gray90", size = 20)

r_title %>%
  image_join(r_subtitle, r_plot) %>%
  image_append(stack = TRUE) %>%
  image_write("plots/monthly_rasters.png")

file.remove(yearly_rasters)

# NORMALIZED --------------------------------------------------------------

index <- function(x) {
  min_x <- min(x)
  max_x <- max(x)
  vapply(X = x,
         FUN = function(value) (value - min_x) / (max_x - min_x),
         FUN.VALUE = numeric(1))
}

h = 395 / 100

hou_monthly %>%
  mutate(mrate = ((n_offenses / pop) * 10^5),
         norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>%
  group_by(`Offense Type`) %>%
  mutate(index_mrate = index(norm_mrate)) %>%
  ungroup() %>%
  ggplot() +
  geom_raster(aes(factor(Date), fct_rev(`Offense Type`), fill = index_mrate), interpolate = TRUE) +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = paste0("Normalized monthly crime rate"),
                               title.position = "top", direction = "horizontal",
                               ticks = FALSE, barwidth = 30, barheight = 1,
                               raster = TRUE),
         color = FALSE) +
  # labs(title = "Crime in Houston: 2010-2017",
  #      subtitle = "Normalized Crime Rate") +
  theme_dk() +
  theme(axis.text.x = element_blank(), legend.position = "top")

# TEST --------------------------------------------------------------------

assaults <- hou_monthly %>%
  mutate(mrate = ((n_offenses / pop) * 10^5),
         norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>%
  filter(`Offense Type` == "Aggravated Assaults") %>%
  ggplot(aes(factor(Date), `Offense Type`)) +
  geom_tile(aes(fill = norm_mrate, color = norm_mrate)) +
  scale_fill_viridis_c() +
  scale_color_viridis_c()

a <- assaults +
  scale_x_discrete(expand = expand_scale()) +
  scale_y_discrete(expand = expand_scale()) +
  guides(fill = guide_colorbar(title = paste0("Aggravated Assaults"),
                               title.position = "top", direction = "horizontal",
                               ticks = FALSE, barwidth = 10, raster = FALSE),
         color = FALSE) +
  theme_dk() +
  theme(axis.text = element_blank(),
        legend.position = "left",
        plot.margin = unit(c(0,0,0,0), "lines"))

x_line <- a %>%
  axis_canvas("x") +
  geom_line(aes(Date, norm_mrate), data = hou_monthly %>%
              mutate(mrate = ((n_offenses / pop) * 10^5),
                     norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>% filter(`Offense Type` == "Aggravated Assaults"))
