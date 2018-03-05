## Give all months the same year for easier plotting
hou_same_month <- hou_monthly %>%
  mutate(mrate = ((n_offenses / pop) * 10^5),
         norm_mrate = mrate * (31 / days_in_month(as.Date(Date)))) %>%
  mutate(year = year(Date),
         Date = update(as.Date(Date), year = 2017))

# x-axis text labels
m <- month.abb
m[-seq(1,12,3)] <- ""

# image dimensions (when saving to png file)
n <- 320
ratio <- 239/290  # ratio I have chosen
dpi = 100
w <- (n / dpi)
h <- (n*ratio / dpi)

yearmon_boxes <- hou_same_month %>%
  split(.$`Offense Type`) %>%
  map2_chr(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    title_size <- 12
    leg_size <- 9
    other_size <- 10
    leg_x <- 0.8
    bar_w <- 5.5

    x <- ggplot(.x, aes(factor(Date), 5)) +
      geom_tile(aes(fill = norm_mrate, color = norm_mrate)) + # NO SPACING VERSION
      
      scale_fill_viridis_c(option = "D", breaks = pretty_breaks(3), minor_breaks = NULL) +
      scale_color_viridis_c(option = "D", breaks = pretty_breaks(3),
                            minor_breaks = NULL) + # NO SPACING VERSION
      scale_x_discrete(expand = expand_scale(),
                       labels = m) +
      scale_y_continuous(NULL) +
      
      coord_cartesian() +
      facet_grid(year ~ .) +
      labs(title = paste0(this_level, "\n")) +
      
      guides(color = FALSE, # NO SPACING VERSION
        fill = guide_colorbar(title = NULL,
                              title.position = "left",
                              barwidth = bar_w, barheight = .5,
                              ticks = FALSE, raster = FALSE,
                              draw.ulim = F, draw.llim = F, label.hjust = 1)) +
      theme_dk() +
      theme(plot.title = element_text(size = title_size,
                                      margin = margin(0.3, 0, -0.7, 0, "lines")),
            plot.margin = unit(c(0.5,0,0.5,0), "lines"),
            panel.grid.major = element_blank(),
            axis.text.y = element_blank(), 
            axis.text.x = element_text(size = other_size, vjust = 0.5,
                                       margin = margin(0,0,0,0,"lines")),
            strip.text.y = element_text(angle = 0, size = other_size,
                                        margin = margin(0,0.2,0,0.2, "lines")),
            panel.spacing.y = unit(-1, "pt"), # NO SPACING VERSION
            legend.direction = "horizontal", legend.position = c(leg_x, 1.081),
            legend.margin = margin(0,0,0,0, "lines"),
            legend.text = element_text(size = leg_size,
                                       margin = margin(0,0,0,0,"lines")))
    
    ggsave(fpath <- sprintf("plots/year_boxes_exp%s.png", .y), x, width = w, height = h, dpi = 100)
    fpath
  })

yb_info <- yearmon_boxes[1] %>% image_read() %>% image_info()

leg <- ggplot(tibble(x = 1:25, y = 1), aes(x, y)) +
  geom_tile(aes(fill = x)) +
  scale_fill_viridis_c() +
  guides(fill = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"))

leg_width <- 250
# leg_width <- 190
leg_height <- 21
ggsave("plots/monthly_rasters.png", leg,
       width = leg_width / dpi, height = leg_height / dpi,
       dpi = dpi)

leg_img <- image_read("plots/monthly_rasters.png")
leg_info <- image_info(leg_img)[c("width", "height")]
leg_text <- image_blank(leg_info$width, leg_info$height + 27, color = "gray10")
leg_text_size <- 16

leg_img <- image_append(c(leg_text, leg_img), stack = TRUE) %>%
  image_annotate("Crimes committed per month*\nper 100,000 persons",
                 location = geometry_point(11, 0), color = "white",
                 size = leg_text_size, font = "Open Sans")


leg_info <- image_info(leg_img)[c("width", "height")]
leg_sides_width <- (yb_info$width - leg_width) %/% 2
leg_sides_remainder <- (yb_info$width - leg_width) %% 2
leg_sides <- leg_sides_width %>%
  list(leg_sides_width + leg_sides_remainder) %>%
  map(~image_blank(.x, leg_info$height, "gray10"))


leg_img <- leg_img %>%
{image_append(c(leg_sides[[1]], leg_img, leg_sides[[2]]))}



title_box_trim <- 150
# title_box_trim <- 110
(title_box <- image_blank(yb_info$width, yb_info$height - title_box_trim,
                         color = "gray10") %>%
  image_annotate("Houston Crime",
                 location = geometry_point(3, -7),
                 color = "white", size = 44, font = "Open Sans") %>%
  image_annotate("Per Month Crime Rate",
                 location = geometry_point(3, 43),
                 color = "white", size = 29, font = "Open Sans"))

(title_box <- title_box %>%
                 {image_append(c(., leg_img), stack = TRUE)} %>%
                 {image_append(c(.,
                                 image_blank(yb_info$width,
                                             title_box_trim - leg_info$height,
                                             color = "gray10")), stack = TRUE)})

(title_box <- title_box %>%
    image_annotate("Low",
                   location = geometry_point(46, 180),
                   color = clr, font = fnt, size = 14) %>%
    image_annotate("High",
                   location = geometry_point(243, 180),
                   color = clr, font = fnt, size = 14))

annotate_size <- 13
# annotate_size <- 12
(title_box <- title_box %>%
    image_annotate("* Months normalized to\n   31-day months.",
                   location = geometry_point(3, h*dpi - 42),
                   color = "white", size = annotate_size, font = "Open Sans"))

sliver_width <- 10
yb <- yearmon_boxes %>%
  map(image_read) %>%
  {
    left_side <- image_join(.[seq_along(.) %% 2 == 0]) %>%
      c(title_box, .) %>%
      image_append(stack = TRUE)
    
    
    right_side <- image_join(.[seq_along(.) %% 2 == 1]) %>%
      image_append(stack = TRUE)
    
    sliver <- image_blank(sliver_width, image_info(left_side)["height"], "gray10")

    image_append(c(left_side, sliver, right_side), stack = FALSE)}

fin_dim <- image_info(yb)[c("width", "height")]

cap_height <- 15
# cap_height <-26
cap_text_size <- 12
cap_text1 <- "Luke Smith (@lksmth)"
cap_text2 <- "Source: City of Houston"
cap_x1 <- cap_text_size * str_length(cap_text1) / 2 
cap_x2 <- cap_text_size * str_length(cap_text2) / 2

(yb <- yb %>%
  {image_append(c(., image_blank(fin_dim$width, cap_height, "gray10")), stack = TRUE)} %>%
  image_annotate(cap_text1,
                 location = geometry_point(fin_dim$width - cap_x1 - 15,
                                           (fin_dim$height + cap_height) - 17),
                 color = "springgreen", size = 13, font = "Open Sans") %>%
  image_annotate(cap_text2,
                 location = geometry_point(3,
                                           (fin_dim$height + cap_height) - 16),
                 color = "gray50", size = 12, font = "Open Sans"))

image_write(yb, "plots/houston_crime_per_month.png")
file.remove(yearmon_boxes, "plots/year_boxes_legend.png")

