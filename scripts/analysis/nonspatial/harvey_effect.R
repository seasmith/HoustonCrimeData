#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# LINES -------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


hou_2017_daily <- hou_daily_summ %>%
  filter(year(Date) == 2017)

lines <- hou_2017_daily %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    p <-  ggplot(.x, aes(x = Date)) +
      geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), harvey,
                fill = "steelblue", inherit.aes = FALSE, alpha = 0.2) +
      geom_area(aes(y = n_offenses), fill = "gray70", alpha = 0.1) +
      # geom_line(aes(y = n_offenses), color = "white") +
      geom_line(aes(y = n_offenses), color = picked_colors[.y], size = 0.6) +
      scale_x_date(NULL, expand = expand_scale(),
                   minor_breaks = NULL,
                   labels = if (any(grepl("Robberies|Aggravated Assaults|Burglaries|Other Thefts", this_level))) waiver() else NULL) +
      scale_y_continuous(NULL,
                         minor_breaks = NULL,
                         breaks = scales::pretty_breaks(2),
                         expand = expand_scale()) +
      guides(color = FALSE, alpha = FALSE) +
      labs(title = paste0(this_level, "\n")) +
      theme_dk()
    
    if (this_level == "Aggravated Assaults") {
      p +
        geom_label(aes(x, y, label = l),
                   tibble(x = as.Date("2017-07-12"), y = 62, l = "Harvey"),
                   color = "gray10", size = 6, fill = "gray10") +
        geom_text(aes(x, y, label = l),
                  tibble(x = as.Date("2017-07-17"), y = 62, l = "Harvey"),
                  color = "#e0cd66", size = 6) +
        geom_text(aes(x, y, label = l),
                  tibble(x = as.Date("2017-08-18"), y = 64, l = "___"),
                  color = "#e0cd66", size = 4)
    } else {
      p
    }
  })

gb <- ggplot() +
  geom_blank() +
  labs(title = "The Harvey-Effect On Houston Crime",
       subtitle = paste0("Daily Crime Activity: January 2017 - November 2017"),
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
x <- multi_plot(lines, gb,1,F)
grid.draw(x)
ggsave("plots/lines_fat.png", x, width = 600 / 100, height = 1800 / 100, dpi = 100)



# ALT ---------------------------------------------------------------------

lines <- hou_2017_daily %>%
 split(.$`Offense Type`) %>%
 map2(seq_along(.), ~{
  
  this_level <- levels(.x$`Offense Type`)[.y]
  
  p <-  ggplot(.x, aes(x = Date)) +
   geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = Inf), harvey,
             fill = "steelblue", inherit.aes = FALSE, alpha = 0.2) +
   geom_area(aes(y = n_offenses), fill = "gray70", alpha = 0.1) +
   # geom_line(aes(y = n_offenses), color = "white") +
   geom_line(aes(y = n_offenses), color = picked_colors[.y], size = 0.6) +
   scale_x_date(NULL, expand = expand_scale(),
                minor_breaks = NULL,
                labels = waiver()) +
   scale_y_continuous(NULL,
                      minor_breaks = NULL,
                      breaks = scales::pretty_breaks(2),
                      expand = expand_scale()) +
   guides(color = FALSE, alpha = FALSE) +
   labs(title = paste0(this_level, "\n")) +
   theme_dk() +
   theme(panel.grid.major = element_blank())
  
  # if (this_level == "Aggravated Assaults") {
  #  p +
  #   geom_label(aes(x, y, label = l),
  #              tibble(x = as.Date("2017-07-12"), y = 62, l = "Harvey"),
  #              color = "gray10", size = 6, fill = "gray10") +
  #   geom_text(aes(x, y, label = l),
  #             tibble(x = as.Date("2017-07-17"), y = 62, l = "Harvey"),
  #             color = "#e0cd66", size = 6) +
  #   geom_text(aes(x, y, label = l),
  #             tibble(x = as.Date("2017-08-18"), y = 64, l = "___"),
  #             color = "#e0cd66", size = 4)
  # } else {
  #  p
  # }
  
  if (any(grepl("Rapes|Aggravated Assaults|Auto Thefts|Other Thefts", this_level))) {
   p
  } else{
   p + theme(axis.text.x = element_text(color = "gray10"))
  }
 })

lines_width <- 400
lines_height <- 200

lines_plots <- lines %>%
 map2_chr(seq_along(.), ~{
  ggsave(fp <- sprintf("plots/lines_fat_%s.png", .y), .x,
         width = lines_width / 100, height = lines_height / 100, dpi = 100)
  
  fp
 })

title_plate <- image_blank(lines_width, lines_height, color = "gray10") %>%
 image_annotate("Hurricane Harvey's Effect",
                location = geometry_point(7, 0),
                font = "Open Sans", color = "gray90", size = 32) %>%
 image_annotate("on Crime in Houston",
                location = geometry_point(7, 35),
                font = "Open Sans", color = "gray90", size = 32) %>%
 image_annotate("The city shutdown for nearly three days,",
                location = geometry_point(7, 85),
                font = "Open Sans", color = "gray90", size = 20) %>%
 image_annotate("and in many cases, so did criminal activity.",
                location = geometry_point(7, 112),
                font = "Open Sans", color = "gray90", size = 20)

sep_plate <- image_blank(15, lines_height * 4, color = "gray10")

all_plots <- lines_plots %>%
 image_read() %>%
 image_shuffle() %>%
 image_join()

x <- image_append(all_plots[1:4], stack = TRUE)
y <- image_append(image_join(title_plate, all_plots[5:7]), stack = TRUE)

the_plot <- image_append(image_join(y, sep_plate, x))

cap_plate <- image_blank((lines_width * 2) + 35, 20, color = "gray10") %>%
 image_annotate("Source: City of Houston",
                location = geometry_point(13, 2),
                font = "Open Sans", color = "gray50", size = 12) %>%
 image_annotate("Luke Smith (@lksmth)",
                location = geometry_point(693, 0),
                font = "Open Sans", color = "springgreen", size = 13)

the_plot %>%
 image_border("gray10", "10x5") %>%
 image_join(cap_plate) %>%
 image_append(stack = TRUE) %>%
 image_write("plots/the_harvey_effect.png")

file.remove(lines_plots)

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END OF LINES ------------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
