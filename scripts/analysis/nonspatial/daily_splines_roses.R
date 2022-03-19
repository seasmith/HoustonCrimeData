
# DATA FOR EACH TYPE OF PLOT ----------------------------------------------

hou_same_day <- hou_daily_summ %>%
  mutate(wday = wday(Date)) %>%
  group_by(wday, `Offense Type`) %>%
  summarize(per_day = mean(offense_count, na.rm = TRUE)) %>%
  ungroup()

#   -----------------------------------------------------------------------
# DAILY (DAY OF THE WEEK) SPLINES -----------------------------------------
#   -----------------------------------------------------------------------

w <- 290
h <- w * 0.5
dpi <- 100

splines_daily <- hou_same_day %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    
    t_text <- 12
    y_text <- 9
    x_text <- y_text
    
    ggplot(.x, aes(wday, per_day)) +
      geom_xspline(color = picked_colors[.y], size = 1) +
      # geom_xspline(aes(color = per_day), size = 1) +
      scale_color_viridis_c() +
      scale_y_continuous(breaks = pretty_breaks(3),
                         labels = comma,
                         expand = expand_scale()) +
      scale_x_continuous(NULL,
                         breaks = c(1, 2, 3, 4, 5, 6, 7),
                         labels = c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat "),
                         expand = expand_scale()) +
      labs(title = paste0(this_level, "\n")) +
      guides(fill = FALSE,
             color = FALSE) +
      theme_dk() +
      theme(plot.margin = unit(c(0, 0.5, 0.2, 0.4), "lines"),
            plot.title = element_text(size = t_text),
            axis.text.y = element_text(size = y_text),
            axis.text.x = element_text(size = x_text))
    
  })

spline_grobs <- map(splines_daily, ggplotGrob)

widths <- map(spline_grobs, ~.x$widths[2:5])
minWidth <- pmap(widths, unit.pmin)
class(minWidth) <- c("unit.list", "unit")

spline_grobs <- map(spline_grobs, ~{
  .x$widths[2:5] <- as.list(minWidth)
  .x
})

splines_daily <- spline_grobs %>%
  map2_chr(seq_along(.),~{
    g <- grid.arrange(arrangeGrob(.x))
    ggsave(fpath <- sprintf("plots/daily_splines%s.png", .y),
           g, width = w / dpi, height = h / dpi, dpi = dpi)
    fpath
    })


# MAGICK ------------------------------------------------------------------

i <- splines_daily %>% map(image_read)
i_info <- image_info(i[[2L]])

title_box <- image_blank(i_info$width, i_info$height, color = bg)

j <- title_box %>%
  c(i) %>%
  image_shuffle() %>%
  map(image_append, stack = TRUE) #%>%
  # reduce(function(...) image_append(c(...)))


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# DAILY (DAY OF THE WEEK) SPLINES -----------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
# ROSE PLOTS --------------------------------------------------------------
#   -----------------------------------------------------------------------

roses_daily <- hou_same_day %>%
  split(.$`Offense Type`) %>%
  map2(seq_along(.), ~{
    
    this_level <- levels(.x$`Offense Type`)[.y]
    y_min <- min(.x$per_day) - sd(.x$per_day)
    y_max <- max(.x$per_day) + (sd(.x$per_day) / 10)
    # set number of breaks
    n <- 3
    
    # create breaks
    b <- (pretty_breaks(n))(.x$per_day)
    # b <- b[-length(b)]
    # b <- if (length(b) == 3) b else b[-c(1, length(b))]
    # b <- if(length(b) == 3) b[-2] else if (length(b) == 4) b[-c(1,3)] else b
    
    # create tibble to place y-labels
    b_step <- unique(abs(diff(b))) / 5
    a <- tibble(x = 1, y = b + b_step, label = b)

    ggplot(.x, aes(xmin = wday + 1.15, xmax = wday + 0.8 + 1,
                   ymin =  y_min, ymax = per_day)) +
      geom_rect(fill = picked_colors[.y], color = bg) +
      geom_text(aes(x, y, label = as.character(label)),
                data = a, color = "white", family = "Open Sans",
                size = 2.5, inherit.aes = FALSE) +
      scale_x_continuous(limits = c(1, 10),
                         breaks = c(0, 1, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5),
                         labels = c("", "", "Sun", "Mon   ", "Tue", "Wed", "Thu", "Fri", "Sat", "")) +
      scale_y_continuous(limits = c(y_min, y_max),
                         expand = expand_scale(),
                         breaks = b, labels = b) +
      coord_polar() +
      guides(fill = FALSE, color = FALSE) +
      theme_dk() +
      theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
            panel.grid.major.y = element_line(linetype = "dashed"),
            axis.line = element_line(color = "#00000000"),
            panel.grid.major.x = element_line(color = "#00000000"),
            axis.text.y = element_blank(),
            axis.text.x = element_text(hjust = 1, vjust = 1),
            plot.title = element_text(color = "#00000000"))
  })

# convert to grobs
rose_grobs <- map(roses_daily, ggplotGrob)

# y-axis is drawn...no matter what
delete_y_axis <- function(grob) {
  info <- grob$grobs[[which(grob$layout$name == "panel")]][[c(4, 1, 4, 4)]]

  num <- length(info$id.lengths)
  len <- sum(info$id.lengths)
  del_index <- seq.int(len - info$id.lengths[num] + 1, len, 1)

  grob$grobs[[which(grob$layout$name == "panel")]][[c(4, 1, 4, 4)]]$x[del_index] <- unit(0, "null")
  grob$grobs[[which(grob$layout$name == "panel")]][[c(4, 1, 4, 4)]]$y[del_index] <- unit(0, "null")

  grob
}

rose_grobs <- map(rose_grobs, delete_y_axis)

# save y-axis-less plots to be read by magick
roses_daily <- rose_grobs %>%
  map2_chr(seq_along(.),~{
    g <- grid.arrange(arrangeGrob(.x))
    ggsave(fpath <- sprintf("plots/daily_roses%s.png", .y),
           g, width = h / dpi, height = h / dpi, dpi = dpi)
    fpath
  })

# read roses
r <- roses_daily %>% map(image_read) %>% map(image_crop, "143x+1")
r_info <- image_info(r[[2L]])

title_box <- image_blank(r_info$width, r_info$height, color = bg)

rr <- title_box %>%
  c(r) %>%
  image_shuffle() %>%
  map(image_append, stack = TRUE)


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END ROSE PLOTS ----------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
# JOIN THE TWO PLOT TYPES  ------------------------------------------------
#   -----------------------------------------------------------------------


sliver_width <- 15
sliver <- image_blank(sliver_width, image_info(rr[[1]])["height"], bg)

(d <- j %>%
  image_join(rr) %>%
  image_shuffle() %>%
  map(image_append, stack = FALSE) %>%
  {c(.[[1]], sliver, .[[2]])} %>%
  image_append()
  # reduce(function(...) image_append(c(...))))
)

(d <- d %>%
  image_annotate("Houston Crime",
                 location = geometry_point(0,-7),
                 color = clr, font = fnt, size = 40) %>%
  image_annotate("Average number of crimes per",
                 location = geometry_point(3, 45),
                 color = clr, font = fnt, size = 25) %>%
  image_annotate("day of the week from 2010 to 2017",
                 location = geometry_point(3, 77),
                 color = clr, font = fnt, size = 25) #%>%
  # image_annotate("*Does not include December 2017",
  #                location = geometry_point(0, 126),
  #                color = clr, font = fnt, size = 12)
)
cap_box <- image_blank(image_info(d)$width, 25, bg) %>%
  image_annotate("Luke Smith (@lksmth)",
                 location = geometry_point(748, 6),
                 color = "springgreen", font = fnt, size = 13) %>%
  image_annotate("Source: City of Houston",
                 location = geometry_point(2, 6),
                 color = "gray50", font = fnt, size = 13)

d <- d %>%
  image_join(cap_box) %>%
  image_append(stack = TRUE)
  

image_write(d, "plots/houston_crime_per_day.png")
file.remove(splines_daily, roses_daily)

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END JOINING THE TWO PLOT TYPES  -----------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
