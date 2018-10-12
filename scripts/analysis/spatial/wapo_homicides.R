
# DEPENDENCIES ------------------------------------------------------------
library(pals)
library(tidyverse)
library(lubridate)
library(scico)
library(vapeplot)
library(sf)
library(magick)
library(extrafont); loadfonts("win", quiet = TRUE)
library(scales)


# LOAD DATA ---------------------------------------------------------------
# Load WaPo Houston data
load("data/wapo_hou.RData")

# Load police beats and major roads
bp <- st_read("data/Houston_Police_Beats/Houston_Police_Beats.shp")
mr <- st_read("data/Major_Roads/Major_Roads.gdb")



# TIDY AND AGGREGATE ------------------------------------------------------
# Quick summary
wapo_year_summ <- wapo_hou %>%
  group_by(year = year(reported_date), victim_race, disposition) %>%
  summarize(n = n()) %>%
  mutate(n_pct = n / sum(n)) %>%
  ungroup()

# Quick summary plot
wapo_year_summ %>%
  ggplot() +
  geom_line(aes(year, n_pct, color = disposition)) +
  facet_wrap(~victim_race)

# Summarize the quick summary
wapo_year_summ2 <- wapo_year_summ %>%
  group_by(year, disposition) %>%
  summarize(n = sum(n)) %>%
  mutate(n_pct = n / sum(n)) %>%
  ungroup()

# Return only 'Interstate' and 'Beltway'
mr <- mr %>%
  filter(Road_Category %in% c("Interstate", "Beltway")) %>%
  st_transform(crs = st_crs(4326)) %>%
  st_crop(st_bbox(bp))


# ANALYSIS ----------------------------------------------------------------
# Plot summary of quick summary
wapo_year_summ %>%
  ggplot(aes(year, n_pct, color = disposition)) +
  geom_line(data = wapo_year_summ2, size = 2, alpha = 0.5) +
  geom_line() +
  facet_wrap(~victim_race)

# Plot police beats and points
wapo_hou %>%
  ggplot() +
  geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
  geom_point(aes(lon, lat, color = victim_race), 
             size = 0.1) +
  geom_density_2d(aes(lon, lat, color = victim_race)) +
  facet_wrap(~victim_race) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10"),
        panel.background = element_rect(fill = "gray10"), 
        text = element_text(color = "white")) +
  coord_sf(datum = NA)


# Plot density of murders by victim race (discrete color map)
ggplot(wapo_hou) +
  geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
  geom_sf(data = mr, color = "gray75", size = 0.7) +
  geom_point(aes(lon, lat, color = victim_race), 
             size = 0.1, alpha = 0.5) +
  geom_density_2d(aes(lon, lat, color = victim_race)) +
  scale_color_discrete(guide = FALSE) +
  facet_wrap(~victim_race) +
  labs(title = "Murder by Victim Race in Houston: 2007-2017",
       subtitle = " ",
       caption = "Source: Washington Post ") +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.caption = element_text(size = 10, color = "gray50", family = "Liberation Mono"),
        plot.title = element_text(size = 20, family = "Liberation Mono"),
        panel.background = element_rect(fill = "gray10"), 
        text = element_text(color = "white", family = "Liberation Mono"),
        strip.text = element_text(size = 15, family = "Liberation Mono")) +
  coord_sf(datum = NA)

# Plot density of murders by victim race (continuous color map)
vr <- ggplot(wapo_hou) +
  geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
  geom_sf(data = mr, color = "gray75", size = 0.7) +
  geom_point(aes(lon, lat, color = victim_race), 
             size = 0.1, alpha = 0.5) +
  stat_density_2d(aes(lon, lat, fill = ..level..), geom = "polygon", alpha = 0.3) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~victim_race) +
  labs(title = "Murder by Victim Race in Houston: 2007-2017",
       subtitle = " ",
       caption = "Source: Washington Post ") +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.caption = element_text(size = 10, color = "gray50", family = "Liberation Mono"),
        plot.title = element_text(size = 20, family = "Liberation Mono"),
        panel.background = element_rect(fill = "gray10"), 
        text = element_text(color = "white", family = "Liberation Mono"),
        strip.text = element_text(size = 15, family = "Liberation Mono")) +
  coord_sf(datum = NA)

# Compute h to be the same in the following plots as in the previous
h <- wapo_hou %>%
  {c(MASS::bandwidth.nrd(.$lat), MASS::bandwidth.nrd(.$lon))}

# Manually compute colors
race_cols <- scales::hue_pal(c(0, 360) + 15, 100, 65, 0, 1)(5)
names(race_cols) <- wapo_hou %>%
  .$victim_race %>%
  unique() %>%
  sort() %>%
  .[-5]

mbr_stats <- wapo_hou %>%
  group_by(victim_race) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         n_pct = n / sum(n)) %>%
  mutate(strip_title = glue::glue("{victim_race}\n({n}/{n_tot}: {percent(n_pct)})")) %>%
  pull(strip_title) %>%
  as.character()

names(mbr_stats) <- wapo_hou %>%
  .$victim_race %>%
  unique() %>%
  sort() #%>%
  # .[-5]

mbr_plots <- wapo_hou %>%
  split(.$victim_race) %>%
  imap(~{
    p <- ggplot(.x) +
      geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.3) +
      geom_sf(data = mr, color = "gray75", size = 0.7) +
      geom_point(aes(lon, lat), 
                 # color = race_cols[.y], 
                 color = "orange",
                 size = 0.1, alpha = 0.5) +
      stat_density_2d(aes(lon, lat, fill = ..level..), geom = "polygon", alpha = 0.2, h = h) +
      scale_color_discrete(guide = FALSE) +
      scale_fill_viridis_c(option = "C") +
      guides(fill = guide_colorbar(title.position = "top", barheight = 0.5)) +
      labs(title = mbr_stats[.y]) +
      theme_void() +
      theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
            plot.caption = element_text(size = 10, color = "gray50"),
            plot.title = element_text(size = 15, hjust = 0.5, family = "Liberation Mono"),
            panel.background = element_rect(fill = "gray10"), 
            legend.position = c(0.25, 0.9),
            legend.direction = "horizontal",
            text = element_text(color = "white", family = "Liberation Mono"),
            strip.text = element_text(size = 15, family = "Liberation Mono")) +
      coord_sf(datum = NA)
    
    ggsave(f <- paste0("plots/", .y, ".png"), width = 4, height = 4, dpi = 600)
    f
  })

(mbr_plots <- mbr_plots %>%
  map(image_read) %>%
  map(image_trim))

first_row <- mbr_plots %>%
  image_join() %>%
  .[c(1, 2, 3)] %>%
  image_append()

second_row <- mbr_plots %>%
  image_join() %>%
  .[c(4, 5)] %>%
  # image_join(image_blank(394, 400, color = "gray10")) %>%
  image_join(image_blank(2360, 2400, color = "gray10")) %>%
  image_append()

comb_row <- first_row %>%
  image_join(second_row) %>%
  image_append(stack = TRUE)

# header <- image_blank(1182, 45, "gray10")
header <- image_blank(7080, 270, "gray10")

header %>%
  # image_annotate("Murder-Victim Race in Houston: 2007-2017", location = geometry_point(5, 5),
  #                color = "white", font = "Liberation Mono", size = 30) %>%
  image_annotate("Murder-Victim Race in Houston: 2007-2017", location = geometry_point(220, 50),
                 color = "white", font = "Liberation Mono", size = 150) %>%
  image_join(comb_row) %>%
  image_append(stack = TRUE) %>%
  # image_annotate("Source: Washington Post", location = geometry_point(992, 828),
  #                color = "gray50", font = "Liberation Mono", size = 13) %>%
  image_annotate("Source: Washington Post", location = geometry_point(6000, 4970),
                 color = "gray50", font = "Liberation Mono", size = 75) %>%
  # image_annotate("@lksmth", location = geometry_point(5, 828),
  #                color = "springgreen", font = "Liberation Mono", size = 13) %>%
  image_annotate("@lksmth", location = geometry_point(220, 4970),
                 color = "springgreen", font = "Liberation Mono", size = 75) %>%
  image_write("plots/murder-by-victim-heatmap.png")




# Murder by disposition (discrete color map)
ggplot(wapo_hou) +
  geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
  geom_sf(data = mr, color = "gray75", size = 0.7) +
  geom_point(aes(lon, lat, color = disposition), 
             size = 0.1, alpha = 0.5) +
  geom_density_2d(aes(lon, lat, color = disposition), h = 0.07) +
  scale_color_brewer(palette = "Accent", guide = FALSE) +
  facet_wrap(~disposition) +
  labs(title = "Murder by Case Disposition in Houston: 2007-2017",
       subtitle = " ",
       caption = "Source: Washington Post ") +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.caption = element_text(size = 10, color = "gray50", family = "Liberation Mono"),
        plot.title = element_text(size = 20, family = "Liberation Mono"),
        panel.background = element_rect(fill = "gray10"), 
        text = element_text(color = "white", family = "Liberation Mono"),
        strip.text = element_text(size = 15, family = "Liberation Mono")) +
  coord_sf(datum = NA)

# Murder by disposition (continuous color map)
# Manually compute colors
disp_cols <- RColorBrewer::brewer.pal(3, "Accent")
names(disp_cols) <- wapo_hou %>%
  .$disposition %>%
  unique() %>%
  sort()

mbd_stats <- wapo_hou %>%
  group_by(disposition) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         n_pct = n / sum(n)) %>%
  mutate(strip_title = glue::glue("{disposition}\n({n}/{n_tot}: {percent(n_pct)})")) %>%
  pull(strip_title) %>%
  as.character()

names(mbd_stats) <- wapo_hou %>%
  .$disposition %>%
  unique() %>%
  sort()

mbd_plots <- wapo_hou %>%
  split(.$disposition) %>%
  imap(~{
    p <- ggplot(.x) +
      geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
      geom_sf(data = mr, color = "gray75", size = 0.7) +
      geom_point(aes(lon, lat), 
                 # color = disp_cols[.y],
                 color = "orange",
                 size = 0.1, alpha = 0.5) +
      stat_density_2d(aes(lon, lat, fill = ..level..), geom = "polygon", alpha = 0.2, h = h) +
      scale_color_discrete(guide = FALSE) +
      scale_fill_viridis_c(option = "D") +
      guides(fill = guide_colorbar(title.position = "top", barheight = 0.5)) +
      labs(title = mbd_stats[.y]) +
      theme_void() +
      theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
            plot.caption = element_text(size = 10, color = "gray50"),
            plot.title = element_text(size = 15, hjust = 0.5, family = "Liberation Mono"),
            panel.background = element_rect(fill = "gray10"), 
            legend.position = c(0.25, 0.9),
            legend.direction = "horizontal",
            text = element_text(color = "white", family = "Liberation Mono"),
            strip.text = element_text(size = 15, family = "Liberation Mono")) +
      coord_sf(datum = NA)
    
    ggsave(f <- paste0("plots/", gsub("/", " or ", .y), ".png"), width = 4, height = 4, dpi = 100)
    f
  })

(mbd_plots <- mbd_plots %>%
  map(image_read) %>%
  map(image_trim))

first_row <- mbd_plots %>%
  image_join() %>%
  .[c(1, 2, 3)] %>%
  image_append()


header <- image_blank(1182, 45, "gray10")

header %>%
  image_annotate("Disposition of Murders in Houston: 2007-2017", location = geometry_point(5, 5),
                 color = "white", font = "Liberation Mono", size = 30) %>%
  image_join(first_row) %>%
  image_append(stack = TRUE) %>%
  image_annotate("Source: Washington Post", location = geometry_point(992, 429),
                 color = "gray50", font = "Liberation Mono", size = 13) %>%
  image_annotate("@lksmth", location = geometry_point(5, 429),
                 color = "springgreen", font = "Liberation Mono", size = 13) %>%
  image_write("plots/murder-by-disposition-heatmap.png")



# DISPOSITION ~ VICTIM_RACE -----------------------------------------------

mbdr_stats <- wapo_hou %>%
  group_by(disposition, victim_race) %>%
  summarize(n = n()) %>%
  mutate(n_tot = sum(n),
         n_pct = n / sum(n)) %>%
  mutate(strip_title = glue::glue("{n}/{n_tot} ({percent(n_pct)})"),
         search_name = paste0(disposition, "_", victim_race)) %>%
  ungroup() %>%
  group_by(victim_race) %>%
  mutate(r_tot = sum(n),
         r_pct = n / r_tot,
         strip_title3 = glue::glue("{n}/{r_tot} ({percent(r_pct)})")) %>%
  st_set_geometry(NULL)

mbdr_stats2 <- mbdr_stats %>%
  group_by(disposition) %>%
  summarize(n = sum(n)) %>%
  mutate(n_tot = sum(n),
         n_pct = n / n_tot,
         strip_title = glue::glue("{n}/{n_tot} ({percent(n_pct)})"))

search_name <- mbdr_stats %>%
  pull(search_name)

plot_title1 <- mbdr_stats %>%
  pull(strip_title3) %>%
  as.character() %>%
  set_names(search_name)

plot_title2 <- mbdr_stats %>%
  pull(strip_title) %>%
  as.character() %>%
  set_names(search_name)

mbdr_plots <- wapo_hou %>%
  mutate(disposition = fct_relevel(disposition, c("Closed without arrest", "Open/No arrest", "Closed by arrest"))) %>%
  split(.$disposition) %>%
  map(~split(.x, .x$victim_race)) %>%
  map2(names(.),
       ~map2(.x, paste0(.y, "_", names(.x)),
             ~{
               n <- if (.y == "Closed without arrest_Other") {
                 0 
               } else if (grepl("Asian", .y)) {
                 7
               } else {
                 10
               }
               p <- ggplot(.x) +
                 geom_sf(data = bp, color = "gray50", fill = "gray30", size = 0.05) +
                 geom_sf(data = mr, color = "gray75", size = 0.7) +
                 geom_point(aes(lon, lat), 
                            # color = disp_cols[.y],
                            color = "orange",
                            size = 0.1, alpha = 0.8) +
                 geom_hex(aes(lon, lat), alpha = 0.8, bins = n) +
                 scale_color_discrete(guide = FALSE) +
                 scale_fill_gradientn(colours = parula(256), labels = ) +
                 guides(fill = guide_colorbar(title.position = "top", barheight = 0.5)) +
                 labs(title = paste0("  ",
                                     "\n",
                                     "  ")) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
                       plot.caption = element_text(size = 10, color = "gray50"),
                       plot.title = element_text(size = 15, hjust = 0.5, family = "Liberation Mono"),
                       panel.background = element_rect(fill = "gray10"), 
                       legend.position = c(0.25, 0.9),
                       legend.direction = "horizontal",
                       text = element_text(color = "white", family = "Liberation Mono"),
                       strip.text = element_text(size = 15, family = "Liberation Mono")) +
                 coord_sf(datum = NA)
               
               ggsave(f <- paste0("plots/", gsub("/", " or ", .y), ".png"), width = 4, height = 4, dpi = 600)
               f
             })
  )

top_header_col <- "limegreen"
side_header_col <- "red"

{
  x <- mbdr_plots %>% 
    modify_depth(2, image_read) %>%
    modify_depth(2, image_trim)
  
  x <- x %>%
    map2(names(x),
         ~{
           pt1 = plot_title1[grepl(.y, names(plot_title1))]
           pt2 = plot_title2[grepl(.y, names(plot_title2))]
           map2(.x, names(.x),
                ~{
                  image_annotate(.x, pt1[grepl(.y, names(pt1))], location = geometry_point(110, 25),
                                 size = 20, font = "Liberation Mono", color = side_header_col) %>%
                    image_annotate(pt2[grepl(.y, names(pt2))], location = geometry_point(110, 5),
                                   size = 20, font = "Liberation Mono", color = top_header_col)
                })
         })
  
  x <-  x %>%
    map(image_join) %>%
    map(image_append, stack = TRUE) %>%
    image_join() %>%
    image_append()
  
  top_title <- names(mbdr_plots)
  
  top_header <- image_blank(401, 85, "gray10") %>%
    replicate(3, . )
  
  
  top_header[[1]] <- top_header[[1]] %>%
    image_annotate(top_title[1], location = geometry_point(20, 5), 
                   size = 30, font = "Liberation Mono", color = "white") %>%
    image_annotate(mbdr_stats2$strip_title[[1]], location = geometry_point(60, 35), 
                   size = 30, font = "Liberation Mono", color = top_header_col)
  
  top_header[[2]] <- top_header[[2]] %>%
    image_annotate(top_title[2], location = geometry_point(75, 5), 
                   size = 30, font = "Liberation Mono", color = "white") %>%
    image_annotate(mbdr_stats2$strip_title[[2]], location = geometry_point(60, 35), 
                   size = 30, font = "Liberation Mono", color = top_header_col)
  
  top_header[[3]] <- top_header[[3]] %>%
    image_annotate(top_title[3], location = geometry_point(60, 5), 
                   size = 30, font = "Liberation Mono", color = "white") %>%
    image_annotate(mbdr_stats2$strip_title[[3]], location = geometry_point(60, 35), 
                   size = 30, font = "Liberation Mono", color = top_header_col)
  
  side_title <- wapo_hou %>%
    group_by(victim_race) %>%
    summarize(n = n()) %>%
    mutate(n_tot = sum(n),
           n_pct = n / sum(n)) %>%
    mutate(top = glue::glue("{victim_race}"),
           bottom = glue::glue("{n}/{n_tot}\n({percent(n_pct)})")) %>%
    mutate_at(vars(top, bottom), as.character)
  
  side_title <- side_title %>%
  {set_names(pull(., bottom), .$top)}
  
  side_header <- image_blank(200, 412, "gray10") %>%
    replicate(5, .)
  
  side_header <- side_header %>%
    map2(seq_along(side_header), 
         ~{
           image_annotate(.x, names(side_title)[.y], location = geometry_point(10, 5), 
                          size = 30, font = "Liberation Mono", color = "white") %>%
             image_annotate(side_title[.y], location = geometry_point(10, 35), 
                            size = 30, font = "Liberation Mono", color = side_header_col)
         }) %>%
    image_join(image_blank(200, 50, "gray10"), .) %>%
    image_append(stack = TRUE) 
  
  the_title <- image_blank(1403, 50, "gray10") %>%
    image_annotate("Murders in Houston: 2007-2017",
                   color = "white", font = "Liberation Mono", size = 45,
                   location = geometry_point(10, 5))
  
  the_subtitle <- image_blank(1403, 70, "gray10") %>%
    image_annotate("By Victim Race and Disposition",
                   color = "white", font = "Liberation Mono", size = 37,
                   location = geometry_point(10, 5))
  
  the_caption <- image_blank(1403, 38, "gray10") %>%
    image_annotate(paste0("Source: Washington Post (https://github.com/washingtonpost/data-homicides)",
                          "\n",
                          "Note: Includes only murders within Houston police beats."),
                   color = "white", font = "Liberation Mono", size = 11,
                   location = geometry_point(875, 7)) %>%
    image_annotate("\n@lksmth",
                   color = "springgreen", font = "Liberation Mono", size = 13,
                   location = geometry_point(5, 5))
  
  top_header %>%
    image_join() %>%
    image_append() %>%
    image_join(x) %>%
    image_append(stack = TRUE) %>%
    image_join(side_header, .) %>%
    image_append() %>%
    image_join(the_subtitle, .) %>%
    image_join(the_title, .) %>%
    image_append(stack = TRUE) %>%
    image_join(the_caption) %>%
    image_append(stack = TRUE) #%>%
    # image_write("plots/test-heatmap.png")
}


# Slope graph
mbdr_stats %>%
  ungroup() %>%
  ggplot(aes(fct_relevel(disposition, c("Closed without arrest", "Open/No arrest", "Closed by arrest")), r_pct, color = victim_race, group = victim_race)) +
  geom_line() +
  # scale_color_brewer("Victim Race", palette = "Paired") +
  scale_color_manual("Victim Race", values = cubicl(5)) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  theme(plot.background = element_rect(fill = "gray10", color = "gray10"),
        plot.caption = element_text(size = 10, color = "gray50"),
        plot.title = element_text(size = 15, hjust = 0.5, family = "Liberation Mono"),
        panel.background = element_rect(fill = "gray10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "gray90"),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "gray90"),
        legend.position = c(0.25, 0.7),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#00000000"),
        legend.key = element_rect(fill = "#00000000", color = "#00000000"),
        text = element_text(color = "white", family = "Liberation Mono"),
        strip.text = element_text(size = 15, family = "Liberation Mono"))
