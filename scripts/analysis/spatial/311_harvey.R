library(tidyverse)
library(lubridate)
library(ggridges)
library(ggalt)
library(grid)
library(gridExtra)
library(naniar)
library(blg)
library(patchwork)
library(sf)

library(extrafont)
loadfonts("win", TRUE)


# NOTES -------------------------------------------------------------------

# -- Everything in the Harvey file
#    can be found in the 2017 file.

# -- There are some warnings.
#    read.delim says they are embedded nulls.

# -- Missing 'LONGITUDE' and 'LATITUDE' values
#    are filled with 'Unkown'



# IMPORT ------------------------------------------------------------------


f_2017 <- list.files("data/311", "2017-clean.txt$", full.names = TRUE)

d <- f_2017 %>%
 read_delim(delim = "|", na = c("", "NA", "Unknown"))

# set.seed(832)
# subd <- d %>% sample_frac(0.01)
# subd2 <- subd %>%
#  filter(!is.na(LONGITUDE)) %>%
#  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4326), agr = "identity")


bp <- st_read("data/Houston_Police_Beats")
roads <- st_read("data/Major_Roads/Major_Roads.gdb") %>%
 st_transform(crs = st_crs(4326))


#   -----------------------------------------------------------------------
# PLOTS -------------------------------------------------------------------
#   -----------------------------------------------------------------------


# HEATMAP - MONTHLY -------------------------------------------------------
bbox <- st_bbox(bp)
these_roads <- roads %>% filter(grepl("Interstate|Beltway", Road_Category))

heatmap <- d %>%
 mutate(month = month(as.Date(`SR CREATE DATE`), label = TRUE)) %>%
 filter(!is.na(month)) %>%
 ggplot() +
 stat_density_2d(aes(LONGITUDE, LATITUDE, fill = ..density..),
                 geom = "raster", contour = FALSE, interpolate = TRUE) +
 geom_sf(data = bp, fill = "#00000000", color = "gray11") +
 geom_sf(data = these_roads, fill = "#00000000", color = "#000000", size = 0.65) +
 scale_fill_viridis_c("Case density  ") +
 facet_wrap(~month) +
 coord_sf(datum = NA, xlim = bbox[c(1, 3)], ylim = bbox[c(2, 4)]) +
 labs(x = NULL,
      title = " ",
      y = NULL) +
 guides(fill = guide_colorbar(ticks = FALSE, barwidth = 40, title.vjust = 0.5)) +
 theme_g10() +
 theme(panel.background = element_rect(fill = viridis::viridis(2)[1]),
       plot.margin = unit(c(1, 1, 1, 1), "lines"),
       legend.position = "top",
       legend.direction = "horizontal")

heatmap1 <- heatmap +
 theme(panel.spacing = unit(0, "lines"))
heatmap2 <- heatmap +
 theme(panel.spacing.x = unit(0, "lines"), panel.spacing.y = unit(0.7, "lines"))

ggsave("plots/heatmap1.png", heatmap1, width = 940 / 100, height = 820 / 100, dpi = 100)
ggsave("plots/heatmap2.png", heatmap2, width = 940 / 100, height = 820 / 100, dpi = 100)

h <- image_read("plots/heatmap.png")

h %>%
 image_annotate("Houston 311 Issues: 2017",
                location = geometry_point(20, 0),
                font = "Open Sans", color = "white", size = 30) %>%
 image_annotate("Source: City of Houston",
                location = geometry_point(19, 802),
                font = "Open Sans", color = "gray50", size = 12) %>%
 image_annotate("Luke Smith (@lksmth)",
                location = geometry_point(789, 801),
                font = "Open Sans", color = "springgreen", size = 13) %>%
 image_annotate("_____________________________________________",
                location = geometry_point(19, 585),
                font = "Open Sans", color = "yellow", size = 12) %>%
 image_annotate("_____________________________________________",
                location = geometry_point(19, 586),
                font = "Open Sans", color = "yellow", size = 12) %>%
 image_write("plots/heatmap_annotated.png")

# RIDGES ------------------------------------------------------------------
top_20_SR_TYPE <- arrange(count(d, `SR TYPE`), desc(n)) %>% slice(1:20)

top_issues_sans_storm <- d %>%
 filter(`SR TYPE` %in% top_20_SR_TYPE$`SR TYPE`[-9]) %>%
 ggplot() +
 geom_density_ridges(aes(`SR CREATE DATE`, `SR TYPE`, height = ..density..),
                     rel_min_height = 0.01, stat = "density") +
 annotate("rect", fill = "steelblue",
          xmin = as.POSIXct("2017-08-23"), xmax = as.POSIXct("2017-08-31"),
          ymin = -Inf, ymax = Inf, alpha = 0.2) +
 # labs(title = "") +
 theme(plot.margin = unit(c(0.5, 1.1, 0.1, 0.1), "lines"),
       axis.title = element_blank())

top_issues <- d %>%
 filter(`SR TYPE` %in% top_20_SR_TYPE$`SR TYPE`) %>%
 ggplot() +
 geom_density_ridges(aes(`SR CREATE DATE`, `SR TYPE`, height = ..density..),
                     rel_min_height = 0.01, stat = "density") +
 annotate("rect", fill = "steelblue",
          xmin = as.POSIXct("2017-08-23"), xmax = as.POSIXct("2017-08-31"),
          ymin = -Inf, ymax = Inf, alpha = 0.2) +
 # labs(title = "") +
 theme(plot.margin = unit(c(0.5, 1.1, 0.1, 0.1), "lines"),
       axis.title = element_blank())

p <- top_issues_sans_storm + top_issues

title_grob <- textGrob(
 label = "Popular 311 Issues In Houston: 2017",
 x = unit(0, "lines"), 
 y = unit(0, "lines"),
 hjust = 0, vjust = 0,
 gp = gpar(fontsize = 28))

subtitle_grob <- textGrob(
 label = "",
 x = unit(0, "lines"), 
 y = unit(0, "lines"),
 hjust = 0, vjust = 0,
 gp = gpar(fontsize = 16))

titles <- arrangeGrob(title_grob, subtitle_grob, ncol = 1)

grid.newpage()
grid.draw(arrangeGrob(patchworkGrob(p), top = title_grob))
grid.newpage()
grid.draw(patchworkGrob(p))
ggsave("plots/Rplot01.png", patchworkGrob(p), width = 1200 / 100, height = 450 / 100, dpi = 100)

i <- image_read("plots/Rplot01.png")

i %>%
 image_annotate("____________________________",
                location = geometry_point(463, 15),
                color = "black", font = "Open Sans", size = 12) %>%
 image_annotate("Hurricane Harvey",
                location = geometry_point(608, 20),
                color = "black", font = "Open Sans", size = 12)

title <- image_blank(1200, 50, color = "gray10") %>%
 image_annotate("Popular 311 Issues In Houston: 2017",
                location = geometry_point(0, 0),
                font = "Open Sans", color = "white", size = 32)

title2 <- image_blank(1200, 50, color = "gray60") %>%
 image_annotate("Popular 311 Issues In Houston: 2017",
                location = geometry_point(0, 0),
                font = "Open Sans", color = "black", size = 32)

title2 %>%
 image_join(i) %>%
 image_append(stack = TRUE)

# grid.newpage()
# tiss <- arrangeGrob(top_issues_sans_storm, top = title_grob)
# grid.draw(tiss)
# grid.newpage()
# ti <- arrangeGrob(top_issues, top = title_grob)
# grid.draw()



# MISSING DATA ------------------------------------------------------------
miss_var_summary(d) %>%
 ggplot() +
 geom_col(aes(reorder(variable, percent, identity), percent), fill = "brown") +
 scale_y_continuous(trans = "log") +
 coord_flip() +
 theme_g10() +
 theme(panel.grid.major = element_blank())



# WEEKEND BIAS ------------------------------------------------------------
d %>%
 transmute_at(vars(`SR CREATE DATE`), as.Date) %>%
 count(`SR CREATE DATE`) %>%
 ggplot() +
 geom_col(aes(`SR CREATE DATE`, n))
