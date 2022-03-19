library(tidyverse)
library(scales)
library(ggridges)
library(zoo)
library(extrafont); loadfonts("win", quiet = TRUE)
library(grid)
library(gridExtra)
library(lubridate)
library(ggalt)
library(magick)
library(nord)


# -- Beat map: http://www.houstontx.gov/police/pdfs/hpd_beat_map.pdf
# -- Substations: http://www.houstontx.gov/police/contact/substations.htm
# -- Premise Codes: http://www.houstontx.gov/police/cs/beatpages/premise.htm

# -- Population data: http://www.houstontx.gov/planning/Demographics/docs_pdfs/Cy/hist_pop_1900_2017.pdf

# -- Load
load("data/hou.RData")
load("data/hou_pop.RData")

#   -----------------------------------------------------------------------
# MULTI-ALIGN AND -PLOT FUNCTION ------------------------------------------
#   -----------------------------------------------------------------------

odd  <- function(x) x %% 2 == 1
even <- function(x) x %% 2 == 0

image_shuffle <- function(x) {
  
  n <- seq_along(x)
  o <- which(odd(n))
  e <- which(even(n))
  
  list(image_join(x[o]),
       image_join(x[e]))
  
}

image_dim <- function(x) sprintf("%sx%s", image_info(x)$width, image_info(x)$height)

image_geometry <- function(x, off) sprintf("%s%s", image_dim(x), off) 


multi_plot <- function(plots, base_plot, n = 1, draw_plot = TRUE) {
  
  # Grobify
  # print(base_plot)
  base_grob  <- ggplotGrob(base_plot)
  many_grobs <- map(plots, ggplotGrob)
  
  # Get and set widths
  many_widths <- map(many_grobs, ~.x$widths[2:5])
  minWidth <- pmap(many_widths, unit.pmin)
  class(minWidth) <- c("unit.list", "unit")
  
  many_grobs <- map(many_grobs, ~{
    .x$widths[2:5] <- as.list(minWidth)
    .x
  })
  
  # Get and set heights
  many_heights <- map(many_grobs, ~.x$heights[2:5])
  minHeights   <- pmap(many_heights, unit.pmin)
  class(minHeights) <- c("unit.list", "unit")
  
  many_grobs <- map(many_grobs, ~{
    .x$heights[2:5] <- as.list(minHeights)
    .x
  })
  
  # Add plots to base_plot
  base_grob$grobs[[which(base_grob$layout$name == "panel")]] <-
    base_grob$grobs[[which(base_grob$layout$name == "panel")]] %>%
    addGrob(arrangeGrob(grobs = many_grobs,
                        ncol = n
    ))
  
  if (draw_plot) {
    grid.newpage()
    grid.draw(base_plot)
    
  } else {
    return(base_grob)
  }
}


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END OF MULTI-ALIGN AND -PLOT FUNCTION -----------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
# OTHER FUNCTIONS ---------------------------------------------------------
#   -----------------------------------------------------------------------

index <- function(x) {
  min_x <- unique(min(x))
  max_x <- unique(max(x))
  vapply(X = x,
         FUN = function(value) (value - min_x) / (max_x - min_x),
         FUN.VALUE = numeric(1))
}

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# OTHER FUNCTIONS ---------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
# SETUP THEME -------------------------------------------------------------
#   -----------------------------------------------------------------------

bg <- "gray10"
repick_colors <- c(4,5,6,1,7,2,3)
n <- 20
picked_colors <- viridisLite::viridis(7*n, begin = 0.4, end = 1)[seq.int(1, 7*n, n)][repick_colors]

fnt <- "Open Sans"
clr <- "white"

theme_dk <- function() {
  theme_minimal() +
    theme(text = element_text(color = "white", family = "Open Sans"),
          plot.background = element_rect(fill = "gray10", color = "gray10"),
          plot.margin = unit(c(0, 0, 0.2, 0.2), "lines"),
          plot.title  = element_text(color = "gray90", size = 14, hjust = 0,
                                    margin = margin(0.5, 0, -0.8, -0.1, "lines")),
          plot.caption = element_text(color = "gray60", size = 8),
          panel.grid.major = element_line(color = "gray50"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "gray90", size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text  = element_text(color = "white", 10),
          panel.ontop = FALSE)}


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END SETUP THEME ---------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# SUMMARIZE DATA ----------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

hou_pop_summ <- hou_pop %>%
  select(matches("^UN_201[0-7]+_E")) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  gather("year", "pop") %>%
  mutate(year = str_extract(year, "[0-9]+"),
         year = as.integer(year))

harvey <- tibble(start = as.Date("2017-08-25"), end = as.Date("2017-08-28"))


#  Summary data:
#    * Group by: crime type and year-month
#    * Join population data and create a `rate` column

hou_monthly <- hou %>%
  mutate(Date = as.yearmon(occurrence_date)) %>%
  group_by(offense_type,
           Date) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
  ungroup()

hou_monthly <- hou_monthly %>%
  mutate(year = year(as.Date(Date))) %>%
  left_join(hou_pop_summ, by = "year") %>%
  mutate(rate = (offense_count / pop) * 10^5)

#  Get daily data - dates without
#    a particular crime will have
#    a missing row for that date
#
#  Join population data and create `rate` column

hou_daily_summ <- hou %>%
  mutate(offense_count = if_else(is.na(offense_count), 0, offense_count),
         Date = as.Date(occurrence_date)) %>%
  group_by(Date,
           offense_type) %>%
  summarize(offense_count = sum(offense_count, na.rm = TRUE)) %>%
  ungroup()

hou_daily_summ <- hou_daily_summ %>%
  mutate(year = year(Date)) %>%
  left_join(hou_pop_summ, by = "year") %>%
  mutate(rate = (offense_count / pop) * 10^5)

# Get yearly data
hou_yearly <- hou_daily_summ %>%
  group_by(year = year(Date), offense_type, pop) %>%
  summarize(offense_count = sum(offense_count)) %>%
  ungroup() %>%
  mutate(rate = (offense_count / pop) * 10^5)


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   --------------------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   END OF SUMMARIZING DATA --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   --------------------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
