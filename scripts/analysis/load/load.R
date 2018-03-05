library(tidyverse)
library(scales)
library(ggridges)
library(zoo)
library(extrafont)
  loadfonts("win", quiet = TRUE)
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
load("data/hou_orig.RData")

hou_pop <- tibble(year = 2010:2017, pop = seq(2100263, 2319603, length.out = 8))
harvey <- tibble(start = as.Date("2017-08-25"), end = as.Date("2017-08-28"))


#   -----------------------------------------------------------------------
# MULTI-ALIGN AND -PLOT FUNCTION ------------------------------------------
#   -----------------------------------------------------------------------

odd <- function(x) x %% 2 == 1
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
  base_grob <- ggplotGrob(base_plot)
  
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
  minHeights <- pmap(many_heights, unit.pmin)
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
# SETUP THEME -------------------------------------------------------------
#   -----------------------------------------------------------------------

bg <- "gray10"
repick_colors <- c(4,5,6,1,7,2,3)
n <- 20
picked_colors <- viridis::viridis(7*n, begin = 0.4, end = 1)[seq.int(1, 7*n, n)][repick_colors]

fnt <- "Open Sans"
clr <- "white"

theme_dk <- function() {
  theme_minimal() +
    theme(text = element_text(color = "white", family = "Open Sans"),
          plot.background = element_rect(fill = "gray10", color = "gray10"),
          plot.margin = unit(c(0, 0, 0.2, 0.2), "lines"),
          plot.title = element_text(color = "gray90", size = 14, hjust = 0,
                                    margin = margin(0.5, 0, -0.8, -0.1, "lines")),
          plot.caption = element_text(color = "gray60", size = 8),
          panel.grid.major = element_line(color = "gray50"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(color = "gray90", size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text = element_text(color = "white", 10),
          panel.ontop = FALSE)}


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END SETUP THEME ---------------------------------------------------------
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# FILTER AND MUTATE DATA --------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------


#  What am I renaming:
#    * `# Of Offenses` to `n_offenses`
hou_orig <- hou_orig %>%
  rename(n_offenses = `# Of Offenses`)


#  What am I getting rid of:
#    * NA `Date`
#    * anything before or 2009 to 2017
#    * `Offense Type` "1" :/
hou <- hou_orig %>%
  filter(year(Date) >= 2010 & year(Date) < 2018) %>%
  filter(`Offense Type` != "1")

#  What I am changing:
#    * `Hour` values of "24" into "00"...25 hours makes no sense
#    * Removing anomalous "'" in `Hour`
#    * Removing anomalous "'" in `Beat`
#    * Replace 'UNK' with 'NA' in `Beat`
#    * `Offense Type` to plural to better convey the meaning of
#         the x- and y-axis
#    * Extract just the numeric part of `DISTRICT`
hou <- hou %>%
  mutate(Hour = if_else(Hour == "24", "00", Hour)) %>%
  mutate(Hour = str_replace(Hour, "'", "")) %>%
  mutate(Beat = str_replace(Beat, "'", "")) %>%
  mutate(Beat = if_else(Beat == "UNK", NA_character_, Beat)) %>%
  mutate(`Offense Type` = case_when(
    `Offense Type` == "Aggravated Assault" ~ "Aggravated Assaults",
    `Offense Type` == "Auto Theft" ~ "Auto Thefts",
    `Offense Type` == "Burglary" ~ "Burglaries",
    `Offense Type` == "Murder" ~ "Murders",
    `Offense Type` == "Rape" ~ "Rapes",
    `Offense Type` == "Robbery" ~ "Robberies",
    `Offense Type` == "Theft" ~ "Other Thefts"
  )) %>%
  mutate(Date = ymd_h(paste0(Date, "-", Hour))) %>%
  mutate(DISTRICT = str_extract(Beat, "[[:digit:]]+"),
         DISTRICT = as.integer(DISTRICT))


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# END FILTERING AND MUTATING DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# CHECK DATA --------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

#  Is every variable (`Offense Type`)
#    present in every month of every
#    year?  They should be

every_var_in_yearmon <- hou %>%
  distinct(year = year(Date),
           `Offense Type`,
           month = month(Date)) %>%
  add_count(`Offense Type`) %>%
  filter(n != n_distinct(as.yearmon(hou$Date)))

if (nrow(every_var_in_yearmon) > 0)
  warning("Some `Offense Type` values not present in certain months.", call. = FALSE)

rm(every_var_in_yearmon)


#   -----------------------------------------------------------------------
# FILL IN MISSING DATETIME (BY HOUR) --------------------------------------

hou <- hou %>%
  split(.$`Offense Type`) %>%
  map(~right_join(.x, tibble(Date = seq(min(hou$Date),
                                        max(hou$Date),
                                        by = "hours")))) %>%
  map(~select(.x, -`Offense Type`)) %>%
  bind_rows(.id = "Offense Type") %>%
  mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses)) %>%
  arrange(`Offense Type`) %>%
  mutate(`Offense Type` = factor(`Offense Type`, sort(unique(`Offense Type`)), ordered = TRUE))
  

#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   -----------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   END CHECK DATA --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   -----------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# SUMMARIZE DATA ----------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------

#  Summary data:
#    * Group by: crime type and year-month
#    * Join population data and create a `rate` column

hou_monthly <- hou %>%
  group_by(`Offense Type`,
           Date = as.yearmon(Date)) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup()

hou_monthly <- hou_monthly %>%
  mutate(year = year(as.Date(Date))) %>%
  left_join(hou_pop) %>%
  mutate(rate = (n_offenses / pop) * 10^5)

#  Get daily data - dates without
#    a particular crime will have
#    a missing row for that date
#
#  Join population data and create `rate` column

hou_daily_summ <- hou %>%
  mutate(n_offenses = if_else(is.na(n_offenses), 0, n_offenses)) %>%
  group_by(Date = as.Date(Date),
           `Offense Type`) %>%
  summarize(n_offenses = sum(n_offenses, na.rm = TRUE)) %>%
  ungroup()

hou_daily_summ <- hou_daily_summ %>%
  mutate(year = year(Date)) %>%
  left_join(hou_pop) %>%
  mutate(rate = (n_offenses / pop) * 10^5)

# Get yearly data
hou_yearly <- hou_daily_summ %>%
  group_by(year = year(Date), `Offense Type`, pop) %>%
  summarize(n_offenses = sum(n_offenses)) %>%
  ungroup() %>%
  mutate(rate = (n_offenses / pop) * 10^5)


#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   --------------------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   END OF SUMMARIZING DATA --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   --------------------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
