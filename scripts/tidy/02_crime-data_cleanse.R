library(lubridate)
library(tidyverse)
load("data/crime_data_raw.RData")
offense_classification <- read_csv("data/offense-classification.csv")
source("scripts/functions/functions.R")
# month_end()
# month_start()

END_MONTH <- as.Date("2022-01-01") # cannot trust data's end date so easily

hou <- crime_data_raw %>%
  {
    if (sum(is.na(.$offense_type)) == 4) {
      filter(., !is.na(offense_type))
    } else {
      warning("More than 4 rows w/ NA offense_type! Inspect.")
      filter(., !is.na(offense_type))
    }
  } %>%
  filter(between(occurrence_date, as.Date("2010-01-01"), month_end(END_MONTH)))

hou <- hou %>%
  filter(offense_type != "1") # I don't know what "1" is

# clean date/datetime
hou <- hou %>%
  mutate(occurrence_hour = occurrence_hour %>%
           parse_number() %>%
           str_pad(2, "left", "0")) %>%
  mutate(occurrence_hour = if_else(occurrence_hour == "24", "00", occurrence_hour))

# police beat/district info
hou <- hou %>%
 mutate(beat = str_replace(beat, "'", "")) %>%
 mutate(beat = if_else(beat == "UNK", NA_character_, beat)) %>%
 mutate(district = str_extract(beat, "[[:digit:]]+"),
        district = as.integer(district))

# Block/street info
hou <- hou %>%
  mutate(type = if_else(type == "-", NA_character_, type)) %>%
  mutate(block_range = if_else(block_range == "UNK", NA_character_, block_range)) %>%

  separate("block_range", c("block_start", "block_end"), "-", fill = "right") %>%
  mutate(block_start = if_else(block_start == "UNK", NA_character_, block_start)) %>%
  mutate(block_start = as.integer(block_start),
         block_end = as.integer(block_end)) %>%
  mutate(block_end = if_else(is.na(block_end), block_start, block_end))



# ALIGN OFFENSE DESCRIPTIONS ----------------------------------------------

hou <- hou %>%
  left_join(offense_classification,
            suffix = c("", ".keep"),
            by = c("offense_type" = "offense_description")) %>%
  select(-offense_type) %>%
  rename(offense_type = offense_type.keep) %>%
  select(1:2, offense_type, everything())

# typo
hou <- hou %>%
  mutate(offense_type = if_else(offense_type == "AutoTheft", "Auto Theft", offense_type))

# CHECK DATA --------------------------------------------------------------


#  Is every variable (offense_type)
#    present in every month of every
#    year?  They should be

every_var_in_yearmon <- hou %>%
  distinct(year = year(occurrence_date),
           offense_type,
           month = month(occurrence_date)) %>%
  add_count(offense_type) %>%
  filter(n != n_distinct(floor_date(hou$occurrence_date, "month")))

if (nrow(every_var_in_yearmon) > 0)
  warning("Some offense_type values not present in certain months.", call. = FALSE)

rm(every_var_in_yearmon)



# FILL IN MISSING DATETIME (BY HOUR) --------------------------------------


# Should `Beat` information be included?
# If not, then 'holes' will appear in plots
# such as the `week_day ~ hour` tile plot.

# Filter using last date UCR classification was used.
UCR_END_DATE <- as.Date("2018-05-31")

# Altered to reflect changes made with offense-description.csv.
# Does not include "1" or NA.
UCR_OFFENSE_TYPES <- c("Assault (Aggravated)",
                       "Theft (Auto)",
                       "Burglary",
                       "Homicide (Murder)",
                       "Sex Offense (Rape)",
                       "Robbery",
                       "Theft (Other)")
ALL_OFFENSE_TYPES <- offense_classification$offense_type
NOT_UCR_OFFENSE_TYPES <- ALL_OFFENSE_TYPES[!ALL_OFFENSE_TYPES %in% UCR_OFFENSE_TYPES]
hou <- complete(hou, offense_type, occurrence_date, occurrence_hour)

hou <- hou %>%
  # excluding all non-UCR offenses for now.
  # filter(!(occurrence_date <= UCR_END_DATE &
  #          offense_type %in% NOT_UCR_OFFENSE_TYPES))
  filter(!offense_type %in% NOT_UCR_OFFENSE_TYPES)

# SAVE IT -----------------------------------------------------------------
save(hou, file = "data/hou.RData")
