library(lubridate)
library(tidyverse)
load("data/crime_data_raw.RData")

END_MONTH <- as.Date("2022-01-01")

crime_data <- crime_data_raw %>%
  filter(between(year(occurrence_date), 2010, 2020)) %>%
  filter(offense_type != "1") %>%
  mutate(occurrence_hour = occurrence_hour %>%
           parse_number() %>%
           str_pad(2, "left", "0")) %>%
  mutate(occurrence_hour = if_else(occurrence_hour == "24", "00", occurrence_hour))

crime_data <- crime_data %>%
 mutate(beat = str_replace(beat, "'", "")) %>%
 mutate(beat = if_else(beat == "UNK", NA_character_, beat)) %>%
 mutate(type = if_else(type == "-", NA_character_, type)) %>%
 mutate(block_range = if_else(block_range == "UNK", NA_character_, block_range)) %>%
 mutate(district = str_extract(beat, "[[:digit:]]+"),
        district = as.integer(district))

crime_data <- crime_data %>%
  separate("block_range", c("block_start", "block_end"), "-", fill = "right") %>%
  mutate(block_start = if_else(block_start == "UNK", NA_character_, block_start)) %>%
  mutate(block_start = as.integer(block_start),
         block_end = as.integer(block_end)) %>%
  mutate(block_end = if_else(is.na(block_end), block_start, block_end))


# FILTER ------------------------------------------------------------------

crime_data <- crime_data %>%
  filter(floor_date(occurrence_date) <= END_MONTH)


# CHECK DATA --------------------------------------------------------------


#  Is every variable (offense_type)
#    present in every month of every
#    year?  They should be

every_var_in_yearmon <- crime_data %>%
  distinct(year = year(occurrence_date),
           offense_type,
           month = month(occurrence_date)) %>%
  add_count(offense_type) %>%
  filter(n != n_distinct(floor_date(crime_data$occurrence_date, "month")))

if (nrow(every_var_in_yearmon) > 0)
  warning("Some offense_type values not present in certain months.", call. = FALSE)

rm(every_var_in_yearmon)



# FILL IN MISSING DATETIME (BY HOUR) --------------------------------------


# Should `Beat` information be included?
# If not, then 'holes' will appear in plots
# such as the `week_day ~ hour` tile plot.

crime_data <- complete(crime_data, offense_type, occurrence_date, occurrence_hour)


# SAVE IT -----------------------------------------------------------------

save(hou, file = "data/crime_data.RData")
