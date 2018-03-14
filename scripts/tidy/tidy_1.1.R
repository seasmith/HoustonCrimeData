library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)
load("data/hou_orig.RData")

#   -----------------------------------------------------------------------
# FILTER AND MUTATE DATA --------------------------------------------------

# What am I renaming:
#   * `# Of Offenses` to `n_offenses`
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
#    * Convert to NA:
#        * Beat == "UNK"
#        * Type == "-"
#        * `Block Range` == "UNK"

hou <- hou %>%
 mutate(Hour = if_else(Hour == "24", "00", Hour)) %>%
 mutate(Hour = str_replace(Hour, "'", "")) %>%
 mutate(Beat = str_replace(Beat, "'", "")) %>%
 mutate(Beat = if_else(Beat == "UNK", NA_character_, Beat)) %>%
 mutate(Type = if_else(Type == "-", NA_character_, Type)) %>%
 mutate(`Block Range` = if_else(`Block Range` == "UNK", NA_character_, `Block Range`)) %>%
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
# END FILTERING AND MUTATING DATA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   -----------------------------------------------------------------------
#   -----------------------------------------------------------------------
# CHECK DATA --------------------------------------------------------------
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
#   -----------------------------------------------------------------------

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
#   -----------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   END CHECK DATA --!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   -----------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# SAVE IT -----------------------------------------------------------------

save(hou, file = "data/hou.RData")
