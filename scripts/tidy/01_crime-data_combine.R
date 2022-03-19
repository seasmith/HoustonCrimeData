
# DEPENDENCIES ------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)
library(tools)
library(janitor)

not_one_of <- compose(`-`, one_of)
not <- `!`


# NOTES -------------------------------------------------------------------

# THESE EXCEL FILES MUST HAVE THEIR CELL FILL COLOR
# SET TO 'NO COLOR':
# 
# * jul09
# * nov09
# * sep09
# * sep10
# * oct17
# * sep17



# SCRIPT ------------------------------------------------------------------

# -- 2009-Present
dest <- "data/hou_crime_data"

# Handle expected:1 got: 0 errors from fread()
all_files <- dest %>%
  list.files(full.names = TRUE)

# Prior to 2019 HPD tracked monthly crimes in monthly files.
# Full year crime stats now have their own file and new(current)
# year crime stats are in the "incomplete" file.
month_files <- all_files[str_detect(all_files, "complete|incomplete", negate = TRUE)]
year_files_complete  <- all_files[str_detect(all_files, "[^in]complete", negate = FALSE)]
year_files_incomplete  <- all_files[str_detect(all_files, "incomplete", negate = FALSE)]

# HPD switched to NIBRS reporting in 2018.
# The following files are NIBRS and must be read differently.
is_nibrs_file <- str_detect(month_files, paste(c("jun18", "jul18", "aug18", "sep18", 
                                                 "oct18", "nov18", "dec18"), collapse = "|"))

nibrs_files <- month_files[is_nibrs_file]
ucr_files   <- month_files[not(is_nibrs_file)]



# NIBRS IMPORT AND CLEANUP ------------------------------------------------
month_data_nibrs <- map_dfr(nibrs_files, read_excel, skip = 11)
year_data_nibrs_complete <- map_dfr(year_files_complete,
                                    read_xlsx,
                                    col_types = c("text", "date", "text", "text",
                                                  "text", "numeric", rep("text", 8) ))
year_data_nibrs_incomplete <- map_dfr(year_files_incomplete,
                                      read_xlsx,
                                      col_types = c("text", "date", "text", "text",
                                                    "text", "numeric", rep("text", 10) ))

year_data_nibrs <- bind_rows(year_data_nibrs_complete, 
                             year_data_nibrs_incomplete)

month_data_nibrs <- clean_names(month_data_nibrs)
year_data_nibrs <- clean_names(year_data_nibrs)

# do not select all NA columns
month_data_nibrs <- month_data_nibrs %>%
  select(where(function (x) (not(all(is.na(x))))))

month_data_nibrs <- month_data_nibrs %>%
  mutate(occurrence_date = as.Date(occurrence_date))

year_data_nibrs <- year_data_nibrs %>%
  mutate(occurrence_date = as.Date(occurrence_date))



# UCR IMPORT AND CLEANUP --------------------------------------------------
month_data_ucr <- ucr_files %>%
  map(read_excel) %>%
  set_names(file_path_sans_ext(basename(ucr_files)))

# coerce Date column to common type
month_data_ucr <- month_data_ucr %>%
  map(~{
    if (not(inherits(.x$Date, "POSIXct"))) {
      mutate(.x, Date = as.Date(Date, format = "%m/%d/%Y")) # RETURN
    } else {
      mutate(.x, Date = as.Date(Date)) # RETURN
      }
  })
# bind data
month_data_ucr <- map_dfr(month_data_ucr, ~.x)
month_data_ucr <- clean_names(month_data_ucr)

month_data_ucr <- month_data_ucr %>%
  rename(occurrence_date = date,
         occurrence_hour = hour,
         nibrs_description = offense_type) %>%
  mutate(offense_count = case_when(!is.na(number_of_offenses) ~ number_of_offenses,
                                        !is.na(number_offenses) ~ number_offenses,
                                        !is.na(number_of) ~ number_of,
                                        !is.na(number_offenses_2) ~ number_offenses_2,
                                        !is.na(offenses) ~ offenses,
                                        TRUE ~ number_of_offenses)) %>%
  mutate(street_name = case_when(!is.na(street_name) ~ street_name,
                                 !is.na(street_name_2) ~ street_name_2,
                                 TRUE ~ street_name)) %>%
  mutate(block_range = case_when(!is.na(block_range) ~ block_range,
                                 !is.na(block_range_2) ~ block_range_2,
                                 TRUE ~ block_range)) %>%
  select(-number_of_offenses, -number_offenses, -number_offenses_2, -number_of, -offenses,
         -street_name_2, -block_range_2, -x2, -field11)


# -------------------------------------------------------------------------
# Combine monthly/yearly NIBRS w/ UCR
crime_data_raw <- month_data_ucr %>%
  bind_rows(month_data_nibrs, year_data_nibrs)

assault_category <- c("Aggravated Assault")
auto_theft_category <- c("Auto Theft",
                         "AutoTheft", 
                         "Motor vehicle theft",
                         "Theft of motor vehicle parts or accessory")
burglary_category <- c("Burglary")
murder_category <- c("Murder, non-negligent", "Murder")
rape_category <- c("Forcible rape",
                   "Forcible sodomy",
                   "Sexual assault with an object",
                   "Statutory rape",
                   "Rape")
prostitution  <- c("Prostitution",
                   "Purchasing prostitution",
                   "Assisting or promoting prostitution")
robbery_category <- c("Robbery")
other_assault_category <- c("Simple assault")
other_sex_offense <- c("Peeping tom", 
                       "Forcible fondling",
                       "Human Trafficking/Commercial Sex Act",
                       prostitution)
other_theft_category <- c("Identify theft",
                          "Theft from building",
                          "Theft of motor vehicle parts or accessory",
                          "Theft from motor vehicle",
                          "Theft")


crime_data_raw <- crime_data_raw %>%
  mutate(offense_type = case_when(nibrs_description %in% assault_category ~ "Aggravated Assault",
                                  nibrs_description %in% auto_theft_category ~ "Auto Theft",
                                  nibrs_description %in% burglary_category ~ "Burglary",
                                  nibrs_description %in% murder_category ~ "Murder",
                                  nibrs_description %in% rape_category ~ "Rape",
                                  nibrs_description %in% robbery_category ~ "Robbery",
                                  nibrs_description %in% other_theft_category ~ "Other Theft",
                                  nibrs_description %in% prostitution ~ "Prostitution",
                                  nibrs_description %in% other_assault_category ~ "Other Assault",
                                  nibrs_description %in% other_sex_offense ~ "Other Sex Offense",
                                  TRUE ~ nibrs_description))


save(crime_data_raw, file = "data/crime_data_raw.RData")
