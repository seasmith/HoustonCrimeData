# This file downloads the NIBRS data 

library(tidyverse)
library(rvest)
library(curl)

url <- "http://www.houstontx.gov/police/cs/Monthly_Crime_Data_by_Street_and_Police_Beat.htm"


links <- url %>%
  read_html() %>%
  html_nodes(xpath = "//p/a") %>%
  html_attr("href") %>%
  .[grepl("\\.xls$|\\.xlsx$", .)]

full_links <- paste0("http://www.houstontx.gov/police/cs/", links)

# Manually check which is being downloaded
curl_download(full_links[1], "data/hou_crime_data/2022-incomplete.xlsx")
curl_download(full_links[2], "data/hou_crime_data/2021-complete.xlsx") # may not be complete (ViewDec21)
curl_download(full_links[3], "data/hou_crime_data/2020-complete.xlsx")
curl_download(full_links[4], "data/hou_crime_data/2019-complete.xlsx")

