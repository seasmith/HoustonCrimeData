# In 2019, HPD switched from UCR to NIBRS reporting method.
# This script downloads the historic UCR data (2009-May 2018),
# and the remaining 2018 data that uses the NIBRS method.

library(tidyverse)
library(rvest)
library(curl)

# -- Beat map: http://www.houstontx.gov/police/pdfs/hpd_beat_map.pdf
# -- Substations: http://www.houstontx.gov/police/contact/substations.htm
# -- Premise Codes: http://www.houstontx.gov/police/cs/beatpages/premise.htm

url <- "http://www.houstontx.gov/police/cs/crime-stats-archives.htm"

# Must have directories
if (! dir.exists("downloads")) dir.create("downloads")

# 2009-Previous Year ------------------------------------------------------

html <- url %>%
  read_html()

links <- html %>%
  html_nodes(xpath = "//div/a") %>%
  html_attr("href") %>%
  .[grepl("\\.xls$|\\.xlsx$", .)]

links <- str_replace(links, "^/police/cs/", "") # Sep. 2011

base <- "http://www.houstontx.gov/police/cs/"

links_excel <- paste0(base, links)

dest <- "data/hou_crime_data"
dest_paths <- paste0(dest, str_replace(links, "^xls", ""))


dest_paths <- dest_paths %>%
  str_remove("\\.NIBRS_Public_Data_Group_A&B") %>%
  str_replace("[:digit:]{2}-2018", function (x) {
    d <- str_extract(x, "[:digit:]{2}")
    
    r <- lapply(d, function (i) {
      switch(i,
           `06` = "jun18",
           `07` = "jul18",
           `08` = "aug18",
           `09` = "sep18",
           `10` = "oct18",
           `11` = "nov18",
           `12` = "dec18"
           )
      })
    
    unlist(r)
  })

## Download

hou_cr_dwnld <- links_excel %>%
  map2(dest_paths, function(x, y) {
    curl_download(x, y, mode = "wb")
  })


