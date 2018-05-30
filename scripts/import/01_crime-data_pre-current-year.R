library(tidyverse)
library(stringr)
library(rvest)
library(curl)

# -- Beat map: http://www.houstontx.gov/police/pdfs/hpd_beat_map.pdf
# -- Substations: http://www.houstontx.gov/police/contact/substations.htm
# -- Premise Codes: http://www.houstontx.gov/police/cs/beatpages/premise.htm

url <- "http://www.houstontx.gov/police/cs/crime-stats-archives.htm"
url2 <- "http://www.houstontx.gov/police/cs/index-2.htm" # for 2017 data

# Must have directories
if (! dir.exists("downloads")) dir.create("downloads")

# 2009-2016 ---------------------------------------------------------------

html <- url %>%
  read_html()

links_pre_cy <- html %>%
  html_nodes(xpath = "//div/a") %>%
  html_attr("href") %>%
  .[grepl("\\.xls$", .)]

links_pre_cy <- str_replace(links_pre_cy, "^/police/cs/", "")

base <- "http://www.houstontx.gov/police/cs/"

links_pre_cy_excel <- paste0(base, links_pre_cy)

dest <- "data/hou_crime_data"
dest_paths <- paste0(dest, str_replace(links_pre_cy, "^xls", ""))

## Download

hou_cr_dwnld <- links_pre_cy_excel %>%
  map2(dest_paths, function(x, y) {
    curl_download(x, y, mode = "wb")
  })


# CURRENT YEAR ------------------------------------------------------------

# NOT RUNNING ANY OF THIS, FOR NOW

# links_2018 <- url2 %>%
#   read_html() %>%
#   html_nodes("a")
# 
# # Find excel files
# is_excel <- links_2018 %>%
#   html_text() %>%
#   map_lgl(grepl, pattern = "Excel")
# 
# # Return only links to excel files
# links_excel <- links_2018 %>%
#   html_attr("href") %>%
#   .[is_excel]
# 
# dest_paths2 <- paste0(dest, str_replace(links_excel, "^xls", ""))
# 
# links_excel <- paste("http://www.houstontx.gov/police/cs", links_excel, sep = "/")
# 
# ## Download
# hou_cr_dwnld_2018 <- map2(links_excel, dest_paths2, ~curl_download(.x, .y, mode = "wb"))
