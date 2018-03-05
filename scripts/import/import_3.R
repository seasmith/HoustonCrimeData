library(rvest)
library(stringr)
library(purrr)
library(curl)

# <!--  <p>(data is updated nightly)</p></center> 
#   <p align="left"><strong>Update 01/20/2016:</strong>&nbsp;Individuals  looking for pothole data should refer to&nbsp;<a href="http://www.houstonpotholes.org/">www.houstonpotholes.org</a> for  current information. The 311 data feeds may not be  reflective of the actual &quot;work finished&quot; time for potholes due to  system integration issues.&nbsp;&nbsp;&nbsp;&nbsp;
#   <a href="http://houstontx.gov/311/311-Public-Data-All-Notes-Events-11112011-03142017-tab-delimited.csv">.</a>&nbsp;&nbsp;&nbsp;&nbsp;
#   <a href="http://houstontx.gov/311/311-Public-Data-All-Notes-Events-11112011-03142017-pipe-delimited.txt">.</a> 
#     </p>
#     -->

url <- "http://www.houstontx.gov/311/"
h <- url %>% read_html()
links <- h %>%
  html_nodes("#callToAction") %>%
  html_nodes("#threeColMiddle") %>%
  html_nodes(".contentBox") %>%
  html_nodes(".bullets") %>%
  html_nodes("li") %>%
  html_nodes("a")

hrefs <- links %>% html_attr("href")
  
l_text <- links %>% html_text()

files <- l_text %>%
  str_detect("^\\(piped\\)$")

df <- data.frame(dates = l_text[!files], type = l_text[files], link = hrefs[files], stringsAsFactors = FALSE)

dir.create("~/R/misc/violence/houston/311")

df$link %>% map(~{
  print(.x)
  curl_download(.x, paste0("data/311/", basename(.x)))
  print("Complete")
  })


# -- Public 311 Location: https://cohgis-mycity.opendata.arcgis.com/datasets/public-311-location
#      https://opendata.arcgis.com/datasets/2d51d2af86a54ebb8297288916b41574_0.zip


url <- "https://opendata.arcgis.com/datasets/2d51d2af86a54ebb8297288916b41574_0.kml"

curl_download(url, "data/311/PUBLIC_311_Location.kml")
