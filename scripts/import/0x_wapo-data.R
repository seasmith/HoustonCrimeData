library(curl)

# WaPo dataset url
url <- "https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"
curl_download(url, "data/wapo_homicides.csv")
