library(tidyverse)
library(curl)

# Data home page: http://mycity.houstontx.gov/home/
# 

# Must have directories
if ( !dir.exists("data") ) dir.create("data")

dz <- function(url, z_file, ex_dir = "data") {
  d_file <- paste0("downloads", "/", z_file)
  curl::curl_download(url, d_file)
  unzip(d_file, exdir = paste0(ex_dir, "/", tools::file_path_sans_ext(basename(z_file))))
  }

# POLICE DISTRICTS --------------------------------------------------------
# -- Police District Maps:https://h-gac.sharefile.com/share/view/sa86e2155b534b9e9
#      https://storage-ec2-938.sharefile.com/download.ashx?dt=dt0c0bb25133734326ab9f65fcdd10f0c6&h=qZFmRR6eMQ%2fIwGxPmIOMQ8bkAblKLGQMvAC9fPVTgn4%3d

url <- "https://storage-ec2-938.sharefile.com/download.ashx?dt=dt0c0bb25133734326ab9f65fcdd10f0c6&h=qZFmRR6eMQ%2fIwGxPmIOMQ8bkAblKLGQMvAC9fPVTgn4%3d"
dz(url, "Houston_Police_Districts.zip")


# BEATS -------------------------------------------------------------------
# -- Beatmap: https://cohgis-mycity.opendata.arcgis.com/datasets/houston-police-beats
#      https://opendata.arcgis.com/datasets/fb3bb02ec56c4bb4b9d0cf3b8b3e5545_4.zip

url <- "https://opendata.arcgis.com/datasets/fb3bb02ec56c4bb4b9d0cf3b8b3e5545_4.zip"
dz(url, "Houston_Police_Beats.zip")


# MAJOR ROADS -------------------------------------------------------------
# -- Major Roads: https://storage-ec2-917.sharefile.com/download.ashx?dt=dt7c266ce2464548b5977d3f33f5e05094&h=6VfbVAN%2b4g%2bvk8CV9blRRXqtmpfX7PK2qCKYvtXJMp4%3d

url <- "https://storage-ec2-917.sharefile.com/download.ashx?dt=dt7c266ce2464548b5977d3f33f5e05094&h=6VfbVAN%2b4g%2bvk8CV9blRRXqtmpfX7PK2qCKYvtXJMp4%3d"
dz(url, "Major_Roads.zip")


# OSM DATA ----------------------------------------------------------------
# -- OSM Extracts:
# Texas only

url <- "http://download.geofabrik.de/north-america/us/texas-latest-free.shp.zip"
dz(url, "Texas_OSM.zip")


# SCHOOLS -----------------------------------------------------------------
# -- Schools: https://cohgis-mycity.opendata.arcgis.com/datasets/schools
#      https://opendata.arcgis.com/datasets/59d52cd8fa9d463ea7cf9f3c0a0c6ea2_0.zip

url <- "https://opendata.arcgis.com/datasets/59d52cd8fa9d463ea7cf9f3c0a0c6ea2_0.zip"
dz(url, "Houston_Schools.zip")



# SCHOOL DISTRICTS --------------------------------------------------------
# -- School Districts: http://cohgis-mycity.opendata.arcgis.com/datasets/school-districts

url <- "https://opendata.arcgis.com/datasets/59d52cd8fa9d463ea7cf9f3c0a0c6ea2_1.zip"
dz(url, "School_Districts.zip")


# POLICE STATIONS ---------------------------------------------------------
# -- Police Stations: https://cohgis-mycity.opendata.arcgis.com/datasets/houston-police-stations
#      https://opendata.arcgis.com/datasets/fb3bb02ec56c4bb4b9d0cf3b8b3e5545_0.zip

url <- "https://opendata.arcgis.com/datasets/fb3bb02ec56c4bb4b9d0cf3b8b3e5545_0.zip"
dz(url, "Police_Stations.zip")


# ADDRESS DATA ------------------------------------------------------------
# -- Openaddress: https://github.com/openaddresses/openaddresses
#     http://openaddresses.io/
url <- "https://s3.amazonaws.com/data.openaddresses.io/openaddr-collected-us_south.zip"
dz(url, "US_South_Addresses.zip")


# ZIP CODES ---------------------------------------------------------------
# -- Zip Codes: http://cohgis-mycity.opendata.arcgis.com/datasets/zip-codes?geometry=-96.663%2C29.509%2C-94.048%2C29.927

url <- "https://opendata.arcgis.com/datasets/7237db114eeb416cb481f4450d8a0fa6_7.zip"
dz(url, "Zip_Codes.zip")

# NASA SEDAC --------------------------------------------------------------
# -- NASA SEDAC GPWv4: http://sedac.ciesin.columbia.edu/data/collection/gpw-v4
#
# Documents: http://sedac.ciesin.columbia.edu/downloads/on-demand/gpw/doc/GPWv4_Revision_10_documentation.zip
#
# Must be logged-in for downloads. :(
#
# Center points manually downloaded performed by selecting 'North  America' and
# 'TX' at: http://sedac.ciesin.columbia.edu/data/collection/gpw-v4
#
# Raster data manually downloaded from: http://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-density

