library(tidyverse)
library(sf)
# library(odbc)
# library(DBI)
library(RPostgreSQL)

b_file <- "data/Houston_Police_Beats/Houston_Police_Beats.shp"
bp <- st_read(b_file)

# con <- dbConnect(odbc(), driver = "PostgreSQL Driver", database = "HoustonCrimeData",
#                  uid = "postgres", pwd = getOption("psql_pwd"), host = "localhost")

# con <- RPostgres::dbConnect(RPostgres::Postgres(), user = "postgres",
#                             password = getOption("psql_pwd"), host = "localhost",
#                             dbname = "HoustonCrimeData", port  = 5432)

con <- RPostgreSQL::dbConnect(PostgreSQL(), host = "localhost", dbname = "HoustonCrimeData",
                      user = "postgres", password = getOption("psql_pwd"), port = 5432)

st_write_db(con, obj = bp, table = c("public", "Houston_Police_Beats"), drop = FALSE)
