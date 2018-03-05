library(sparklyr)
library(dplyr)

df <- data.frame()
f <- list.files("data/311", "[^monthly]-clean\\.txt$", full.names = TRUE)
nms <- 2011:2017

for (i in seq_along(f)) {
  df <- sdf_bind_rows(df, spark_read_csv(sc, nms[i], f[i], delimiter = "|"), id = "year")
}

for (i in seq_along(f)) {
  x <- read.delim(f[i], sep = "|")
  names(x) <- nms[i]
  df <- bind_rows(df, x, .id = "year")
  rm(x)
}
