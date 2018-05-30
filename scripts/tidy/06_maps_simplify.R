library(rmapshaper)
library(sf)

# Houston police beats
pb_file <- "data/Houston_Police_Beats/Houston_Police_Beats.shp"
pb <- st_read(pb_file)

map_pol_beat_simp <- ms_simplify(bp, keep = 0.025, weighting = 0.8)

save(map_pol_beat_simp, file = "data/map_pol_beat_simp.RData")
