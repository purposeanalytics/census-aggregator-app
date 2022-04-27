# Bounding box of Canada geography to use for bounding the map

library(cancensus)
library(dplyr)
library(sf)

canada <- get_census("CA16", c(C = 01), level = "Regions", geo_format = "sf")

canada <- canada %>%
  select(geometry)

# Create bounding box

canada <- st_bbox(canada)

usethis::use_data(canada, overwrite = TRUE)
