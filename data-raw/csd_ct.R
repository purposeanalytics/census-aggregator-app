# Pull CSD and CT data along with total population, number of households, area, and population density to be loaded into mapbox

library(cancensus)
library(dplyr)
library(janitor)
library(sf)
library(mapboxapi)
library(rmapshaper)

dataset <- "CA16"

canada_region <- c(C = 01)

# Get without geography first, since "Area (sq km)" is as reported in the census, but with geography, it only has "Shape Area" which is the literal area of the polygon.

# CSD

csd <- get_census(dataset = dataset, regions = canada_region, level = "CSD") %>%
  clean_names() %>%
  select(geo_uid, area_sq_km, population, households)

csd_sf <- get_census(dataset = dataset, regions = canada_region, level = "CSD", geo_format = "sf") %>%
  clean_names() %>%
  select(geo_uid, geometry)

csd <- csd %>%
  left_join(csd_sf, by = "geo_uid") %>%
  st_as_sf(crs = st_crs(csd_sf))

# Optimizing as per recommendations in https://docs.mapbox.com/help/troubleshooting/uploads/#troubleshooting
# Since the processing takes >1 hour, it times out

# Reproject to Web Mercator (EPSG:3857)
# If not in this format, then Mapbox will reproject on upload, which takes time and can contribute to timing out

csd <- csd %>%
  st_transform(3857)

# One option is to break multipolygon into a single polygon, which will also speed up uploading
# But we don't want to do this - selection (click) should be on multipolygons, so we really don't want to let people select single polygons

# Size before:
csd_size <- object.size(csd)

# Simplify features

csd <- csd %>%
  ms_simplify(keep = 0.2, keep_shapes = TRUE)

# Size after:
csd_simplified_size <- object.size(csd)

as.numeric(csd_simplified_size) / as.numeric(csd_size)

# Upload

csd <- csd %>%
  head(1)

upload_tiles(
  input = csd,
  username = "purposeanalytics",
  tileset_id = "2016_csd_test",
  tileset_name = "2016_census_csd_test",
  multipart = TRUE
)

# CT

ct <- get_census(dataset = dataset, regions = canada_region, level = "CT") %>%
  clean_names() %>%
  select(geo_uid, type, region_name, area_sq_km, population, households)

ct_sf <- get_census(dataset = dataset, regions = canada_region, level = "CT", geo_format = "sf") %>%
  clean_names() %>%
  select(geo_uid, geometry)

ct <- ct %>%
  left_join(ct_sf, by = "geo_uid") %>%
  st_as_sf(crs = st_crs(ct_sf))

upload_tiles(
  input = ct,
  username = "purposeanalytics",
  tileset_id = "2016_ct",
  tileset_name = "2016_census_ct",
  multipart = TRUE
)

# Save CT geography only for boundary export
ct_geography <- ct %>%
  select(geo_uid)

usethis::use_data(ct_geography, overwrite = TRUE)
