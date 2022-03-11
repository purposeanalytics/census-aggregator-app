# Pull CSD and CT data along with total population, number of households, area, and population density to be loaded into mapbox

library(cancensus)
library(dplyr)
library(janitor)
library(sf)

dataset <- "CA16"

canada_region <- c(C = 01)

# Get without geography first, since "Area (sq km)" is as reported in the census, but with geography, it only has "Shape Area" which is the literal area of the polygon.

# CSD

csd <- get_census(dataset = dataset, regions = canada_region, level = "CSD") %>%
  clean_names() %>%
  select(geo_uid, type, region_name, area_sq_km, population, households)

csd_sf <- get_census(dataset = dataset, regions = canada_region, level = "CSD", geo_format = "sf") %>%
  clean_names() %>%
  select(geo_uid, geometry)

csd <- csd %>%
  left_join(csd_sf, by = "geo_uid") %>%
  st_as_sf(crs = st_crs(csd_sf))

# usethis::use_data(csd, overwrite = TRUE)

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

usethis::use_data(ct, overwrite = TRUE)
