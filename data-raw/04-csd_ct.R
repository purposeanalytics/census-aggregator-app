# Pull CSD and CT data along with total population, number of households, area, and population density to be loaded into mapbox

library(tidyverse)
library(cancensus)
library(mapview)
library(janitor)
library(rmapshaper)
library(sf)
library(sfarrow)
library(mapboxapi)

dataset <- "CA21"

canada_region <- c(C = 01)

area_vector <- list_census_vectors(dataset) %>%
  filter(label == "Land area in square kilometres") %>%
  pull(vector)

# CSD -----

# Get "Land area in square kilometres" vector (for calculating population density), since it is
# how StatCan calculates population density. The Area field returned by cancensus is computed
# from the polygons, and might include water areas (which StatCan removes) - we want to be consistent
# with how StatCan calculates and displays these.

csd_raw <- get_census(
  dataset = dataset,
  regions = canada_region,
  level = "CSD",
  geo_format = "sf",
  vectors = area_vector,
  labels = "short"
) %>%
  clean_names() %>%
  select(geo_uid, pr_uid, region_name = name, population, households, area_sq_km = contains(tolower(area_vector)), geometry)

# Derive population density for tooltips
csd <- csd_raw %>%
  mutate(
    population_density = population / area_sq_km
  )

# Exclude CSDs with >10% missing data (mainly 50-100% missing), since we will not be able to render reports for them anyways
csd_remove <- readRDS(here::here("data-raw", "intermediary", "csd_remove.rds"))

csd <- csd %>%
  anti_join(csd_remove, by = "geo_uid")

# Simplify features - based on number of points - required for uploading to mapbox
# without it timing out, and also to make data storage easier for us

# Size before:
csd_size <- object.size(csd)

csd <- csd %>%
  split(.$geo_uid) %>%
  map_dfr(function(feature) {
    pts <- npts(feature)

    if (pts > 10000) {
      ms_simplify(feature, keep = 0.1, keep_shapes = TRUE)
    } else if (pts > 5000) {
      ms_simplify(feature, keep = 0.3, keep_shapes = TRUE)
    } else if (pts > 500) {
      ms_simplify(feature, keep = 0.5, keep_shapes = TRUE)
    } else {
      feature
    }
  })

# Make geometries valid

csd <- csd %>%
  st_make_valid()

# Size after:
csd_simplified_size <- object.size(csd)

as.numeric(csd_simplified_size) / as.numeric(csd_size)

# Write arrow dataset, partitioned by province, for getting geometry / boundary export in app
csd_geometry <- csd %>%
  select(geo_uid, pr_uid)

dir <- "inst/extdata/csd/"
if (dir.exists(dir)) {
  fs::dir_delete(dir)
}
fs::dir_create(dir)

csd_geometry %>%
  group_by(pr_uid) %>%
  write_sf_dataset(dir,
    format = "parquet",
    hive_style = FALSE
  )

# Create formatted version of values, round original population density
csd <- csd %>%
  mutate(
    across(c(population, households), .fns = list(fmt = scales::comma)),
    across(c(area_sq_km, population_density), .fns = list(fmt = ~ scales::comma(.x, accuracy = 0.1))),
    population_density = round(population_density)
  )

# Remove original values (except population density)
csd <- csd %>%
  select(-population, -households, -area_sq_km)

# Now to upload to mapbox

csd <- csd %>%
  select(-pr_uid)

# Optimizing as per recommendations in https://docs.mapbox.com/help/troubleshooting/uploads/#troubleshooting
# Since the processing takes >1 hour, it times out

# Reproject to Web Mercator (EPSG:3857)
# If not in this format, then Mapbox will reproject on upload, which takes time and can contribute to timing out

csd <- csd %>%
  st_transform(3857)

# Upload

upload_tiles(
  input = csd,
  username = "purposeanalytics",
  tileset_id = "2021_csd",
  tileset_name = "2021_census_csd",
  multipart = TRUE
)

# Save without geography for usage in app (don't need population density either, since it needs to be derived)

csd <- csd %>%
  st_set_geometry(NULL) %>%
  select(-region_name, -tidyselect::ends_with("fmt"))

usethis::use_data(csd, overwrite = TRUE)

# CT ----

ct_raw <- get_census(
  dataset = dataset,
  regions = canada_region,
  level = "CT",
  geo_format = "sf",
  vectors = area_vector,
  labels = "short"
) %>%
  clean_names() %>%
  select(geo_uid, cd_uid, csd_uid, population, households, area_sq_km = contains(tolower(area_vector)), geometry)

# Add region_name on, via csd
csd_name <- csd_raw %>%
  st_set_geometry(NULL) %>%
  distinct(csd_uid = geo_uid, region_name)

ct <- ct_raw %>%
  left_join(csd_name, by = "csd_uid") %>%
  select(-csd_uid)

# Exclude CTs with >5% missing data (mainly 50-100% missing), since we will not be able to render reports for them anyways
ct_remove <- readRDS(here::here("data-raw", "intermediary", "ct_remove.rds"))

ct <- ct %>%
  anti_join(ct_remove, by = "geo_uid")

# Write arrow dataset, partitioned by cd, for getting geometry / boundary export in app
ct_geometry <- ct %>%
  select(geo_uid, cd_uid)

dir <- "inst/extdata/ct/"
if (dir.exists(dir)) {
  fs::dir_delete(dir)
}
fs::dir_create(dir)

ct_geometry %>%
  group_by(cd_uid) %>%
  write_sf_dataset(dir,
    format = "parquet",
    hive_style = FALSE
  )

# Derive population density for tooltips
ct <- ct %>%
  mutate(
    population_density = population / area_sq_km,
  )

# Create formatted version of values, round original population density
ct <- ct %>%
  mutate(
    across(c(population, households), .fns = list(fmt = scales::comma)),
    across(c(area_sq_km, population_density), .fns = list(fmt = ~ scales::comma(.x, accuracy = 0.1))),
    population_density = round(population_density)
  )

# Remove original values (except population density)
ct <- ct %>%
  select(-population, -households, -area_sq_km)

# Now to upload to mapbox

ct <- ct %>%
  select(-cd_uid)

upload_tiles(
  input = ct,
  username = "purposeanalytics",
  tileset_id = "2021_ct",
  tileset_name = "2021_census_ct",
  multipart = TRUE
)

# Save without geography (and population density, and formatted values) for usage in app

ct <- ct %>%
  st_set_geometry(NULL) %>%
  select(-population_density, -region_name, -tidyselect::("ends_with"))

usethis::use_data(ct, overwrite = TRUE)
