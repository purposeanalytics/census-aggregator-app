# Pull CSD and CT data along with total population, number of households, area, and population density to be loaded into mapbox

library(cancensus)
library(dplyr)
library(janitor)
library(sf)
library(mapboxapi)
library(rmapshaper)
library(sfarrow)
library(purrr)
library(mapview)

dataset <- "CA16"

canada_region <- c(C = 01)

area_vector <- "v_CA16_407"

# CSD -----

# Get "Land area in square kilometres" vector (for calculating population density), since it is
# how StatCan calculates population density. The Area field returned by cancensus is computed
# from the polygons, and might include water areas (which StatCan removes) - we want to be consistent
# with how StatCan calculates and displays these.

csd <- get_census(
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
csd <- csd %>%
  mutate(
    population_density = population / area_sq_km,
    population_density = scales::comma(population_density, accuracy = 1)
  )

# Exclude CSDs with >10% missing data (mainly 50-100% missing), since we will not be able to render reports for them anyways
csd_remove <- readRDS(here::here("data-raw", "csd_remove.rds"))

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
  tileset_id = "2016_csd",
  tileset_name = "2016_census_csd",
  multipart = TRUE
)

# Save without geography for usage in app (don't need population density either, since it needs to be derived)

csd <- csd %>%
  st_set_geometry(NULL) %>%
  select(-population_density, -region_name)

usethis::use_data(csd, overwrite = TRUE)

# CT ----

ct <- get_census(
  dataset = dataset,
  regions = canada_region,
  level = "CT",
  geo_format = "sf",
  vectors = area_vector,
  labels = "short"
) %>%
  clean_names() %>%
  select(geo_uid, pr_uid, region_name, population, households, area_sq_km = contains(tolower(area_vector)), geometry)

# Exclude CTs with >10% missing data (mainly 50-100% missing), since we will not be able to render reports for them anyways
ct_remove <- readRDS(here::here("data-raw", "ct_remove.rds"))

ct <- ct %>%
  anti_join(ct_remove, by = "geo_uid")

# Write arrow dataset, partitioned by province, for getting geometry / boundary export in app
ct_geometry <- ct %>%
  select(geo_uid, pr_uid)

dir <- "inst/extdata/ct/"
if (dir.exists(dir)) {
  fs::dir_delete(dir)
}
fs::dir_create(dir)

ct_geometry %>%
  group_by(pr_uid) %>%
  write_sf_dataset(dir,
    format = "parquet",
    hive_style = FALSE
  )

# Derive population density for tooltips
ct <- ct %>%
  mutate(
    population_density = population / area_sq_km,
    population_density = scales::comma(population_density, accuracy = 1)
  )

# Now to upload to mapbox

ct <- ct %>%
  select(-pr_uid)

upload_tiles(
  input = ct,
  username = "purposeanalytics",
  tileset_id = "2016_ct",
  tileset_name = "2016_census_ct",
  multipart = TRUE
)

# Save without geography (and population density) for usage in app

ct <- ct %>%
  st_set_geometry(NULL) %>%
  select(-population_density, -region_name)

usethis::use_data(ct, overwrite = TRUE)
