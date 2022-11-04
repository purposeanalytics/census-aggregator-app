# Get Jenks breaks for population density

library(cancensus)
library(dplyr)
library(BAMMtools)
library(purrr)
devtools::load_all()

dataset <- "CA21"
canada_region <- c(C = 01)

# Pull CTs and CTs again, get population density vector
population_density_vector <- list_census_vectors(dataset) %>%
  filter(label == "Population density per square kilometre") %>%
  pull(vector)

# CSD ----

csd_population_density <- get_census(
  dataset = dataset,
  regions = canada_region,
  level = "CSD",
  vectors = population_density_vector,
  labels = "short"
) %>%
  select(geo_uid = GeoUID, population_density = tidyselect::all_of(population_density_vector))

# Limit to CSDs shown in app (some are removed)
csd <- csd_population_density %>%
  semi_join(censusaggregatorapp::csd, by = "geo_uid")

csd %>%
  pull(population_density) %>%
  hist()

# From SO https://stackoverflow.com/a/32508105
round_to <- function(x, y = 1000) {
  if ((y - x %% y) <= x %% y) {
    x + (y - x %% y)
  } else {
    x - (x %% y)
  }
}

csd_population_density_quantiles <- quantile(csd$population_density)

usethis::use_data(csd_population_density_quantiles, overwrite = TRUE)

csd_jenks_breaks <- getJenksBreaks(csd[["population_density"]], k = 5)
csd_jenks_breaks
# These are quite uneven, so do more deliberately - e.g. 0, 500, 2000, 6000, 15000?
# csd_jenks_breaks <- map_dbl(csd_jenks_breaks, round_to, 500)

csd %>%
  mutate(
    group = cut(population_density, breaks = csd_jenks_breaks),
    group = as.character(group),
    group = coalesce(group, paste0(csd_jenks_breaks[5], "+"))
  ) %>%
  count(group)

# CT ----

ct_population_density <- get_census(
  dataset = dataset,
  regions = canada_region,
  level = "CT",
  vectors = population_density_vector,
  labels = "short"
) %>%
  select(geo_uid = GeoUID, population_density = tidyselect::all_of(population_density_vector))

# Limit to CTs shown in app (some are removed)
ct <- ct_population_density %>%
  semi_join(censusaggregatorapp::ct, by = "geo_uid")

ct %>%
  pull(population_density) %>%
  hist()

ct_population_density_quantiles <- quantile(ct$population_density)

usethis::use_data(ct_population_density_quantiles, overwrite = TRUE)

ct_jenks_breaks <- getJenksBreaks(ct[["population_density"]], k = 5)
ct_jenks_breaks
ct_jenks_breaks <- map_dbl(ct_jenks_breaks, round_to, 500)
# 0, 3500, 10000, 25000, 75000
