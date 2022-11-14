# Get Jenks breaks for population density

library(cancensus)
library(dplyr)
library(BAMMtools)
library(purrr)
library(glue)
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

csd_population_density_quantiles <- c(0, 1, 10, 50, 500, 1000)
csd_quantiles_text <- glue("{csd_population_density_quantiles} - {lead(csd_population_density_quantiles)}")
csd_quantiles_text[1] <- glue("< {csd_population_density_quantiles[2]}")
csd_quantiles_text <- csd_quantiles_text[-length(csd_quantiles_text)]
csd_quantiles_text[5] <- glue("{csd_population_density_quantiles[5]}+")

usethis::use_data(csd_population_density_quantiles, overwrite = TRUE)
usethis::use_data(csd_quantiles_text, overwrite = TRUE)

csd %>%
  mutate(group = cut(population_density, breaks = c(-1, 1, 10, 50, 500, 1000, 20000))) %>%
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

ct_population_density_quantiles <- c(0, 400, 1000, 2500, 10000, 80000)
ct_population_density_quantiles_text <- scales::comma(ct_population_density_quantiles)
ct_quantiles_text <- glue("{ct_population_density_quantiles_text} - {lead(ct_population_density_quantiles_text)}")
ct_quantiles_text[1] <- glue("< {ct_population_density_quantiles_text[2]}")
ct_quantiles_text <- ct_quantiles_text[-length(ct_quantiles_text)]
ct_quantiles_text[5] <- glue("{ct_population_density_quantiles_text[5]}+")

usethis::use_data(ct_population_density_quantiles, overwrite = TRUE)
usethis::use_data(ct_quantiles_text, overwrite = TRUE)

ct %>%
  mutate(group = cut(population_density, breaks = c(-1, 400, 1000, 2500, 10000, 80000))) %>%
  count(group)
