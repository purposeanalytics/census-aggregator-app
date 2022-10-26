# Get data for vectors

library(tidyverse)
library(cancensus)
library(censusaggregate)
library(arrow)

dataset <- "CA21"

vectors_original <- readRDS(here::here("data-raw", "intermediary", "vectors.rds"))
age_cohort_vectors <- readRDS(here::here("data-raw", "intermediary", "age_cohort_vectors.rds"))
income_vectors <- readRDS(here::here("data-raw", "intermediary", "income_vectors_grouped.rds"))

ethnic_cultural_origin_vectors <- vectors_original %>%
  filter(
    label_short == "ethnic_cultural_origin",
    !is.na(parent_vector)
  ) %>%
  pull(vector)

language_at_home_vectors <- vectors_original %>%
  filter(
    label_short == "language_at_home",
    !is.na(parent_vector)
  ) %>%
  filter(!label %in% c("English and French", "English and non-official language(s)", "French and non-official language(s)", "English, French and non-official language(s)", "English", "French")) %>%
  pull(vector)

# cancensus errors out if trying to get too much data, so do this in a few goes
# Less than 25 child vectors:
vectors_few <- vectors_original %>%
  add_count(highest_parent_vector) %>%
  filter(n <= 25) %>%
  select(-n)

# More than 25 child vectors:
vectors_many <- vectors_original %>%
  add_count(highest_parent_vector) %>%
  filter(n > 25) %>%
  select(-n)

## CSD ----

csd_data_few <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CSD",
  vectors = unique(vectors_few[["vector"]]), labels = "short"
)

# Split into two here, because of further errors

csd_data_many <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CSD",
  vectors = head(vectors_many[["vector"]], 200), labels = "short"
)

n <- length(vectors_many[["vector"]])

csd_data_many_2 <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CSD",
  vectors = vectors_many[["vector"]][201:n], labels = "short"
)

# CT ----

ct_data_few <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = unique(vectors_few[["vector"]]), labels = "short"
)

# Split into two here, because of further errors

ct_data_many <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = head(vectors_many[["vector"]], 200), labels = "short"
)

ct_data_many_2 <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = vectors_many[["vector"]][201:n], labels = "short"
)

# Pivot and combine
pivot_census_data <- function(data) {
  data %>%
    dplyr::select(
      geo_uid = .data$GeoUID,
      dplyr::starts_with("v_CA21_")
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("v_CA21_"), names_to = "vector") %>%
    # Remove 0s for children of ethnic origin, language at home
    # Keep 0s otherwise
    mutate(remove = value == 0 & vector %in% c(ethnic_cultural_origin_vectors, language_at_home_vectors)) %>%
    filter(!remove) %>%
    select(-remove)
}

csd_values <- bind_rows(
  csd_data_few %>%
    pivot_census_data(),
  csd_data_many %>%
    pivot_census_data(),
  csd_data_many_2 %>%
    pivot_census_data()
)

ct_values <- bind_rows(
  ct_data_few %>%
    pivot_census_data(),
  ct_data_many %>%
    pivot_census_data(),
  ct_data_many_2 %>%
    pivot_census_data()
)

# Explore missing data -----

# TODO THIS SECTION
#
# # How many geos have almost all vectors NA?
#
# n_vectors <- csd_values %>%
#   # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
#   distinct(vector) %>%
#   nrow()
#
# csd_values %>%
#   # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
#   filter(is.na(value)) %>%
#   count(geo_uid) %>%
#   mutate(prop = n / n_vectors) %>%
#   count(prop) %>%
#   arrange(-prop)
#
# # 0.985, only thing available is land area
# # 0.970, only thing is 2011 population and land area
# # 0.955, everything except total population, area, and households is suppressed
# # 0.522, data is unreliable, to be used with caution, or suppressed
# # 0.493, a lot is unreliable
# # 0.478, suppression
#
# # Only INCLUDE if < 10% missing values
# # What's missing then?
# csd_values %>%
#   # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
#   filter(is.na(value)) %>%
#   count(geo_uid) %>%
#   mutate(prop = n / n_vectors) %>%
#   filter(prop < 0.10) %>%
#   distinct(geo_uid) %>%
#   left_join(
#     csd_values,
#     # csd_values %>%
#     # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)),
#     by = "geo_uid"
#   ) %>%
#   filter(is.na(value)) %>%
#   count(vector) %>%
#   left_join(vectors, by = "vector") %>%
#   select(highest_parent_vector, label)
#
# # Just LIM-AT, 2011 population, and unaffordable housing
# # We can just say those are not available?
#
# # Similar for CT
#
# # Remove any CSDs and CTs that have >10% missing data
#
# csd_remove <- csd_values %>%
#   # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
#   mutate(n_vectors = n_distinct(vector)) %>%
#   filter(is.na(value)) %>%
#   count(geo_uid, n_vectors) %>%
#   mutate(prop = n / n_vectors) %>%
#   filter(prop >= 0.10) %>%
#   distinct(geo_uid)
#
# csd_values <- csd_values %>%
#   anti_join(csd_remove, by = "geo_uid")
#
# ct_remove <- ct_values %>%
#   # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
#   mutate(n_vectors = n_distinct(vector)) %>%
#   filter(is.na(value)) %>%
#   count(geo_uid, n_vectors) %>%
#   mutate(prop = n / n_vectors) %>%
#   filter(prop >= 0.10) %>%
#   distinct(geo_uid)
#
# ct_values <- ct_values %>%
#   anti_join(ct_remove, by = "geo_uid")
#
# # Write removal datasets so that they are removed in geographic data sets too
#
# saveRDS(csd_remove, here::here("data-raw", "csd_remove.rds"))
# saveRDS(ct_remove, here::here("data-raw", "ct_remove.rds"))

# Collapse / remove etc ----

# Keep 5 year age buckets and original income buckets separately, for data export and estimated median calculation

vectors_keep <- censusaggregatorapp::vectors %>%
  filter(label_short %in% c("age", "income"))

csd_keep <- csd_values %>%
  semi_join(vectors_keep, by = "vector")

ct_keep <- ct_values %>%
  semi_join(vectors_keep, by = "vector")

# Now collapse the grouped vectors

collapse_vectors <- bind_rows(
  vectors_original %>%
    filter(label == "With children") %>%
    select(vector) %>%
    mutate(new_vector = "Couples with children"),
  age_cohort_vectors %>%
    select(-label) %>%
    rename(new_vector = group),
  income_vectors
)

csd_values <- csd_values %>%
  collapse_census_vectors(collapse_vectors, aggregate = TRUE) %>%
  anti_join(vectors_keep, by = "vector") %>%
  bind_rows(csd_keep)

ct_values <- ct_values %>%
  collapse_census_vectors(collapse_vectors, aggregate = TRUE) %>%
  anti_join(vectors_keep, by = "vector") %>%
  bind_rows(ct_keep)

# Remove "couples" vector
couples_vector <- vectors_original %>%
  filter(label_short == "couples", vector == highest_parent_vector) %>%
  pull(vector)

csd_values <- csd_values %>%
  filter(vector != couples_vector)

ct_values <- ct_values %>%
  filter(vector != couples_vector)

# Save via arrow, partitioned by first 2 characters of geo_uid
csd_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "csd_values"))

ct_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "ct_values"))
