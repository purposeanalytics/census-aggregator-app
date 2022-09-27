# Vectors and breakdowns for app

library(dplyr)
library(censusaggregate)
library(cancensus)
library(purrr)
library(stringr)
library(tidyr)
library(arrow)

vectors <- tibble::tribble(
  ~vector, ~label, ~label_short,
  "v_CA21_1", "Population, 2021", "population_2021",
  "v_CA21_2", "Population, 2016", "population_2016",
  "v_CA21_449", "Number of persons in private households", "population_in_private_households",
  "v_CA21_7", "Land area in square kilometres", "area",
  "v_CA21_8", "Total - Age", "age",
  "v_CA21_443", "Private households by household size", "household_size",
  "v_CA21_499", "Total number of census families in private households", "family_type",
  "v_CA21_500", "Total couple families", "couples",
  "v_CA21_543", "Household type", "household_type",
  "v_CA21_1144", "Knowledge of official languages for the total population excluding institutional residents", "knowledge_of_english",
  "v_CA21_2200", "Total - Language spoken most often at home for the total population excluding institutional residents - 100% data", "top_10_languages",
  "v_CA21_1025", "In low income based on the Low-income measure, after tax (LIM-AT)", "lim_at",
  # TODO
  # "v_CA16_3405", "Total - Immigrant status and period of immigration for the population in private households - 25% sample data", "immigrant_status",
  # "v_CA16_3852", "Total - Aboriginal identity for the population in private households - 25% sample data", "aboriginal_identity",
  # "v_CA16_3954", "Total - Visible minority for the population in private households - 25% sample data", "visible_minority",
  # "v_CA16_3999", "Total - Ethnic origin for the population in private households - 25% sample data", "ethnic_origin",
  # End TODO
  "v_CA21_4237", "Total - Private households by tenure", "household_tenure",
  # TODO - what is v_CA21_4302?
  "v_CA21_4288", "Total - Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio", "unaffordable_housing",
  # TODO
  # "v_CA16_5051", "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data", "educational_attainment",
  # END TODO
  "v_CA21_4305", "Total - Owner households in non-farm, non-reserve private dwellings", "shelter_cost_owner",
  "v_CA21_4313", "Total - Tenant households in non-farm, non-reserve private dwellings", "shelter_cost_renter"
)

# List census vectors and their children
vectors_and_children <- vectors[["vector"]] %>%
  purrr::set_names() %>%
  purrr::map_dfr(child_census_vectors,
    keep_parent = TRUE,
    .id = "highest_parent_vector"
  )

# Filter for specific children/breakdowns
# NA means keep all children, otherwise just keep the ones in the list

# Age cohort vectors
age_cohort_vectors <- vectors %>%
  filter(label_short == "age") %>%
  pull(vector) %>%
  child_census_vectors(keep_parent = TRUE) %>%
  filter(str_detect(label, "to") | str_detect(label, "and over"), units == "Number") %>%
  mutate(label_derived = str_replace(label, "years and over", "to NA years")) %>%
  separate(label_derived, into = c("min", "max"), sep = " to ", convert = TRUE) %>%
  mutate(
    max = str_remove(max, " years"), max = as.numeric(max),
    gap = max - min,
    group = case_when(
      min == 0 & max == 14 ~ "0 to 14",
      min >= 15 & max <= 24 & gap == 4 ~ "15 to 24",
      min >= 25 & max <= 44 & gap == 4 ~ "25 to 44",
      min >= 45 & max <= 64 & gap == 4 ~ "45 to 65",
      min >= 65 & max <= 84 & gap == 4 ~ "65 to 84",
      min == 85 & is.na(max) ~ "85+"
    )
  ) %>%
  filter(!is.na(group)) %>%
  select(vector, label, group)

# Language spoken at home
top_10_languages_vectors <- vectors %>%
  filter(label_short == "top_10_languages") %>%
  pull(vector) %>%
  child_census_vectors(leaves_only = TRUE) %>%
  pull(label)

# Ethnic origin
# ethnic_origin_vectors <- vectors %>%
#   filter(label_short == "ethnic_origin") %>%
#   pull(vector) %>%
#   child_census_vectors(leaves_only = TRUE) %>%
#   pull(vector)

breakdown_labels <- list(
  "population_2021" = NA,
  "household_size" = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
  # "visible_minority" = c(
  #   "v_CA16_3957", "v_CA16_3960", "v_CA16_3963", "v_CA16_3966",
  #   "v_CA16_3969", "v_CA16_3972", "v_CA16_3975", "v_CA16_3978", "v_CA16_3981",
  #   "v_CA16_3984", "v_CA16_3987", "v_CA16_3990", "v_CA16_3993",
  #   "v_CA16_3996"
  # ),
  "household_tenure" = c("Owner", "Renter", "Dwelling provided by the local government, First Nation or Indian band"),
  "unaffordable_housing" = "Spending 30% or more of income on shelter costs",
  "age" = c(
    # 5 year breakdown
    "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years",
    "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years",
    "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years",
    "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over",
    # Age cohorts
    age_cohort_vectors[["label"]]
  ),
  "knowledge_of_english" = c("English only", "English and French"),
  # "immigrant_status" = c("Immigrants", "2016 to 2021"),
  # "aboriginal_identity" = c("Aboriginal identity"),
  # "educational_attainment" = c("No certificate, diploma or degree", "Secondary (high) school diploma or equivalency certificate", "Postsecondary certificate, diploma or degree"),
  "family_type" = c("Total one-parent families", "in which the parent is a woman+"),
  # TODO
  "couples" = "With children", # Collapse into the one above - couples with children can just be part of family characteristics
  "household_type" = c("One-census-family households without additional persons", "Multigenerational households", "Multiple-census-family households", "One-census-family households with additional persons", "Two-or-more-person non-census-family households", "One-person households"),
  # "ethnic_origin" = NA,
  "area" = NA,
  "top_10_languages" = top_10_languages_vectors,
  "lim_at" = NA,
  # "ethnic_origin" = ethnic_origin_vectors,
  "shelter_cost_owner" = "Average monthly shelter costs for owned dwellings ($)",
  "shelter_cost_renter" = "Average monthly shelter costs for rented dwellings ($)"
)

filter_breakdown_vectors <- function(data, vectors, label_short) {
  original_vector_label <- vectors %>%
    filter(label_short == !!label_short) %>%
    pull(label)

  breakdown_labels <- breakdown_labels[[label_short]]

  if (!all(is.na(breakdown_labels))) {
    data <- data %>%
      filter(
        label %in% c(breakdown_labels, !!original_vector_label)
      )
  } else {
    data <- data %>%
      filter(
        label == !!original_vector_label
      )
  }

  original_vector <- vectors %>%
    filter(label_short == !!label_short) %>%
    pull(vector)

    data %>%
      filter(highest_parent_vector == original_vector) %>%
      mutate(label_short = label_short)
}

vectors_and_children_original <- vectors_and_children

vectors_and_children <- map_dfr(
  vectors[["label_short"]],
  function(label_short) {
    vectors_and_children_original %>%
      filter_breakdown_vectors(vectors, label_short)
  }
)

# Check if any are missing / mislabelled etc
breakdown_labels %>%
  unlist() %>%
  as_tibble() %>%
  filter(!is.na(value)) %>%
  anti_join(vectors_and_children, by = c("value" = "label"))

# Remove "couples" parent vector, change "with children" to be children of "family_type"
couples_with_children_vectors <- vectors_and_children %>%
  filter(label_short == "couples") %>%
  filter(vector != highest_parent_vector) %>%
  pull(vector)

family_type_vector <- vectors_and_children %>%
  filter(label_short == "family_type") %>%
  pull(highest_parent_vector) %>%
  unique()

couples_with_children <- tibble(
  vector = couples_with_children_vectors,
  parent_vector = family_type_vector,
  highest_parent_vector = family_type_vector
)

# Change parent vector and highest parent vector of LIM-AT to be v_CA16_424 (Number of persons in private households)
lim_at_vector <- vectors_and_children %>%
  filter(label_short == "lim_at") %>%
  pull(vector)

persons_in_private_households_vector <- vectors_and_children %>%
  filter(label_short == "population_in_private_households") %>%
  pull(vector)

lim_at <- tibble(
  vector = lim_at_vector,
  parent_vector = persons_in_private_households_vector,
  highest_parent_vector = persons_in_private_households_vector
)

vectors_and_children <- vectors_and_children %>%
  rows_update(couples_with_children, by = "vector") %>%
  rows_update(lim_at, by = "vector")

vectors <- vectors_and_children

usethis::use_data(vectors, overwrite = TRUE)

# Get data ----

# cancensus errors out if trying to get too much data, so do this in a few goes
# Less than 25 child vectors:
vectors_and_children_few <- vectors_and_children %>%
  add_count(highest_parent_vector) %>%
  filter(n <= 25) %>%
  select(-n)

# More than 25 child vectors:
vectors_and_children_many <- vectors_and_children %>%
  add_count(highest_parent_vector) %>%
  filter(n > 25) %>%
  select(-n)

## CSD ----

dataset <- "CA21"

csd_data_few <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CSD",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

csd_data_many <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CSD",
  vectors = vectors_and_children_many[["vector"]], labels = "short"
)

# CT ----

ct_data_few <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

# Split into two here, because of further errors

ct_data_many <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = head(vectors_and_children_many[["vector"]], 200), labels = "short"
)

n <- length(vectors_and_children_many[["vector"]])

ct_data_many_2 <- get_census(
  dataset = dataset,
  regions = list(C = 01),
  level = "CT",
  vectors = vectors_and_children_many[["vector"]][201:n], labels = "short"
)

# Pivot and combine
pivot_census_data <- function(data) {
  data %>%
    dplyr::select(
      geo_uid = .data$GeoUID,
      dplyr::starts_with("v_CA21_")
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("v_CA21_"), names_to = "vector") #%>%
    # Remove 0s for children of ethnic origin and visible minority
    # Keep 0s otherwise
    # TODO
    # mutate(remove = value == 0 & vector %in% c(ethnic_origin_vectors,)) %>%
    # filter(!remove) %>%
    # select(-remove)
}

combine_age_cohort_vectors <- function(data) {
  data_without <- data %>%
    anti_join(age_cohort_vectors, by = "vector")

  age_cohort_data <- data %>%
    inner_join(age_cohort_vectors, by = "vector") %>%
    group_by(geo_uid, group) %>%
    # No na.rm = TRUE, because if any are NA, they are all NA - suppressed or otherwise not available
    summarise(across(value, sum), .groups = "drop") %>%
    mutate(vector = group) %>%
    select(-group)

  bind_rows(
    data_without,
    age_cohort_data
  )
}

combine_with_children_vectors <- function(data) {
  couples_with_children_vectors <- vectors %>%
    filter(label == "With children") %>%
    select(vector, label) %>%
    mutate(group = "Couples with children")

  data_without <- data %>%
    anti_join(couples_with_children_vectors, by = "vector")

  couples_with_children_data <- data %>%
    inner_join(couples_with_children_vectors, by = "vector") %>%
    group_by(geo_uid, group) %>%
    # No na.rm = TRUE, because if any are NA, they are all NA - suppressed or otherwise not available
    summarise(across(value, sum), .groups = "drop") %>%
    mutate(vector = group) %>%
    select(-group)

  bind_rows(
    data_without,
    couples_with_children_data
  )
}

csd_values <- bind_rows(
  csd_data_few %>%
    pivot_census_data(),
  csd_data_many %>%
    pivot_census_data()
) %>%
  combine_age_cohort_vectors() %>%
  combine_with_children_vectors()

ct_values <- bind_rows(
  ct_data_few %>%
    pivot_census_data(),
  ct_data_many %>%
    pivot_census_data(),
  ct_data_many_2 %>%
    pivot_census_data()
) %>%
  combine_age_cohort_vectors() %>%
  combine_with_children_vectors()

# Explore missing data -----

# TODO THIS SECTION

# How many geos have almost all vectors NA?

n_vectors <- csd_values %>%
  # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
  distinct(vector) %>%
  nrow()

csd_values %>%
  # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
  filter(is.na(value)) %>%
  count(geo_uid) %>%
  mutate(prop = n / n_vectors) %>%
  count(prop) %>%
  arrange(-prop)

# 0.985, only thing available is land area
# 0.970, only thing is 2011 population and land area
# 0.955, everything except total population, area, and households is suppressed
# 0.522, data is unreliable, to be used with caution, or suppressed
# 0.493, a lot is unreliable
# 0.478, suppression

# Only INCLUDE if < 10% missing values
# What's missing then?
csd_values %>%
  # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
  filter(is.na(value)) %>%
  count(geo_uid) %>%
  mutate(prop = n / n_vectors) %>%
  filter(prop < 0.10) %>%
  distinct(geo_uid) %>%
  left_join(
    csd_values,
    # csd_values %>%
      # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)),
    by = "geo_uid"
  ) %>%
  filter(is.na(value)) %>%
  count(vector) %>%
  left_join(vectors, by = "vector") %>%
  select(highest_parent_vector, label)

# Just LIM-AT, 2011 population, and unaffordable housing
# We can just say those are not available?

# Similar for CT

# Remove any CSDs and CTs that have >10% missing data

csd_remove <- csd_values %>%
  # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
  mutate(n_vectors = n_distinct(vector)) %>%
  filter(is.na(value)) %>%
  count(geo_uid, n_vectors) %>%
  mutate(prop = n / n_vectors) %>%
  filter(prop >= 0.10) %>%
  distinct(geo_uid)

csd_values <- csd_values %>%
  anti_join(csd_remove, by = "geo_uid")

ct_remove <- ct_values %>%
  # filter(!vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
  mutate(n_vectors = n_distinct(vector)) %>%
  filter(is.na(value)) %>%
  count(geo_uid, n_vectors) %>%
  mutate(prop = n / n_vectors) %>%
  filter(prop >= 0.10) %>%
  distinct(geo_uid)

ct_values <- ct_values %>%
  anti_join(ct_remove, by = "geo_uid")

# Write removal datasets so that they are removed in geographic data sets too

saveRDS(csd_remove, here::here("data-raw", "csd_remove.rds"))
saveRDS(ct_remove, here::here("data-raw", "ct_remove.rds"))

# Save via arrow, partitioned by first 2 characters of geo_uid
csd_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "csd_values"))

ct_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "ct_values"))

