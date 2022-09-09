# Vectors and breakdowns for app

library(dplyr)
library(censusaggregate)
library(cancensus)
library(purrr)
library(stringr)
library(tidyr)
library(arrow)

vectors <- tibble::tribble(
  ~vector, ~vector_label, ~vector_label_short,
  "v_CA16_401", "Population, 2016", "population_2016",
  "v_CA16_402", "Population, 2011", "population_2011",
  "v_CA16_407", "Land area in square kilometres", "area",
  "v_CA16_1", "Total - Age", "age",
  "v_CA16_418", "Private households by household size", "household_size",
  "v_CA16_484", "Total number of census families in private households - 100% data", "family_type",
  "v_CA16_504", "Total - Private households by household type - 100% data", "household_type",
  "v_CA16_512", "Total - Knowledge of official languages for the total population excluding institutional residents - 100% data", "knowledge_of_english",
  "v_CA16_1355", "Total - Language spoken most often at home for the total population excluding institutional residents - 100% data", "top_10_languages",
  "v_CA16_2525", "In low income based on the Low-income measure, after tax (LIM-AT)", "lim_at",
  "v_CA16_3405", "Total - Immigrant status and period of immigration for the population in private households - 25% sample data", "immigrant_status",
  "v_CA16_3852", "Total - Aboriginal identity for the population in private households - 25% sample data", "aboriginal_identity",
  "v_CA16_3954", "Total - Visible minority for the population in private households - 25% sample data", "visible_minority",
  "v_CA16_3999", "Total - Ethnic origin for the population in private households - 25% sample data", "ethnic_origin",
  "v_CA16_4836", "Total - Private households by tenure - 25% sample data", "household_tenure",
  "v_CA16_4886", "Total -  Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio - 25% sample data", "unaffordable_housing",
  "v_CA16_5051", "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households - 25% sample data", "educational_attainment"
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
age_cohort_vectors <- child_census_vectors("v_CA16_1", keep_parent = TRUE) %>%
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
  select(vector, group)

# Language spoken at home
v_CA16_1355_vectors <- child_census_vectors("v_CA16_1355", leaves_only = TRUE) %>%
  pull(vector)

# Ethnic origin
v_CA16_3999_vectors <- child_census_vectors("v_CA16_3999", leaves_only = TRUE) %>%
  pull(vector)

breakdown_vectors <- list(
  "v_CA16_401" = NA,
  "v_CA16_418" = c("v_CA16_419", "v_CA16_420", "v_CA16_421", "v_CA16_422", "v_CA16_423"),
  "v_CA16_3954" = c(
    "v_CA16_3957", "v_CA16_3960", "v_CA16_3963", "v_CA16_3966",
    "v_CA16_3969", "v_CA16_3972", "v_CA16_3975", "v_CA16_3978", "v_CA16_3981",
    "v_CA16_3984", "v_CA16_3987", "v_CA16_3990", "v_CA16_3993",
    "v_CA16_3996"
  ),
  "v_CA16_4836" = c("v_CA16_4837", "v_CA16_4838", "v_CA16_4839"),
  "v_CA16_4886" = "v_CA16_4888",
  "v_CA16_1" = c(
    # 5 year breakdown
    "v_CA16_7", "v_CA16_25", "v_CA16_43", "v_CA16_64", "v_CA16_82",
    "v_CA16_100", "v_CA16_118", "v_CA16_136", "v_CA16_154", "v_CA16_172",
    "v_CA16_190", "v_CA16_208", "v_CA16_226", "v_CA16_247", "v_CA16_265",
    "v_CA16_283", "v_CA16_301", "v_CA16_322", "v_CA16_340", "v_CA16_358", "v_CA16_376",
    # Age cohorts
    age_cohort_vectors[["vector"]]
  ),
  "v_CA16_512" = c("v_CA16_515", "v_CA16_521"),
  "v_CA16_3405" = c("v_CA16_3411", "v_CA16_3432"),
  "v_CA16_3852" = c("v_CA16_3855"),
  "v_CA16_5051" = c("v_CA16_5054", "v_CA16_5057", "v_CA16_5060"),
  "v_CA16_484" = c("v_CA16_485", "v_CA16_488", "v_CA16_489"),
  "v_CA16_504" = c("v_CA16_505", "v_CA16_508", "v_CA16_509"),
  "v_CA16_402" = NA,
  "v_CA16_407" = NA,
  "v_CA16_1355" = v_CA16_1355_vectors,
  "v_CA16_2525" = NA,
  "v_CA16_3999" = v_CA16_3999_vectors
)

filter_breakdown_vectors <- function(data, vector) {
  vector_breakdown_vectors <- breakdown_vectors[[vector]]

  if (!all(is.na(vector_breakdown_vectors))) {
    data <- data %>%
      filter(
        vector %in% c(vector_breakdown_vectors, !!vector)
      )
  } else {
    data <- data %>%
      filter(
        vector == !!vector
      )
  }

  data
}

vectors_and_children <- map_dfr(
  vectors[["vector"]],
  function(vector) {
    vectors_and_children %>%
      filter_breakdown_vectors(vector)
  }
)

# Save vectors
vectors <- vectors_and_children %>%
  left_join(vectors, by = c("vector", "label" = "vector_label"))

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

csd_data_few <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CSD",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

csd_data_many <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CSD",
  vectors = vectors_and_children_many[["vector"]], labels = "short"
)

# CT ----

ct_data_few <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = unique(vectors_and_children_few[["vector"]]), labels = "short"
)

# Split into two here, because of further errors

ct_data_many <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = head(vectors_and_children_many[["vector"]], 200), labels = "short"
)

n <- length(vectors_and_children_many[["vector"]])

ct_data_many_2 <- get_census(
  dataset = "CA16",
  regions = list(C = 01),
  level = "CT",
  vectors = vectors_and_children_many[["vector"]][201:n], labels = "short"
)

# Pivot and combine
pivot_census_data <- function(data) {
  data %>%
    dplyr::select(
      geo_uid = .data$GeoUID,
      dplyr::starts_with("v_CA16_")
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("v_CA16_"), names_to = "vector") %>%
    # Remove 0s for children of v_CA16_3999 (ethnic origin) and v_CA16_1355 (visible minority)
    # Keep 0s otherwise
    mutate(remove = value == 0 & vector %in% c(v_CA16_3999_vectors, v_CA16_1355_vectors)) %>%
    filter(!remove) %>%
    select(-remove)
}

combine_age_cohort_vectors <- function(data) {
  data_without <- data %>%
    anti_join(age_cohort_vectors, by = "vector")

  age_cohort_data <- data %>%
    left_join(age_cohort_vectors, by = "vector") %>%
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

csd_values <- bind_rows(
  csd_data_few %>%
    pivot_census_data(),
  csd_data_many %>%
    pivot_census_data()
) %>%
  combine_age_cohort_vectors()

ct_values <- bind_rows(
  ct_data_few %>%
    pivot_census_data(),
  ct_data_many %>%
    pivot_census_data(),
  ct_data_many_2 %>%
    pivot_census_data()
) %>%
  combine_age_cohort_vectors()

# Save via arrow, partitioned by first 2 characters of geo_uid
csd_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "csd_values"))

ct_values %>%
  mutate(id = str_sub(geo_uid, 1, 2)) %>%
  group_by(id) %>%
  write_dataset(here::here("inst", "extdata", "ct_values"))
