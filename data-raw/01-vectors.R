# Prepare a list of vectors used in the app

library(tidyverse)
library(cancensus)
library(censusaggregate)

# List of parent vectors ----

vectors <- tribble(
  ~vector, ~label, ~label_short,
  "v_CA21_1", "Population, 2021", "population_2021",
  "v_CA21_2", "Population, 2016", "population_2016",
  "v_CA21_449", "Number of persons in private households", "population_in_private_households",
  "v_CA21_7", "Land area in square kilometres", "area",
  "v_CA21_6", "Population density per square kilometre", "population_density",
  "v_CA21_8", "Total - Age", "age",
  "v_CA21_443", "Private households by household size", "household_size",
  "v_CA21_499", "Total number of census families in private households", "family_type",
  "v_CA21_500", "Total couple families", "couples",
  "v_CA21_543", "Household type", "household_type",
  "v_CA21_1144", "Knowledge of official languages for the total population excluding institutional residents", "knowledge_of_english_french",
  "v_CA21_2200", "Total - Language spoken most often at home for the total population excluding institutional residents - 100% data", "language_at_home",
  "v_CA21_906", "Median total income of household in 2020 ($)", "median_income",
  "v_CA21_923", "Household total income groups in 2020 for private households", "income",
  "v_CA21_1025", "In low income based on the Low-income measure, after tax (LIM-AT)", "lim_at",
  "v_CA21_4404", "Total - Immigrant status and period of immigration for the population in private households", "immigrant_status",
  "v_CA21_4201", "Total - Indigenous identity for the population in private households", "indigenous_identity",
  "v_CA21_4872", "Total - Visible minority for the population in private households", "visible_minority",
  "v_CA21_4917", "Total - Ethnic or cultural origin for the population in private households", "ethnic_cultural_origin",
  "v_CA21_4237", "Total - Private households by tenure", "household_tenure",
  "v_CA21_4288", "Total - Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio", "unaffordable_housing",
  "v_CA21_5817", "Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households", "educational_attainment",
  "v_CA21_4305", "Total - Owner households in non-farm, non-reserve private dwellings", "shelter_cost_owner",
  "v_CA21_4313", "Total - Tenant households in non-farm, non-reserve private dwellings", "shelter_cost_renter"
)

# List census vectors and their children ----

vectors_and_children <- vectors[["vector"]] %>%
  set_names() %>%
  map_dfr(child_census_vectors,
    keep_parent = TRUE,
    .id = "highest_parent_vector"
  )

# Filter for specific children/breakdowns ----

# NA means keep only the parent vector, otherwise just keep the ones in the list

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

saveRDS(age_cohort_vectors, here::here("data-raw", "intermediary", "age_cohort_vectors.rds"))

age_five_year_vectors <- vectors %>%
  filter(label_short == "age") %>%
  pull(vector) %>%
  child_census_vectors(keep_parent = TRUE) %>%
  filter(label %in% c(
    "0 to 4 years", "5 to 9 years", "10 to 14 years", "15 to 19 years", "20 to 24 years",
    "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years",
    "50 to 54 years", "55 to 59 years", "60 to 64 years", "65 to 69 years", "70 to 74 years",
    "75 to 79 years", "80 to 84 years", "85 to 89 years", "90 to 94 years", "95 to 99 years", "100 years and over"
  )) %>%
  select(vector, label)

saveRDS(age_five_year_vectors, here::here("data-raw", "intermediary", "age_five_year_vectors.rds"))

# Language spoken at home
language_at_home_vectors <- vectors %>%
  filter(label_short == "language_at_home") %>%
  pull(vector) %>%
  child_census_vectors(leaves_only = TRUE) %>%
  pull(label)

income_vectors <- vectors %>%
  filter(label_short == "income") %>%
  pull(vector) %>%
  child_census_vectors(keep_parent = TRUE, leaves_only = TRUE) %>%
  select(vector, label)

saveRDS(income_vectors, here::here("data-raw", "intermediary", "income_vectors.rds"))

# Ethnic origin
ethnic_cultural_origin_vectors <- vectors %>%
  filter(label_short == "ethnic_cultural_origin") %>%
  pull(vector) %>%
  child_census_vectors(leaves_only = TRUE) %>%
  select(vector, label)

# Breakdowns to keep

breakdown_labels <- list(
  "population_2021" = NA,
  "household_size" = c("1 person", "2 persons", "3 persons", "4 persons", "5 or more persons"),
  "visible_minority" = c(
    "Total visible minority population", "South Asian", "Chinese", "Black", "Filipino", "Arab", "Latin American",
    "Southeast Asian", "West Asian", "Korean", "Japanese", "Visible minority, n.i.e.",
    "Multiple visible minorities"
  ),
  "household_tenure" = c("Owner", "Renter", "Dwelling provided by the local government, First Nation or Indian band"),
  "unaffordable_housing" = "Spending 30% or more of income on shelter costs",
  "age" = c(
    # 5 year breakdown
    age_five_year_vectors[["label"]],
    # Age cohorts
    age_cohort_vectors[["label"]]
  ),
  "knowledge_of_english_french" = c("English only", "English and French", "French only"),
  "immigrant_status" = c("Immigrants", "2016 to 2021"),
  "indigenous_identity" = c("Indigenous identity"),
  "educational_attainment" = c("No certificate, diploma or degree", "High (secondary) school diploma or equivalency certificate", "Apprenticeship or trades certificate or diploma", "College, CEGEP or other non-university certificate or diploma", "University certificate or diploma below bachelor level", "Bachelor's degree", "University certificate or diploma above bachelor level", "Degree in medicine, dentistry, veterinary medicine or optometry", "Master's degree", "Earned doctorate"),
  "family_type" = c("Total one-parent families", "in which the parent is a woman+"),
  "couples" = "With children", # Collapse into the one above - couples with children can just be part of family characteristics
  "household_type" = c("One-census-family households without additional persons", "Multigenerational households", "Multiple-census-family households", "One-census-family households with additional persons", "Two-or-more-person non-census-family households", "One-person households"),
  "area" = NA,
  "language_at_home" = language_at_home_vectors,
  "median_income" = NA,
  "income" = income_vectors[["label"]],
  "lim_at" = NA,
  "ethnic_cultural_origin" = ethnic_cultural_origin_vectors[["label"]],
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

vectors_and_children <- map_dfr(
  vectors[["label_short"]],
  function(label_short) {
    vectors_and_children %>%
      filter_breakdown_vectors(vectors, label_short)
  }
)

# Check if any are missing / mislabelled etc
breakdown_labels %>%
  unlist() %>%
  as_tibble() %>%
  filter(!is.na(value)) %>%
  anti_join(vectors_and_children, by = c("value" = "label"))

# Keep a copy of the original / full vectors for pulling data from

original_vectors <- vectors_and_children

saveRDS(original_vectors, here::here("data-raw", "intermediary", "vectors.rds"))
