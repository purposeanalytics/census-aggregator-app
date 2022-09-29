# Collapse, recode, etc vectors

library(tidyverse)
library(censusaggregate)

vectors <- readRDS(here::here("data-raw", "intermediary", "vectors.rds"))
age_cohort_vectors <- readRDS(here::here("data-raw", "intermediary", "age_cohort_vectors.rds"))

# Collapse age cohort vectors and "with children" vectors ----

collapse_vectors <- bind_rows(
  age_cohort_vectors %>%
    select(-label) %>%
    rename(new_vector = group),
  vectors %>%
    filter(label == "With children") %>%
    select(vector) %>%
    mutate(new_vector = "Couples with children")
)

vectors <- vectors %>%
  collapse_census_vectors(collapse_vectors)

vectors <- vectors %>% mutate(label_short = case_when(
  vector %in% age_cohort_vectors[["group"]] ~ "age_cohorts",
  TRUE ~ label_short
))

# Change parent of "Couples with children" to be "family_type" vector, change parent of "lim_at" to be "population_in_private_households" vector ----
family_type_vector <- vectors %>%
  filter(label_short == "family_type") %>%
  pull(highest_parent_vector) %>%
  unique()

lim_at_vector <- vectors %>%
  filter(label_short == "lim_at") %>%
  pull(vector)

persons_in_private_households_vector <- vectors %>%
  filter(label_short == "population_in_private_households") %>%
  pull(vector)

replacement_parent_vectors <- bind_rows(
  tibble(vector = "Couples with children", new_parent_vector = family_type_vector, new_label_short = "family_type"),
  tibble(vector = lim_at_vector, new_parent_vector = persons_in_private_households_vector)
)

vectors <- vectors %>%
  reassign_parent_vector(replacement_parent_vectors)

# Remove "couples" vector ----
couples_vector <- vectors %>%
  filter(label_short == "couples") %>%
  pull(vector)

vectors <- vectors %>%
  filter(vector != couples_vector)

# Recode vectors and breakdowns ----

parent_vectors_recoding <- tibble::tribble(
  ~label, ~new_label,
  "Number of persons in private households", "Low income (LIM-AT)",
  "Total - Age", "Age",
  "Private households by household size", "Household size",
  "Total number of census families in private households", "Family type",
  "Household type", "Multiple family households",
  "Knowledge of official languages for the total population excluding institutional residents", "Knowledge of English",
  "Total - Language spoken most often at home for the total population excluding institutional residents - 100% data", "Top 10 languages spoken most often at home",
  "Total - Private households by tenure", "Household tenure",
  "Total - Owner and tenant households with household total income greater than zero, in non-farm, non-reserve private dwellings by shelter-cost-to-income ratio", "Unaffordable housing",
  "Total - Owner households in non-farm, non-reserve private dwellings", "Average monthly shelter cost",
  "Total - Tenant households in non-farm, non-reserve private dwellings", "Average monthly shelter cost"
)

breakdown_vectors_recoding <- tribble(
  ~label, ~ new_label,
  "0 to 4 years", "0 to 4",
  "5 to 9 years", "5 to 9",
  "10 to 14 years", "10 to 14",
  "85 to 89 years", "85 to 89",
  "90 to 94 years", "90 to 94",
  "95 to 99 years", "95 to 99",
  "100 years and over", "100+",
  "5 or more persons", "5+ persons",
  "Total one-parent families", "Single parent families",
  "in which the parent is a woman+", "Single woman+ parent families",
  "In low income based on the Low-income measure, after tax (LIM-AT)", "Low income (LIM-AT)",
  "Spending 30% or more of income on shelter costs", "Spending 30% or more of income on shelter",
  "Average monthly shelter costs for owned dwellings ($)", "Owned dwellings",
  "Average monthly shelter costs for rented dwellings ($)", "Rented dwellings"
)

vectors <- vectors %>%
  left_join(parent_vectors_recoding, by = "label") %>%
  mutate(label = coalesce(new_label, label)) %>%
  select(-new_label) %>%
  left_join(breakdown_vectors_recoding, by = "label") %>%
  mutate(label = coalesce(new_label, label)) %>%
  select(-new_label)

usethis::use_data(vectors, overwrite = TRUE)
