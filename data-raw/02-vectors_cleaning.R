# Collapse, recode, etc vectors

library(tidyverse)
library(censusaggregate)

vectors <- readRDS(here::here("data-raw", "intermediary", "vectors.rds"))
age_cohort_vectors <- readRDS(here::here("data-raw", "intermediary", "age_cohort_vectors.rds"))

# Collapse age cohort, "with children", total income vectors ----

age_cohort_vectors <- age_cohort_vectors %>%
  select(-label) %>%
  rename(new_vector = group)

with_children_vectors <- vectors %>%
  filter(label == "With children") %>%
  select(vector) %>%
  mutate(new_vector = "Couples with children")

total_income_vectors <- tibble(
  label = c(
    "Under $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999",
    "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
    "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
    "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999",
    "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $124,999",
    "$125,000 to $149,999", "$150,000 to $199,999", "$200,000 and over"
  )
) %>%
  mutate(new_vector = case_when(
    label %in% c("Under $5,000", "$5,000 to $9,999", "$10,000 to $14,999", "$15,000 to $19,999") ~ "Under $20,000",
    label %in% c(
      "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
      "$35,000 to $39,999"
    ) ~ "$20,000 to $40,000",
    label %in% c(
      "$40,000 to $44,999", "$45,000 to $49,999",
      "$50,000 to $59,999"
    ) ~ "$40,000 to $60,000",
    label %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $80,000",
    label %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $100,000",
    label %in% c(
      "$100,000 to $124,999",
      "$125,000 to $149,999", "$150,000 to $199,999", "$200,000 and over"
    ) ~ "$100,000 and over"
  )) %>%
  inner_join(vectors %>%
    select(label, vector), by = "label") %>%
  select(-label)

collapse_vectors <- bind_rows(
  age_cohort_vectors,
  with_children_vectors,
  total_income_vectors
)

vectors <- vectors %>%
  mutate(label = str_replace_all(label, "\\$", "\\\\$")) %>%
  collapse_census_vectors(collapse_vectors) %>%
  distinct()

vectors <- vectors %>%
  mutate(label_short = case_when(
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
  "Total - Tenant households in non-farm, non-reserve private dwellings", "Average monthly shelter cost",
  "Median total income of household in 2020 ($)", "Median total household income"
)

breakdown_vectors_recoding <- tribble(
  ~label, ~new_label,
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
