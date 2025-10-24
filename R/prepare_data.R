if(FALSE){
# library(tidyverse)
# library(censusaggregate)
geography <-'ridings'
regions <- c("2023A000411004", "2023A000411001")
dataset <-'extdata/ridings_values/'

geography <-'ct'
regions <-'3100001.01'
dataset <-'inst/extdata/ct_values/'


}
#' Prepare data for export/report
#'
#' @param geography CT or CSD or ridings
#' @param regions Vector of geo_uids
#'
#' @export
prepare_data <- function(geography, regions) {
 #rlog::log_info(paste("Prepare data geo: ", geography))
 #rlog::log_info(paste("Prepare data regions: ", regions))
  # Select and open dataset
  dataset <- glue::glue("extdata/{geography}_values")

  data <- arrow::open_dataset(system.file(dataset, package = "censusaggregatorapp"))

  vectors_data <- data %>%
    # Filter for regions
    dplyr::filter(geo_uid %in% regions) %>%
    dplyr::select(-id) %>%
    dplyr::collect()



  # Expand to have all vectors for all regions
  vectors_data <- vectors_data %>%
    dplyr::mutate(geo_uid = forcats::fct_expand(.data$geo_uid, regions))


  metadata <- switch(geography,
    "csd" = censusaggregatorapp::csd,
    "ct" = censusaggregatorapp::ct,
    "ridings" = censusaggregatorapp::ridings
  ) %>%
    dplyr::filter(geo_uid %in% regions) |>  dplyr::distinct()




  vectors_data <- vectors_data |> dplyr::distinct() |>
    dplyr::left_join(metadata, by = "geo_uid")

  vectors_data <- vectors_data %>%
    dplyr::left_join(censusaggregatorapp::vectors, by = "vector") %>%
    censusaggregate::derive_aggregation_type()



  if(FALSE){
  warning("TEMP TO SEE IF IT WORKS")
  vectors_data <- vectors_data |>  filter(!is.na(aggregation))
### Locate missing aggregation types
missing_vector_info <-  vectors_data |> filter(is.na(aggregation)) |>
      select(vector) |>
      dplyr::distinct()
  }

  # Aggregate vectors - treat land area separately (used for population density), population 2016 separately (user for population change, median total income separately (used for median income if only 1 region selected)
  vectors_data_filtered <- vectors_data %>%
    dplyr::filter(!(.data$label %in% c("Population, 2016", "Land area in square kilometres", "Median total household income", "Population density per square kilometre")))

  # Also exclude income vectors, aggregate separately
  data_breakdown <- vectors_data_filtered %>%
    censusaggregate::aggregate_census_vectors() %>%
    dplyr::distinct() %>%
    dplyr::left_join(censusaggregatorapp::vectors, by = c("highest_parent_vector", "vector", "type", "label", "units", "parent_vector", "aggregation", "details")) %>%
    dplyr::select(highest_parent_vector, vector, label, label_short, value, value_proportion) %>%
    dplyr::group_by(highest_parent_vector) %>%
    tidyr::fill(label_short, .direction = "updown") %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  data <- dplyr::bind_rows(
    # Population, 2021
    data_breakdown %>%
      filter_breakdown("population_2021", "Population, 2021", exclude_parent = FALSE, proportion = FALSE) %>%
      dplyr::mutate(label = NA),
    # Households
    metadata %>%
      dplyr::select(geo_uid, households) %>%
      dplyr::distinct() %>%
      dplyr::summarise(value = sum(households)) %>%
      dplyr::mutate(
        parent_label = "Households"
      ),
    # Population change (2016 to 2021)
    if(geography!='ridings'){
    vectors_data %>%
      censusaggregate::aggregate_population_change() %>%
      dplyr::select(label, value) %>%
      dplyr::mutate(parent_label = "Population change, 2016 to 2021", label = NA, value = round(value, 3))} else {NULL},
    # Population density
    # Use actual population density vector if there is only one
    {
      if (length(regions) == 1) {
        population_density <- vectors_data %>%
          dplyr::filter(label == "Population density per square kilometre") %>%
          dplyr::select(value)
      } else {
        population_density <- vectors_data %>%
          dplyr::filter(label %in% c("Population, 2021", "Land area in square kilometres")) %>%
          censusaggregate::aggregate_population_density() %>%
          dplyr::select(value)
      }
      population_density %>%
        dplyr::mutate(parent_label = "Population density", value = round(value, 1))
    },
    # Age (5 year buckets)
    data_breakdown %>%
      dplyr::filter(label_short == "age", vector != highest_parent_vector) %>%
      tidyr::separate(label,
        into = c("min", "max"),
        sep = " to ", remove = FALSE, convert = TRUE, fill = "right"
      ) %>%
      dplyr::arrange(max) %>%
      dplyr::select(-min, -max) %>%
      dplyr::select(label, value, value_proportion) %>%
      dplyr::mutate(parent_label = "Age (5 year groups)"),
    # Age (cohorts)
    data_breakdown %>%
      filter_breakdown("age_cohorts", "Age (cohorts)"),
    # Household size
    data_breakdown %>%
      filter_breakdown("household_size", "Household size"),
    # Family type
    data_breakdown %>%
      filter_breakdown("family_type", "Family type"),
    # Household type
    data_breakdown %>%
      filter_breakdown("household_type", "Household type") %>%
      dplyr::mutate(label = forcats::fct_relevel(
        label, "1 person",
        "2+ persons non census family",
        "1 census family, no additional persons",
        "1 census family, + additional persons",
        "Multiple census families",
        "Multigen. households"
      )) %>%
      dplyr::arrange(label),
    # Knowledge of English
    data_breakdown %>%
      filter_breakdown("knowledge_of_english_french", "Knowledge of official languages") %>%
      additional_filter_and_combine(c("English only", "English and French"), "Knowledge of English"),
    # Knowledge of French
    data_breakdown %>%
      filter_breakdown("knowledge_of_english_french", "Knowledge of official languages") %>%
      additional_filter_and_combine(c("French only", "English and French"), "Knowledge of French"),
    # English spoken at home
    data_breakdown %>%
      filter_breakdown("language_at_home", "Official languages most often spoken at home") %>%
      additional_filter_and_combine(
        c(
          "English", "English and French", "English and non-official language(s)", "English, French and non-official language(s)"
        ),
        "English"
      ),
    # French spoken at home
    data_breakdown %>%
      filter_breakdown("language_at_home", "Official languages most often spoken at home") %>%
      additional_filter_and_combine(
        c(
          "French", "English and French", "French and non-official language(s)", "English, French and non-official language(s)"
        ),
        "French"
      ),
    # Non-official language spoken at home
    data_breakdown %>%
      dplyr::filter(
        label_short == "language_at_home",
        vector != highest_parent_vector
      ) %>%
      dplyr::inner_join(censusaggregatorapp::vectors %>%
        dplyr::filter(label_short == "language_at_home", stringr::str_detect(details, "Single")) %>%
        dplyr::select(vector), by = "vector") %>%
      dplyr::filter(!label %in% c("English", "French")) %>%
      dplyr::filter(value > 0) %>%
      dplyr::arrange(-value) %>%
      utils::head(10) %>%
      censusaggregate::derive_census_vector_order(by_value = TRUE) %>%
      dplyr::select(label, value, value_proportion) %>%
      dplyr::mutate(parent_label = "Top non-official languages spoken most often at home"),
    # Educational attainment
    data_breakdown %>%
      filter_breakdown("educational_attainment", "Educational attainment") %>%
      dplyr::mutate(label = forcats::fct_relevel(
        label, "No high school or postsecondary",
        "High school or equivalent",
        "Apprenticeship or trades certificate or diploma",
        "College, CEGEP or other non-university certificate or diploma",
        "University certificate or diploma below bachelor level",
        "Bachelor's degree or certificate/diploma above bachelor level",
        "Graduate or professional degree"
      )) %>%
      dplyr::arrange(label),

    # (Estimated) median household income
    {
      if (length(regions) == 1) {
        median_income <- vectors_data %>%
          dplyr::filter(vector == "v_CA21_906") %>%
          dplyr::select(value)
        median_income_label <- "Median household income"
      } else {
        median_income <- data_breakdown %>%
          dplyr::filter(label_short == "income") %>%
          censusaggregate::aggregate_estimated_median_income()
        median_income_label <- "Estimated median household income"
      }

      median_income %>%
        dplyr::mutate(parent_label = median_income_label)
    },
    # Buckets
    data_breakdown %>%
      filter_breakdown("income_buckets", "Total household income ($20,000 buckets)") %>%
      dplyr::mutate(label = forcats::fct_relevel(
        label, "Under $20,000", "$20,000 to $40,000", "$40,000 to $60,000",
        "$60,000 to $80,000", "$80,000 to $100,000", "$100,000 and over"
      )) %>%
      dplyr::arrange(label),
    # Original breakdowns
    data_breakdown %>%
      filter_breakdown("income", "Total household income (original buckets)"),
    # Low-income measure after tax (LIM-AT)
    data_breakdown %>%
      filter_breakdown("lim_at", "Low-income measure after tax (LIM-AT)"),
    # Unaffordable housing
    data_breakdown %>%
      filter_breakdown("unaffordable_housing", "Unaffordable housing"),
    # Average shelter cost
    data_breakdown %>%
      filter_breakdown(c("shelter_cost_renter", "shelter_cost_owner"), "Average shelter cost") %>%
      dplyr::mutate(value = round(value, digits = 0)),
    # Household tenure
    data_breakdown %>%
      filter_breakdown("household_tenure", "Tenure"),
    # Visible minority population
    data_breakdown %>%
      filter_breakdown("visible_minority", "Visible minority population"),
    # Immigrant Status, Recent Immigrants
    data_breakdown %>%
      filter_breakdown("immigrant_status", "Immigrant status"),
    # Indigenous identity
    data_breakdown %>%
      filter_breakdown("indigenous_identity", "Indigenous identity"),
    # Ethnic or cultural origin
    data_breakdown %>%
      filter_breakdown("ethnic_cultural_origin") %>%
      dplyr::filter(value > 0) %>%
      dplyr::arrange(-value) %>%
      utils::head(10) %>%
      censusaggregate::derive_census_vector_order(by_value = TRUE) %>%
      dplyr::select(label, value, value_proportion) %>%
      dplyr::mutate(parent_label = "Top 10 ethnic or cultural origins")
  )

  data %>%
    dplyr::rename(breakdown = .data$label) %>%
    dplyr::rename(label = .data$parent_label) %>%
    dplyr::select(label, breakdown, value, value_proportion)
}

filter_breakdown <- function(data, label_short, parent_label = NA, exclude_parent = TRUE, proportion = TRUE) {
  if (exclude_parent) {
    data <- data %>%
      dplyr::filter(.data$vector != .data$highest_parent_vector)
  }

  data <- data %>%
    dplyr::filter(.data$label_short %in% !!label_short) %>%
    dplyr::select(.data$label, .data$value, .data$value_proportion)

  if (!proportion) {
    data <- data %>%
      dplyr::select(-.data$value_proportion)
  }

  data %>%
    dplyr::mutate(parent_label = !!parent_label)
}

additional_filter_and_combine <- function(data, label, new_label) {
  data %>%
    dplyr::group_by(.data$parent_label) %>%
    dplyr::filter(.data$label %in% !!label) %>%
    dplyr::summarise(dplyr::across(c(.data$value, .data$value_proportion), sum)) %>%
    dplyr::mutate(label = new_label)
}
