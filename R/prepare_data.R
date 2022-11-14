#' Prepare data for export/report
#'
#' @param geography CT or CSD
#' @param regions Vector of geo_uids
#'
#' @export
prepare_data <- function(geography, regions) {

  # Select and open dataset
  dataset <- glue::glue("extdata/{geography}_values")

  data <- arrow::open_dataset(system.file(dataset, package = "censusaggregatorapp"))

  vectors_data <- data %>%
    # Filter for regions
    dplyr::filter(.data$geo_uid %in% regions) %>%
    dplyr::select(-.data$id) %>%
    dplyr::collect()

  # Expand to have all vectors for all regions
  vectors_data <- vectors_data %>%
    dplyr::mutate(geo_uid = forcats::fct_expand(.data$geo_uid, regions))

  metadata <- switch(geography,
    "csd" = censusaggregatorapp::csd,
    "ct" = censusaggregatorapp::ct
  ) %>%
    dplyr::filter(.data$geo_uid %in% regions)

  vectors_data <- vectors_data %>%
    dplyr::left_join(metadata, by = "geo_uid")

  vectors_data <- vectors_data %>%
    dplyr::left_join(censusaggregatorapp::vectors, by = "vector") %>%
    censusaggregate::derive_aggregation_type()

  # Aggregate vectors - treat land area separately (used for population density), population 2016 separately (user for population change, median total income separately (used for median income if only 1 region selected)
  vectors_data_filtered <- vectors_data %>%
    dplyr::filter(!(.data$label %in% c("Population, 2016", "Land area in square kilometres", "Median total household income")))

  # Also exclude income vectors, aggregate separately
  data_breakdown <- vectors_data_filtered %>%
    censusaggregate::aggregate_census_vectors() %>%
    dplyr::distinct() %>%
    dplyr::left_join(censusaggregatorapp::vectors, by = c("highest_parent_vector", "vector", "type", "label", "units", "parent_vector", "aggregation", "details")) %>%
    dplyr::select(.data$highest_parent_vector, .data$vector, .data$label, .data$label_short, .data$value, .data$value_proportion) %>%
    dplyr::group_by(.data$highest_parent_vector) %>%
    tidyr::fill(.data$label_short, .direction = "updown") %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  browser()

  data <- dplyr::bind_rows(
    # Population, 2021
    data_breakdown %>%
      filter_breakdown("population_2021", "Population, 2021", exclude_parent = FALSE, proportion = FALSE) %>%
      dplyr::mutate(label = NA),
    # Households
    metadata %>%
      dplyr::select(.data$geo_uid, .data$households) %>%
      dplyr::distinct() %>%
      dplyr::summarise(value = sum(.data$households)) %>%
      dplyr::mutate(
        parent_label = "Households"
      ),
    # Population change (2016 to 2021)
    vectors_data %>%
      censusaggregate::aggregate_population_change() %>%
      dplyr::select(.data$label, .data$value) %>%
      dplyr::mutate(parent_label = "Population change, 2016 to 2021") %>%
      dplyr::mutate(label = NA),
    # Population density
    vectors_data %>%
      dplyr::filter(.data$label %in% c("Population, 2021", "Land area in square kilometres")) %>%
      censusaggregate::aggregate_population_density() %>%
      dplyr::select(.data$value) %>%
      dplyr::mutate(parent_label = "Population density"),
    # Age (5 year buckets)
    data_breakdown %>%
      dplyr::filter(.data$label_short == "age") %>%
      tidyr::separate(.data$label,
        into = c("min", "max"),
        sep = " to ", remove = FALSE, convert = TRUE, fill = "right"
      ) %>%
      dplyr::arrange(.data$max) %>%
      dplyr::select(-.data$min, -.data$max) %>%
      dplyr::select(.data$label, .data$value, .data$value_proportion) %>%
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
        .data$label, "1 person",
        "2+ persons non census family",
        "1 census family, no additional persons",
        "1 census family, + additional persons",
        "Multiple census families",
        "Multigen. households"
      )) %>%
      dplyr::arrange(.data$label),
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
      filter_breakdown("language_at_home", "Official languages primarily spoken at home") %>%
      additional_filter_and_combine(
        c(
          "English", "English and French", "English and non-official language(s)", "English, French and non-official language(s)"
        ),
        "English spoken at home"
      ),
    # French spoken at home
    data_breakdown %>%
      filter_breakdown("language_at_home", "Official languages primarily spoken at home") %>%
      additional_filter_and_combine(
        c(
          "French", "English and French", "French and non-official language(s)", "English, French and non-official language(s)"
        ),
        "French spoken at home"
      ),
    # Non-official language spoken at home
    data_breakdown %>%
      dplyr::filter(
        .data$label_short == "language_at_home",
        .data$vector != .data$highest_parent_vector
      ) %>%
      dplyr::inner_join(censusaggregatorapp::vectors %>%
        dplyr::filter(.data$label_short == "language_at_home", stringr::str_detect(.data$details, "Single")) %>%
        dplyr::select(.data$vector), by = "vector") %>%
      dplyr::filter(!.data$label %in% c("English", "French")) %>%
      dplyr::top_n(10, wt = value) %>%
      dplyr::filter(.data$value > 0) %>%
      dplyr::arrange(-.data$value) %>%
      head(10) %>%
      censusaggregate::derive_census_vector_order(by_value = TRUE) %>%
      dplyr::select(.data$label, .data$value, .data$value_proportion) %>%
      dplyr::mutate(parent_label = "Top 10 non-official languages primarily spoken at home"),
    # (Estimated) median household income
    # Buckets
    # Original breakdowns
    # Low-income measure after tax (LIM-AT)
    data_breakdown %>%
      filter_breakdown("lim_at", "Low-income measure after tax (LIM-AT)"),
    # Unaffordable housing
    data_breakdown %>%
      filter_breakdown("unaffordable_housing", "Unaffordable housing"),
    # Average shelter cost
    data_breakdown %>%
      filter_breakdown(c("shelter_cost_renter", "shelter_cost_owner"), "Average shelter cost"),
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
    # Educational attainment - TODO
  )

  data %>%
    dplyr::rename(breakdown = .data$label) %>%
    dplyr::rename(label = .data$parent_label) %>%
    dplyr::select(.data$label, .data$breakdown, .data$value, .data$value_proportion)
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