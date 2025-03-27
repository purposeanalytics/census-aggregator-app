#' display_stats_in_modal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_display_stats_in_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
  )
}

#' display_stats_in_modal Server Functions
#'
#' @noRd
mod_display_stats_in_modal_server <- function(id, aggregate_area, selected_geographies, bookmark_query){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$html_output <- renderUI({

      source("R/utils_report_helpers.R")

      ##
      # prep data ----
      regions <- selected_geographies()$geo_uid
      geography <- aggregate_area()

      data <- prepare_data(geography, regions) %>%
        split(.$label) %>%
        purrr::map(~ .x %>%
                     dplyr::select(-label) %>%
                     dplyr::rename(label = breakdown))

      ##
      # calculate values and tables ----
      population <- data[["Population, 2021"]] %>%
        pull_and_format_value("comma")

      households <- data[["Households"]] %>%
        pull_and_format_value("comma")

      if(geography!='ridings'){
        population_change <- data[["Population change, 2016 to 2021"]] %>%
          pull_and_format_value()

        if (!stringr::str_starts(population_change, "-")) {
          population_change <- paste0("+", population_change)
        }
      }

      population_density <- data[["Population density"]] %>%
        pull_and_format_value("comma_decimal")

      knowledge_of_english <- data[["Knowledge of official languages"]] %>%
        dplyr::filter(label == "Knowledge of English") %>%
        pull_and_format_value(column = "value_proportion")

      knowledge_of_french <- data[["Knowledge of official languages"]] %>%
        dplyr::filter(label == "Knowledge of French") %>%
        pull_and_format_value(column = "value_proportion")

      english_at_home <- data[["Official languages most often spoken at home"]] %>%
        dplyr::filter(label == "English") %>%
        pull_and_format_value(column = "value_proportion")

      french_at_home <- data[["Official languages most often spoken at home"]] %>%
        dplyr::filter(label == "French") %>%
        pull_and_format_value(column = "value_proportion")

      top_languages <- data[["Top non-official languages spoken most often at home"]]
      if (is.null(top_languages)) {
        top_languages <- "No data on non-official languages spoken most often at home."
      } else {
        top_languages <- data[["Top non-official languages spoken most often at home"]] %>%
          dplyr::mutate(label = forcats::fct_inorder(label)) %>%
          censusaggregate::inline_barchart()
      }

      median_income_label <- ifelse(length(regions) == 1, "Median household income", "Estimated median household income")

      median_income <- data[[median_income_label]] %>%
        pull_and_format_value("dollar")

      lim_at <- data[["Low-income measure after tax (LIM-AT)"]] %>% pull_and_format_value(column = "value_proportion")

      unaffordable_housing <- data[["Unaffordable housing"]] %>% pull_and_format_value(column = "value_proportion")

      shelter_cost <- data[["Average shelter cost"]]

      # If they are all NA, then fake as proportion so that no bars are shown
      shelter_cost_format <- ifelse(all(is.na(shelter_cost[["value"]])), "proportion", "dollar")

      # Remove 'count' field if still there, fix widths
      shelter_cost <- shelter_cost %>%
        censusaggregate::inline_barchart(shelter_cost_format)

      if ("count" %in% names(shelter_cost[["_data"]])) {
        shelter_cost <- shelter_cost %>%
          gt::cols_hide(count) %>%
          gt::cols_width(label ~ 250, value_fmt ~
                           75, hist ~ 100)
      }

      visible_minority_summary <- data[["Visible minority population"]] %>%
        dplyr::filter(label == "Total visible minority population") %>%
        pull_and_format_value(column = "value_proportion")

      immigrant_status_total <- data[["Immigrant status"]] %>%
        dplyr::filter(label == "Immigrants") %>%
        pull_and_format_value(column = "value_proportion")

      immigrant_status_recent <- data[["Immigrant status"]] %>%
        dplyr::filter(label == "Recent immigrants (2016 to 2021)") %>%
        pull_and_format_value(column = "value_proportion")

      indigenous_identity <- data[["Indigenous identity"]] %>%
        pull_and_format_value(column = "value_proportion")

      ethnic_or_cultural_origin <- data[["Top 10 ethnic or cultural origins"]]

      if (is.null(ethnic_or_cultural_origin)) {
        ethnic_or_cultural_origin <- "No data available on ethnic or cultural origin."
      } else {
        ethnic_or_cultural_origin <- data[["Top 10 ethnic or cultural origins"]] %>%
          dplyr::mutate(label = forcats::fct_inorder(label)) %>%
          censusaggregate::inline_barchart()
      }

      geography_to_layer_id <- function(geography, type) {
        glue::glue("{geography}_{type}")
      }

      geography_to_source_id <- function(geography) {
        glue::glue("2021_{geography}")
      }

      geography_to_source_layer_id <- function(geography) {
        glue::glue("2021_census_{geography}")
      }

      fill_layer_id <- geography_to_layer_id(geography, "fill_report")
      line_layer_id <- geography_to_layer_id(geography, "line_report")

      fill_opacity <- ifelse(geography == "ridings", 0.75/1.5, 0.75)

      filter_list <- append(
        list("in", "geo_uid"),
        as.list(regions)
      )

      boundaries_dataset <- glue::glue("extdata/{geography}")

      boundaries_data <- arrow::open_dataset(system.file(boundaries_dataset, package = "censusaggregatorapp"))
      boundaries_data <- boundaries_data %>%
        sfarrow::read_sf_dataset()

      # Limit to regions, combine boundaries
      region_boundaries <- boundaries_data %>%
        dplyr::filter(geo_uid %in% regions)
      region_boundaries <- region_boundaries %>%
        sf::st_union()

      # Get bounding box
      regions_bbox <- region_boundaries %>%
        sf::st_bbox()

      map_centre <- list(lon = (regions_bbox$xmin + regions_bbox$xmax) / 2, lat = (regions_bbox$ymin + regions_bbox$ymax) / 2)

      zoom_level <- max(1, 8 - log(max(regions_bbox$xmax - regions_bbox$xmin, regions_bbox$ymax -  regions_bbox$ymin)))

      fill_colour <- ifelse(geography == "ridings", "rgba(95, 211, 188, 0.3)", "rgba(95, 211, 188, 0.6)")

      modal_close <- div(
        class = "close-modal-button",
        modalButton(icon = shiny::icon("x"), label = NULL)
      )

      ##
      # Assemble report components ----

      # Create the interactive map
      fig <- plotly::plot_ly() |>
        plotly::add_sf(
          data = boundaries_data %>%
            dplyr::filter(geo_uid %in% regions),
          type = "scattermapbox",
          mode = "lines",
          fillcolor = fill_colour,
          line = list(width = 1, color = "black")
          )|>
        plotly::layout(
          mapbox = list(
            style = "mapbox://styles/purposeanalytics/cl6mafpzk002r14pdbda7la8r",
            center = map_centre,
            zoom = zoom_level
          )) |>
        plotly::config(mapboxAccessToken = Sys.getenv("MAPBOX_API_TOKEN"), displayModeBar = FALSE)

      title <- if (length(regions) == 1 & geography == "ridings") {
        div(title_heading(paste("Federal Electoral District:", ridings |> dplyr::filter(geo_uid == regions) |> dplyr::pull(geo_name))))
      } else if (length(regions) > 1 & geography == "ridings") {
        div(title_heading(paste0("Custom Area: ", scales::number_format(big.mark = ",")(length(regions)), " Federal Electoral Districts")))
      } else if (length(regions) == 1 & geography == "csd") {
        div(title_heading(paste("Census Subdivision:", csd |> dplyr::filter(geo_uid == regions) |> dplyr::pull(region_name))))
      } else if (length(regions) > 1 & geography == "csd") {
        div(title_heading(paste0("Custom Area: ", scales::number_format(big.mark = ",")(length(regions)), " Census Subdivisions")))
      } else if (length(regions) == 1 & geography == "ct") {
        div(title_heading(paste("Census Tract ID:", ct |> dplyr::filter(geo_uid == regions) |> dplyr::pull(geo_uid))))
      } else if (length(regions) > 1 & geography == "ct") {
        div(title_heading(paste0("Custom Area: ", scales::number_format(big.mark = ",")(length(regions)), " Census Tracts")))
      }

      report <- div(
        div(
          class = "row mb-2 boxes",
          column(width = 3, snapshot_card("Population", population)),
          column(width = 3, snapshot_card("Households", households)),
          if (geography != 'ridings') {
            column(width = 3,
                   snapshot_card("Population change<br>(2016 to 2021)", population_change))
          },
          column(
            width = 3,
            snapshot_card(
              "Population density<br>(people/km<sup>2</sup>)",
              population_density
            )
          )
        ),
        div(if (geography != 'ridings') {
          suppressed_note(population,
                          households,
                          population_change,
                          population_density)
        } else {
          suppressed_note(population, households, population_density)
        }
        , section_heading("Age, households, and families")
        ),
        div(
          class = "row mb-2",
          header_and_barchart(
            subsection_heading("Population by age cohorts"),
            data[["Age (cohorts)"]] %>%
              censusaggregate::inline_barchart(),
            subsection_heading("Private households by size"),
            data[["Household size"]] %>%
              censusaggregate::inline_barchart()
          ),
          header_and_barchart(
            subsection_heading("Family type"),
            data[["Family type"]] %>%
              censusaggregate::inline_barchart(),
            subsection_heading("Household type"),
            data[["Household type"]] %>%
              censusaggregate::inline_barchart()
          )
        ),
        div(section_heading("Language and education")),
        div(
          class = "row mb-2 boxes",
          column(
            width = 3,
            snapshot_card("Knowledge of English", knowledge_of_english)
          ),
          column(
            width = 3,
            snapshot_card("Knowledge of French", knowledge_of_french)
          ),
          column(
            width = 3,
            snapshot_card("English spoken most often at home", english_at_home)
          ),
          column(
            width = 3,
            snapshot_card("French spoken most often at home", french_at_home)
          )
        ),
        div(suppressed_note(knowledge_of_english, knowledge_of_french, english_at_home, french_at_home)),
        div(
          class = "row",
          header_and_barchart(
            subsection_heading("Top non-official languages spoken most often at home"),
            top_languages
          ),
          header_and_barchart(
            subsection_heading("Educational attainment"),
            data[["Educational attainment"]] %>%
              dplyr::mutate(label = forcats::fct_inorder(label)) %>%
              censusaggregate::inline_barchart()
          )
        ),
        div(section_heading("Income and housing")),
        div(class = "row mb-2 boxes",
            column(
              width = 4,
              snapshot_card(median_income_label, median_income)
            ),
            column(
              width = 4,
              snapshot_card("Low income (LIM-AT)", lim_at)
            ),
            column(
              width = 4,
              snapshot_card("Unaffordable housing", unaffordable_housing)
            )
        ),
        div(suppressed_note(median_income, lim_at, unaffordable_housing)),
        div(class = "row",
            header_and_barchart(
              subsection_heading("Total household income"),
              data[["Total household income ($20,000 buckets)"]] %>%
                dplyr::mutate(label = forcats::fct_inorder(label)) %>%
                censusaggregate::inline_barchart()
            ),
            header_and_barchart(
              subsection_heading("Average shelter cost"),
              shelter_cost,
              subsection_heading("Household tenure"),
              data[["Tenure"]] %>%
                dplyr::mutate(label = forcats::fct_inorder(label)) %>%
                censusaggregate::inline_barchart()
            )
          ),
        div(section_heading("Diversity and immigration")),
        div(
          class = "row mb-2 boxes",
          column(
            width = 3,
            snapshot_card("Visible minority", visible_minority_summary)
          ),
          column(
            width = 3,
            snapshot_card("Total immigrants", immigrant_status_total)
          ),
          column(
            width = 3,
            snapshot_card("Recent immigrants<br>(2016 to 2021)", immigrant_status_recent)
          ),
          column(
            width = 3,
            snapshot_card(
              "Indigenous identity", indigenous_identity
            )
          )
        ),
        div(suppressed_note(visible_minority_summary, immigrant_status_total, immigrant_status_recent, indigenous_identity)),
        div(
          class = "row",
          header_and_barchart(
            subsection_heading("Visible minority population"),
            data[["Visible minority population"]] %>%
              dplyr::filter(label != "Total visible minority population") %>%
              censusaggregate::derive_census_vector_order(by_value = TRUE) %>%
              censusaggregate::inline_barchart()
          ),
          header_and_barchart(
            subsection_heading("Top ethnic or cultural origins"),
            ethnic_or_cultural_origin
          )
        )
      )

      # assemble definitions
      cutoff <- censusaggregatorapp::variable_definitions %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        dplyr::filter(stringr::str_detect(variable_definition, "High school or equivalent")) %>%
        dplyr::pull(id)

      definitions_table <- function(data) {
        data %>%
          gt::gt() %>%
          gt::cols_align("left", variable_definition) %>%
          gt::fmt_markdown(columns = variable_definition) %>%
          gt::tab_options(
            column_labels.hidden = TRUE,
            table_body.border.bottom.color = "transparent",
            table_body.border.top.color = "transparent",
            table.font.names = "Lato"
          )
      }

      definitions <- div(
        div(section_heading("Definitions")),
        div(class = "row definitions-table definitions-table-html",
          column(
            width = 6,
            censusaggregatorapp::variable_definitions %>%
              dplyr::filter(dplyr::row_number() <= cutoff) %>%
              definitions_table()
          ),
          column(
            width = 6,
            censusaggregatorapp::variable_definitions %>%
              dplyr::filter(dplyr::row_number() > cutoff) %>%
              definitions_table()
          )
        )
      )

      # insert into html template
      htmlTemplate(system.file("report/template.html", package = "censusaggregatorapp"),
                   modal_close = modal_close,
                   title = title,
                   fig = fig,
                   report = report,
                   definitions = definitions)

    })

      showModal(modalDialog(
        size = "xl",
        uiOutput(session$ns("html_output")),
        easyClose = TRUE,
        footer = NULL
      ))


  })
}

## To be copied in the UI
# mod_display_stats_in_modal_ui("display_stats_in_modal_1")

## To be copied in the server
# mod_display_stats_in_modal_server("display_stats_in_modal_1")
