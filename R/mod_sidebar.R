#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  shiny::div(
    style = "width: 30%; max-width: 500px;",
    shiny::div(style = "margin: 100px;"),
    shinyWidgets::prettyRadioButtons(
      ns("aggregate_area"),
      "Choose aggregate area",
      choices = list(
        "Census Subdivisions" = "csd",
        "Census Tracts" = "ct"
      )
    ),
    shinyWidgets::prettyRadioButtons(
      ns("selection_tool"),
      "Choose selection tool",
      choices = list(
        "Click to select geographies" = "click",
        "Draw a polygon" = "polygon"
      )
    ),
    shiny::div(
      shinyWidgets::actionBttn(
        ns("bookmark_selections"),
        "Bookmark selections",
        style = "bordered",
        color = "primary"
      )
    ),
    breathe(),
    shiny::textOutput(ns("bookmark")),
    shiny::div(
      shinyWidgets::downloadBttn(
        ns("export_boundary"),
        "Export boundary",
        style = "bordered",
        color = "primary"
      )
    ),
    breathe(),
    shiny::div(
      shiny::div("Summary of selected areas", class = "summary-statistics-header breathe"),
      shiny::htmlOutput(ns("summary_statistics"))
    ),
    breathe(),
    shiny::div(
      shinyWidgets::actionBttn(
        ns("export_data"),
        "Export data",
        style = "bordered",
        color = "primary"
      )
    ),
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, selected_geographies, map_rendered, boomarks_to_be_parsed) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set up bookmarking ----
    shiny::observeEvent(input$bookmark_selections, {
      bookmark_query <- construct_bookmark(input, session, exclude = c("export_data", "bookmark_selections", "export_geography"), selected_geographies())
      output$bookmark <- renderText(bookmark_query)
    })

    # Observe any bookmarking to update inputs with ----
    shiny::observe(
      # Priority of 1 - happens AFTER tibble of geo_uid is reset, to ensure any bookmarked geo_uids are kept
      priority = 1,
      {
        req(boomarks_to_be_parsed())
        req(map_rendered())

        query <- shiny::parseQueryString(session$clientData$url_search)
        # Additional parsing of query to split by ,
        query <- split_query(query)

        # Only update inputs that are also in the query string
        query_inputs <- intersect(names(input), names(query))

        # Iterate over them and update
        shinyjs::delay(
          # Add an additional delay, to allow for map to be rendered
          # TODO not great
          ms = 1000,
          purrr::walk(query_inputs, function(x) {
            shinyWidgets::updatePickerInput(session, inputId = x, selected = query[[x]])
          })
        )

        # Update selected_geographies() to have geo_uid
        if (!is.null(query$geo_uid)) {
          selected_geographies(
            dplyr::tibble(geo_uid = query$geo_uid)
          )
        }

        boomarks_to_be_parsed(FALSE)
      }
    )


    # Update inputs with aggregate_area -----
    inputs <- shiny::reactive({
      list(
        aggregate_area = input$aggregate_area
      )
    })

    # Export boundary ----
    output$export_boundary <- shiny::downloadHandler(
        filename = function() {
          "boundary.geojson"
        },
        content = function(con) {
          dataset <- arrow::open_dataset(glue::glue("inst/data/{input$aggregate_area}"))

          query <- dplyr::filter(dataset, .data$geo_uid %in% selected_geographies()[["geo_uid"]])

          sfarrow::read_sf_dataset(query) %>%
            sf::st_union() %>%
            sf::st_write(con)
        }
      )
    shiny::observeEvent(input$export_boundary, {
    })

    # Summary statistics table ----
    shiny::observeEvent(selected_geographies(), ignoreInit = FALSE, {
      if (nrow(selected_geographies()) == 0) {
        summary_statistics <- dplyr::tibble(name = c(
          "Areas selected",
          "Population",
          "Households",
          "Area",
          "Population density"
        )) %>%
          dplyr::mutate(value = "-")
      } else {
        summary_statistics_source <- switch(input$aggregate_area,
          "csd" = censusaggregatorapp::csd,
          "ct" = censusaggregatorapp::ct
        )

        summary_statistics <- summary_statistics_source %>%
          dplyr::inner_join(selected_geographies(), by = "geo_uid") %>%
          dplyr::select(population, households, area_sq_km) %>%
          dplyr::mutate(n = dplyr::n()) %>%
          dplyr::group_by(n) %>%
          dplyr::summarise_all(sum) %>%
          dplyr::mutate(population_density = round(population / area_sq_km)) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_longer(dplyr::everything()) %>%
          dplyr::mutate(value = dplyr::case_when(
            name %in% c("population", "households") ~ scales::comma(value, accuracy = 1),
            name %in% c("area_sq_km", "population_density") ~ scales::comma(value, accuracy = 0.1),
            name == "n" ~ as.character(value)
          ))

        n_units <- switch(inputs()[["aggregate_area"]],
          csd = "Census Subdivision",
          ct = "Census Tract"
        )

        n_units <- ifelse(nrow(selected_geographies()) > 1,
          paste0(n_units, "s"),
          n_units
        )

        summary_statistics_labels_and_units <- dplyr::tribble(
          ~name, ~label, ~units,
          "n", "Areas selected", n_units,
          "population", "Population", NA_character_,
          "households", "Households", NA_character_,
          "area_sq_km", "Area", "km^2",
          "population_density", "Population density", "people / km^2"
        )

        summary_statistics <- summary_statistics %>%
          dplyr::left_join(summary_statistics_labels_and_units, by = "name") %>%
          dplyr::mutate(value = glue::glue("{value} {units}", .na = "")) %>%
          dplyr::select(label, value)
      }

      output$summary_statistics <- shiny::renderText({
        summary_statistics %>%
          knitr::kable("html", col.names = NULL, escape = FALSE, align = "lr") %>%
          kableExtra::column_spec(column = 1, bold = TRUE)
      })
    })

    return(inputs)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
