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
  bslib::card(
    shinybusy::add_busy_spinner("circle", color = "white", height = "30px", width = "30px"),
    shiny::div(
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
        shinyjs::disabled(
          shiny::actionButton(
            ns("bookmark_selections"),
            "Bookmark selections",
            class = "btn-link"
          )
        )
      ),
      breathe(),
      shiny::div(
        shinyjs::disabled(
          shiny::downloadButton(
            ns("export_boundary"),
            "Export boundary",
            class = "btn-link"
          )
        )
      ),
      breathe(),
      shiny::div(
        shiny::div("Summary of selected areas", class = "summary-statistics-header breathe"),
        shiny::htmlOutput(ns("summary_statistics"))
      ),
      breathe(),
      shiny::div(
        shinyjs::disabled(
          shiny::downloadButton(
            ns("export_data"),
            "Export data",
            width = "100%"
          )
        )
      ),
    )
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
      bookmark_query <- construct_bookmark(input, session, exclude = c("selection_tool", "export_data", "bookmark_selections", "export_geography", "export_boundary_bttn"), selected_geographies())
      shiny:::showBookmarkUrlModal(bookmark_query)
    })

    # Observe any bookmarking to update inputs with ----
    shiny::observe(
      # Priority of 1 - happens AFTER tibble of geo_uid is reset, to ensure any bookmarked geo_uids are kept
      priority = 1,
      {
        shiny::req(boomarks_to_be_parsed())
        shiny::req(map_rendered())

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
          {
            purrr::walk(query_inputs, function(x) {
              shinyWidgets::updatePickerInput(session, inputId = x, selected = query[[x]])
            })

            # Update selected_geographies() to have geo_uid
            if (!is.null(query$geo_uid)) {
              selected_geographies(
                dplyr::tibble(geo_uid = query$geo_uid)
              )
            }
          }
        )



        boomarks_to_be_parsed(FALSE)
      }
    )


    # Update inputs with aggregate_area and selection_tool -----
    inputs <- shiny::reactive({
      list(
        aggregate_area = input$aggregate_area,
        selection_tool = input$selection_tool
      )
    })

    # Export boundary ----

    output$export_boundary <- shiny::downloadHandler(
      filename = function() {
        "boundary.geojson"
      },
      content = function(con) {
        dataset <- arrow::open_dataset(glue::glue("inst/extdata/{input$aggregate_area}"))

        query <- dplyr::filter(dataset, .data$geo_uid %in% selected_geographies()[["geo_uid"]])

        sfarrow::read_sf_dataset(query) %>%
          sf::st_make_valid() %>%
          sf::st_union() %>%
          sf::st_write(con)
      }
    )

    # Summary statistics table ----
    shiny::observeEvent(selected_geographies(), ignoreInit = FALSE, {
      if (nrow(selected_geographies()) == 0) {

        # Disable buttons
        shinyjs::disable("bookmark_selections")
        shinyjs::disable("export_boundary")
        shinyjs::disable("export_data")

        summary_statistics <- dplyr::tibble(name = c(
          "Areas selected",
          "Population",
          "Households",
          "Area",
          "Population density"
        )) %>%
          dplyr::mutate(value = "\u2014")
      } else {
        # Enable buttons
        shinyjs::enable("bookmark_selections")
        shinyjs::enable("export_boundary")
        shinyjs::enable("export_data")

        summary_statistics_source <- switch(input$aggregate_area,
          "csd" = censusaggregatorapp::csd,
          "ct" = censusaggregatorapp::ct
        )

        summary_statistics <- summary_statistics_source %>%
          dplyr::inner_join(selected_geographies(), by = "geo_uid") %>%
          dplyr::select(.data$population, .data$households, .data$area_sq_km) %>%
          dplyr::mutate(n = dplyr::n()) %>%
          dplyr::group_by(.data$n) %>%
          dplyr::summarise_all(sum) %>%
          dplyr::mutate(population_density = round(.data$population / .data$area_sq_km)) %>%
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
          dplyr::select(.data$label, .data$value)
      }

      output$summary_statistics <- shiny::renderText({
        summary_statistics %>%
          knitr::kable("html", col.names = NULL, escape = FALSE, align = "lr") %>%
          kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = "none") %>%
          kableExtra::column_spec(column = 1, bold = TRUE)
      })
    })

    # Export data ----
    output$export_data <- shiny::downloadHandler(
      filename = function() {
        "report.html"
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(here::here("scratch", "report.Rmd"), tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          geo_uid = selected_geographies()$geo_uid,
          level = input$aggregate_area
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )

    return(inputs)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
