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
    style = "height: 100vh", # TODO make overflow scroll within div
    shinybusy::add_busy_spinner("circle", color = "white", height = "30px", width = "30px"),
    shiny::div(
      shiny::img(src = "www/logo.png", alt = "CensusAggregator logo", style = "width: 50%"),
      breathe(),
      shiny::p("CensusAggregator makes it easier to retrieve and aggregate common census variables for regions that cover multiple census geographic areas. Follow the steps below to create a custom area on the map and download a summary report, data file, and boundary file for that area. CensusAggregator uses data from the 2021 Canadian census."),
      sidebar_header(
        "Step 1: Choose a geographic unit",
        tooltip("Census subdivision (CSD) is the general term for municipalities (as determined by provincial/territorial legislation) or areas treated as municipal equivalents for statistical purposes (e.g., Indian reserves, Indian settlements and unorganized territories).<br><br>Census tracts (CTs) are small, relatively stable geographic areas that usually have a population of fewer than 7,500 persons, based on data from the previous Census of Population Program. They are located in census metropolitan areas (CMAs) and in census agglomerations (CAs) that had a core population of 50,000 or more in the previous census.")
      ),
      shinyWidgets::prettyRadioButtons(
        ns("aggregate_area"),
        NULL,
        choices = list(
          "Census tract" = "ct",
          "Census subdivision" = "csd"
        ),
        inline = TRUE
      ),
      sidebar_header(
        "Step 2: Choose an area selection method",
        tooltip(shiny::HTML("Use the “Click to select/deselect” option to select one geographic area at a time. Each selected geographic area will be highlighted with a bold outline. This option also permits the selection of non-contiguous areas.<br><br>Use the “Draw a polygon” option to draw a continuous boundary. Each mouse click marks a new point in the boundary. To complete the polygon selection, use a double mouse click for the final point or click on the first point to close the loop. The census geographic areas that overlap with polygon will be selected and highlighted with a bold outline."))
      ),
      shinyWidgets::prettyRadioButtons(
        ns("selection_tool"),
        NULL,
        choices = list(
          "Click to select/deselect" = "click",
          "Draw a polygon" = "polygon"
        ),
        inline = TRUE
      ),
      shinyjs::disabled(
        shiny::actionButton(
          ns("reset"),
          "Clear selection",
          class = "btn-link btn-secondary-effect"
        )
      ),
      sidebar_header("Step 3: Download data"),
      shiny::div(
        shinyjs::disabled(
          shiny::downloadButton(
            ns("export_data"),
            "Export data",
            width = "100%",
            icon = NULL
          )
        )
      ),
      shiny::div(
        shinyjs::disabled(
          shiny::downloadButton(
            ns("export_boundary"),
            "Export boundary",
            class = "btn-link",
            icon = NULL
          )
        )
      ),
      breathe(),
      shiny::div(
        sidebar_header("Summary of selected areas"),
        gt::gt_output(ns("summary_statistics"))
      ),
      shiny::div(
        shinyjs::disabled(
          shiny::actionButton(
            ns("bookmark_selections"),
            "Bookmark selections",
            class = "btn-link"
          )
        )
      )
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, boomarks_to_be_parsed, bookmark_bounds) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Set up bookmarking ----
    shiny::observeEvent(input$bookmark_selections, {
      bookmark_query <- construct_bookmark(input, session, exclude = c("selection_tool", "export_data", "bookmark_selections", "export_geography", "export_boundary_bttn", "reset"), selected_geographies())
      shiny:::showBookmarkUrlModal(bookmark_query)
    })

    # Observe any bookmarking to update inputs with ----
    bookmark_aggregate_area <- shiny::reactiveVal()
    bookmark_geo_uid <- shiny::reactiveVal()

    # Parse bookmark
    shiny::observe({
      query <- shiny::parseQueryString(session$clientData$url_search)
      # Additional parsing of query to split by ,
      query <- split_query(query)

      bookmark_aggregate_area(query$aggregate_area)
      if ("aggregate_area" %in% names(query)) {
        input_aggregate_area(query$aggregate_area)
      }
      bookmark_geo_uid(query$geo_uid)
    })

    shiny::observe(
      # Priority of 1 - happens AFTER tibble of geo_uid is reset, to ensure any bookmarked geo_uids are kept
      priority = 1,
      {
        shiny::req(boomarks_to_be_parsed())
        shiny::req(map_rendered())
        shiny::req(bookmark_aggregate_area())
        shiny::req(bookmark_geo_uid())

        shinyjs::delay(
          # Add an additional delay, to allow for map to be rendered
          # TODO not great
          ms = 1000,
          {

            # Update aggregate area
            shinyWidgets::updatePickerInput(session, inputId = "aggregate_area", selected = bookmark_aggregate_area())

            # Update selected_geographies() to have geo_uid
            if (!is.null(bookmark_geo_uid())) {
              selected_geographies(
                dplyr::tibble(geo_uid = bookmark_geo_uid())
              )

              # Update input aggregate area

              input_aggregate_area(bookmark_aggregate_area())

              # Get bounds of selected area to fly map to
              dataset <- arrow::open_dataset(app_sys(glue::glue("extdata/{bookmark_aggregate_area()}")))

              query <- dplyr::filter(dataset, .data$geo_uid %in% selected_geographies()[["geo_uid"]])
              bookmark_bounds(
                sfarrow::read_sf_dataset(query) %>%
                  sf::st_bbox()
              )
            }
          }
        )

        boomarks_to_be_parsed(FALSE)
      }
    )

    # Update inputs with aggregate_area and selection_tool -----
    shiny::observeEvent(
      input$aggregate_area,
      ignoreInit = FALSE,
      priority = 300, # Set lower priority so aggregate_area input is set to whatever is in bookmark first
      {
        input_aggregate_area(input$aggregate_area)

        # Change which legend is shown
        if (input_aggregate_area() == "csd") {
          shinyjs::show("csd-population-density")
          shinyjs::hide("ct-population-density")
        } else {
          shinyjs::hide("csd-population-density")
          shinyjs::show("ct-population-density")
        }
      }
    )

    shiny::observeEvent(
      input$selection_tool,
      ignoreInit = FALSE,
      {
        # Clear selected geographies
        selected_geographies(dplyr::tibble())

        input_selection_tool(input$selection_tool)
      }
    )

    # Reset geography
    shiny::observeEvent(
      input$reset,
      {
        selected_geographies(dplyr::tibble())

        # Send to JS to reset polygon
        session$sendCustomMessage("reset", TRUE)
      }
    )

    # Export boundary ----

    output$export_boundary <- shiny::downloadHandler(
      filename = function() {
        "boundary.geojson"
      },
      content = function(con) {
        dataset <- arrow::open_dataset(app_sys(glue::glue("extdata/{input$aggregate_area}")))

        query <- dplyr::filter(dataset, .data$geo_uid %in% selected_geographies()[["geo_uid"]])

        sfarrow::read_sf_dataset(query) %>%
          sf::st_union() %>%
          sf::st_write(con)
      }
    )

    # Summary statistics table ----
    shiny::observeEvent(selected_geographies(), ignoreInit = FALSE, priority = 30, {
      shiny::req(input_aggregate_area())

      if (nrow(selected_geographies()) == 0) {

        # Disable buttons
        shinyjs::disable("reset")
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
          dplyr::mutate(value = NA)
      } else {
        # Enable buttons
        shinyjs::enable("reset")
        shinyjs::enable("bookmark_selections")
        shinyjs::enable("export_boundary")
        shinyjs::enable("export_data")

        summary_statistics_source <- switch(input_aggregate_area(),
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

        n_units <- switch(input_aggregate_area(),
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
          "area_sq_km", "Area", "km<sup>2</sup>",
          "population_density", "Population density", "people/km<sup>2</sup>"
        )

        summary_statistics <- summary_statistics %>%
          dplyr::left_join(summary_statistics_labels_and_units, by = "name") %>%
          dplyr::mutate(
            value = glue::glue("{value} {units}", .na = "")
          ) %>%
          dplyr::select(.data$label, .data$value)
      }

      output$summary_statistics <- gt::render_gt({
        summary_statistics %>%
          gt::gt() %>%
          gt::sub_missing(columns = value) %>%
          gt::cols_align("right", value) %>%
          gt::fmt_markdown(columns = value) %>%
          gt::tab_options(
            table.width = "100%",
            column_labels.hidden = TRUE,
            table_body.border.bottom.color = "transparent",
            table_body.border.top.color = "transparent",
            table.font.names = "Lato"
          )
      })
    })

    # Export data ----
    output$export_data <- shiny::downloadHandler(
      filename = function() {
        "CensusAggregator Export.zip"
      },
      content = function(file) {
        cat("export start \n")
        cat(paste0(Sys.time()), "\n")

        # Move to tempdir to save files
        original_wd <- setwd(tempdir())

        # Go back to working directory after function
        on.exit(setwd(original_wd))

        temp_template <- "report.Rmd"
        file.copy(app_sys("report/report.Rmd"), temp_template, overwrite = TRUE)

        report_html <- "CensusAggregator Report.html"
        report_pdf <- "CensusAggregator Report.pdf"
        data_export <- "CensusAggregator Data Export.csv"

        # Set up parameters to pass to Rmd document
        params <- list(
          geo_uid = selected_geographies()$geo_uid,
          level = input$aggregate_area,
          csv_location = data_export
        )

        cat("rendering report \n")
        cat(paste0(Sys.time()), "\n")

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(temp_template,
          output_file = report_html,
          params = params,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )

        cat("report rendered, printing to pdf \n")
        cat(paste0(Sys.time()), "\n")

        # Print to PDF
        pagedown::chrome_print(
          report_html,
          output = report_pdf,
          extra_args = chrome_extra_args(),
          verbose = FALSE
        )

        cat("zipping \n")
        cat(paste0(Sys.time()), "\n")

        # Zip HTML, PDF, and data export
        utils::zip(file, c(report_html, report_pdf, data_export))

        cat("done \n")
        cat(paste0(Sys.time()), "\n")
      }
    )
  })
}

sidebar_header <- function(...) {
  shiny::div(
    ...,
    class = "summary-statistics-header breathe"
  )
}

tooltip <- function(content) {
  shiny::icon("question", `data-html`="true") %>%
    bsplus::bs_embed_popover(title = NULL, content = content, placement = "right", container = "body", trigger = "hover")
}


# Via: https://github.com/RLesur/chrome_print_shiny
#' Return Chrome CLI arguments
#'
#' This is a helper function which returns arguments to be passed to Chrome.
#' This function tests whether the code is running on shinyapps and returns the
#' appropriate Chrome extra arguments.
#'
#' @param default_args Arguments to be used in any circumstances.
#'
#' @return A character vector with CLI arguments to be passed to Chrome.
#' @noRd
chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- default_args
  # Test whether we are in a shinyapps container
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args <- c(
      args,
      "--no-sandbox", # required because we are in a container
      "--disable-dev-shm-usage"
    ) # in case of low available memory
  }
  args
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
