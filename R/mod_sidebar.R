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
    class = "censusagg-sidebar",
    shinybusy::add_busy_spinner("circle", color = "#447E72", height = "30px", width = "30px"),
    shiny::div(
      class = "sidebar-header",
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::img(src = "www/logo.png", alt = "CensusAggregator logo", style = "width: 100%; min-width: 150px;")
        ),
        shiny::column(
          width = 6,
          style = "text-align: right;",
          shiny::actionButton(ns("about"), "About", class = "btn-link btn-nav"),
          shiny::actionButton(ns("contact"), "Contact", class = "btn-link btn-nav", style = "margin-left: 20px;")
        )
      ),
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
      shiny::div(
        sidebar_header(
          "Step 2: Choose an area selection method",
          tooltip(shiny::HTML('Use the "Click to select/deselect" option to select one geographic area at a time. Each selected geographic area will be highlighted with a bold outline. This option also permits the selection of non-contiguous areas.<br><br>Use the "Draw a polygon" option to draw a continuous boundary. Each mouse click marks a new point in the boundary. To complete the polygon selection, use a double mouse click for the final point or click on the first point to close the loop. The census geographic areas that overlap with polygon will be selected and highlighted with a bold outline.')),
          style = "display: inline-block;"
        ),
        shinyjs::disabled(
          shiny::actionButton(
            ns("reset"),
            "Clear selection",
            class = "btn-link btn-secondary-effect", style = "display: inline-block; margin-left: 5px;"
          )
        )
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
      sidebar_header("Step 3: Download data"),
      shiny::div(
        shinyjs::disabled(
          shinyWidgets::dropdownButton(
            inputId = ns("download_report"),
            circle = FALSE,
            inline = TRUE,
            label = "Download report",
            mod_download_report_ui(ns("pdf"), "(.pdf)"),
            mod_download_report_ui(ns("html"), "(.html)")
          )
        ),
        shinyjs::disabled(
          shiny::downloadButton(
            ns("download_data"),
            "Download data (.csv)",
            width = "100%",
            icon = NULL
          )
        ),
        shinyjs::disabled(
          shiny::downloadButton(
            ns("download_boundary"),
            "Download boundary (.geojson)",
            width = "100%",
            icon = NULL
          )
        )
      ),
      breathe(),
      shiny::div(
        shiny::fluidRow(
          shiny::column(
            width = 8,
            sidebar_header("Summary of selected area")
          ),
          shiny::column(
            width = 4,
            shiny::div(
              style = "text-align: right;",
              shinyjs::disabled(
                shiny::actionButton(
                  ns("share"),
                  "Share",
                  class = "btn-link",
                  icon = shiny::icon("share-alt")
                )
              )
            )
          )
        ),
        gt::gt_output(ns("summary_statistics"))
      ),
      # shiny::div(
      #   class = "pa-logo",
      #   shiny::a(
      #     href = "https://purposeanalytics.ca/", target = "_blank",
      #     shiny::img(src = "www/pa-logo.png", alt = "Purpose Analytics logo", width = "50px")
      #   )
      # )
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, boomarks_to_be_parsed, bookmark_bounds) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # About ----

    shiny::observeEvent(
      input$about,
      shiny::showModal(
        shiny::modalDialog(
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          style = "padding: 2rem",
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(
                style = "float: right;",
                shiny::modalButton("Close")
              ),
              shiny::h1("About", style = "float: left;")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::hr(),
              shiny::includeHTML(app_sys("app/www/about.html"))
            )
          )
        )
      )
    )

    # Contact ----

    shiny::observeEvent(
      input$contact,
      shiny::showModal(
        shiny::modalDialog(
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          shiny::div(
            style = "float: right;",
            shiny::modalButton("Close")
          ),
          shiny::tags$iframe(src = "https://purposeanalytics.ca/contact-form", style = "width: 100%; min-height: 800px", frameBorder = "0")
        )
      )
    )

    # Set up bookmarking ----
    bookmark_query <- shiny::reactive(
      bookmark_query <- construct_bookmark(input, session, exclude = c("selection_tool", "export_data", "bookmark_selections", "export_geography", "export_boundary_bttn", "reset", "share", "download_report", "about", "contact", "download_report_state"), selected_geographies())
    )

    shiny::observeEvent(input$share, {
      shiny::showModal(shiny::urlModal(bookmark_query(), "Share link"))
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

    # Summary statistics table ----
    shiny::observeEvent(selected_geographies(), ignoreInit = FALSE, priority = 30, {
      shiny::req(input_aggregate_area())

      if (nrow(selected_geographies()) == 0) {

        # Disable buttons
        shinyjs::disable("reset")
        shinyjs::disable("share")
        shinyjs::disable("download_report")
        shinyjs::disable("download_data")
        shinyjs::disable("download_boundary")

        summary_statistics <- dplyr::tibble(label = c(
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
        shinyjs::enable("share")
        shinyjs::enable("download_report")
        shinyjs::enable("download_data")
        shinyjs::enable("download_boundary")

        summary_statistics_source <- switch(input_aggregate_area(),
          "csd" = censusaggregatorapp::csd,
          "ct" = censusaggregatorapp::ct
        )

        summary_statistics <- summary_statistics_source %>%
          dplyr::inner_join(selected_geographies(), by = "geo_uid") %>%
          dplyr::select(.data$population, .data$households, .data$area_sq_km, .data$population_density) %>%
          dplyr::mutate(n = dplyr::n())

        if (nrow(selected_geographies()) > 1) {
          summary_statistics <- summary_statistics %>%
            dplyr::select(-.data$population_density) %>%
            dplyr::group_by(.data$n) %>%
            dplyr::summarise_all(sum) %>%
            dplyr::mutate(population_density = round(.data$population / .data$area_sq_km)) %>%
            dplyr::ungroup()
        }

        summary_statistics <- summary_statistics %>%
          tidyr::pivot_longer(dplyr::everything()) %>%
          dplyr::mutate(value = dplyr::case_when(
            name %in% c("population", "households") ~ scales::comma(.data$value, accuracy = 1),
            name %in% c("area_sq_km", "population_density") ~ scales::comma(.data$value, accuracy = 0.1),
            name == "n" ~ as.character(.data$value)
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

        summary_statistics <- summary_statistics_labels_and_units %>%
          dplyr::left_join(summary_statistics, by = "name") %>%
          dplyr::mutate(
            value = glue::glue("{value} {units}", .na = "")
          ) %>%
          dplyr::select(.data$label, .data$value)
      }

      output$summary_statistics <- gt::render_gt({
        summary_statistics %>%
          gt::gt() %>%
          gt::sub_missing(columns = .data$value) %>%
          gt::cols_align("right", .data$value) %>%
          gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_body(columns = .data$label)) %>%
          gt::fmt_markdown(columns = .data$value) %>%
          gt::tab_options(
            table.width = "100%",
            column_labels.hidden = TRUE,
            table_body.border.bottom.color = "transparent",
            table_body.border.top.color = "transparent",
            table.font.names = "Lato",
            table.font.size = 14,
            table.align = "left"
          )
      })
    })

    # Export  ----
    mod_download_report_server("pdf", input_aggregate_area(), selected_geographies(), bookmark_query())
    mod_download_report_server("html", input_aggregate_area(), selected_geographies(), bookmark_query())

    output$download_data <- shiny::downloadHandler(
      filename = function() {
        "CensusAggregator Data.csv"
      },
      content = function(file) {
        shiny::req(nrow(selected_geographies()) > 0)
        data <- prepare_data(input_aggregate_area(), selected_geographies()[["geo_uid"]])
        names(data) <- c("Vector", "Breakdown", "Value", "Proportion")
        data[["Proportion"]] <- round(data[["Proportion"]], digits = 3)
        write.csv(data, file, na = "", row.names = FALSE)
      }
    )

    output$download_boundary <- shiny::downloadHandler(
      filename = function() {
        "CensusAggregator Boundary.geojson"
      },
      content = function(file) {
        shiny::req(nrow(selected_geographies()) > 0)
        dataset <- arrow::open_dataset(app_sys(glue::glue("extdata/{input$aggregate_area}")))

        query <- dplyr::filter(dataset, .data$geo_uid %in% selected_geographies()[["geo_uid"]])

        sfarrow::read_sf_dataset(query) %>%
          sf::st_union() %>%
          sf::st_write(file, layer = "CensusAggregator Boundary")
      }
    )
  })
}

sidebar_header <- function(..., style = NULL) {
  shiny::div(
    ...,
    class = "header little-breath",
    style = style
  )
}

tooltip <- function(content) {
  shiny::icon("question-circle", `data-html` = "true", style = "color: lightgrey;") %>%
    bsplus::bs_embed_popover(title = NULL, content = content, placement = "right", container = "body", trigger = "hover")
}
