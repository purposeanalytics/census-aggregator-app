#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "censusagg-map",
    mapboxer::mapboxerOutput(ns("map"), height = "100vh"),
    div(class = "map-overlay-container",
      population_density_legend("ct", ns),
      population_density_legend("csd", ns, display = "block"),
      population_density_legend("ridings", ns),
      div(id = "select-municipality",
          class = "map-overlay",
        shinyWidgets::pickerInput(
          ns("select_municipality"),
          label = "Jump to:",
          choices = municipalities$city_province,
          selected = "Ottawa, ON",
          options = list(`live-search` = TRUE)
        )
      )
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, bookmark_bounds) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- mapboxer::renderMapboxer(
      map() %>%
        add_census_tooltips("csd") %>%
        add_census_tooltips("ct") %>%
        add_census_tooltips("ridings") %>%
        # initiate with "csd" showing
        show_census_layers("csd") %>%
        hide_census_layers("ct") %>%
        hide_census_layers("ridings") %>%
        htmlwidgets::onRender("
    function() {

    var map = mapboxer._widget['map-map'].map;

    // Disable map rotation using right click + drag
    map.dragRotate.disable();

    // Disable map rotation using touch rotation gesture
    map.touchZoomRotate.disableRotation();

    // Send an indicator to shiny that the widget has been rendered, so other reactives don't run until it's rendered
    Shiny.setInputValue('map_rendered', true);

    // Popup to zoom in when CSDs/CTs aren't shown, hide when they are
    showZoomPopup(map);

    // Highlight / fill geography on hover
    highlightGeographyOnHover(map);

    // Polygon draw and associated controls
    polygonDrawControl(map);

}")
    )

    shiny::observeEvent(
      input$select_municipality,
      ignoreInit = FALSE,
      priority = 300,{

        rlog::log_info(paste(input$select_municipality, "selected"))
        selected_municipality <- municipalities |>
          dplyr::filter(city_province == input$select_municipality)

        zoom_out <- 0.7

        mapboxer::mapboxer_proxy(ns("map")) %>%
          mapboxer::fit_bounds(
            c(selected_municipality$longitude - zoom_out,
              selected_municipality$latitude - zoom_out,
              selected_municipality$longitude + zoom_out,
              selected_municipality$latitude + zoom_out)
          ) %>%
          mapboxer::update_mapboxer()

      }
    )


    # Use bounds from any bookmarking to fit the bounds of the map ----
    shiny::observeEvent(
      bookmark_bounds(),
      {
        rlog::log_info("Observe bookmark_bounds")
        mapboxer::mapboxer_proxy(ns("map")) %>%
          mapboxer::fit_bounds(bookmark_bounds()) %>%
          mapboxer::update_mapboxer()
      }
    )

    # Update map based on inputs (CSD/CT) and geographies to be shown ----
    # Geographies to be shown determined via click or bookmark
    shiny::observeEvent(
      {
        input_aggregate_area()
        selected_geographies()
      },
      priority = 100,
      {

        # Only run these once the map has been rendered for the first time
        rlog::log_info("observe event input_aggreatetarea and seleccted geographies")
        shiny::req(map_rendered())
        shiny::req(input_aggregate_area())
        rlog::log_info(paste("Input Aggregate Areas is", input_aggregate_area() ))
        # Change which legend is shown
        if (input_aggregate_area() == "csd") {
          shinyjs::show("csd-legend")
          shinyjs::hide("ridings-legend")
          shinyjs::hide("ct-legend")
        } else if (input_aggregate_area() == "ct") {
          shinyjs::hide("csd-legend")
          shinyjs::hide("ridings-legend")
          shinyjs::show("ct-legend")
        } else if (input_aggregate_area() =='ridings'){
          shinyjs::hide("csd-legend")
          shinyjs::hide("ct-legend")
          shinyjs::show("ridings-legend")
        }


        filter_list <- append(
          list("in", "geo_uid"),
          as.list(selected_geographies()[["geo_uid"]])
        )

    rlog::log_info(paste("Filter list is", dput(filter_list)))

        switch(input_aggregate_area(),
          csd = mapboxer::mapboxer_proxy(ns("map")) %>%
            mapboxer::set_filter(
              layer_id = "csd_line_click",
              filter = filter_list
            ) %>%
            show_census_layers("csd") %>%
            mapboxer::set_filter(
              layer_id = "ct_line_click",
              filter = list("in", "geo_uid", "")
            ) %>%
            mapboxer::set_filter(
              layer_id = "ridings_line_click",
              filter = filter_list
            ) |>
            hide_census_layers("ct") |>
            hide_census_layers("ridings")   ,
          ct = mapboxer::mapboxer_proxy(ns("map")) %>%
            show_census_layers("ct") %>%
            mapboxer::set_filter(
              layer_id = "ct_line_click",
              filter = filter_list
            ) %>%
            mapboxer::set_filter(
              layer_id = "csd_line_click",
              filter = list("in", "geo_uid", "")
            ) %>%
            mapboxer::set_filter(
              layer_id = "ridings_line_click",
              filter = filter_list
            ) |>
            hide_census_layers("csd") |>
            hide_census_layers("ridings")   ,

          ridings= mapboxer::mapboxer_proxy(ns("map")) %>%
            show_census_layers("ridings") %>%
            mapboxer::set_filter(
              layer_id = "ct_line_click",
              filter = filter_list
            ) %>%
            mapboxer::set_filter(
              layer_id = "csd_line_click",
              filter = list("in", "geo_uid", "")
            ) %>%
            mapboxer::set_filter(
              layer_id = "ridings_line_click",
              filter = filter_list
            ) |>
            hide_census_layers("csd") |>
            hide_census_layers("ct")

        ) %>%
          mapboxer::update_mapboxer()
      }
    )
    # Reset geographies clicked when aggregate_area input changes ----
    shiny::observeEvent(
      input_aggregate_area(),
      # Priority = 2 ensures this happens before the bookmark query parsing, which parses out the geography etc - we want that to happen AFTER, so that any geo_uids are retained and not reset
      # priority = 2,
      {
        # Reset geographies clicked when aggregate_area input changes
        selected_geographies(dplyr::tibble())
      }
    )

    # Keep track of geographies that are clicked ----
    shiny::observeEvent(
      input$map_onclick,
      {
        shiny::req(input_selection_tool())

        # Only do this if the selection tool is click (not polygon)

        if (input_selection_tool() == "click") {



          # Check if clicked area is already in selected geographies
          # If it is, clicking again should *deselect* it - remove from the existing tibble
          clicked_id <- input$map_onclick$props$geo_uid
         rlog::log_info(paste("Selection tool click:", clicked_id))

          if (clicked_id %in% selected_geographies()[["geo_uid"]]) {
            selected_geographies(
              selected_geographies() %>%
                dplyr::filter(.data$geo_uid != clicked_id)
            )
          } else {
            # Otherwise, set current value of selected_geographies to be existing tibble, plus new geographies
            rlog::log_info(paste("Selected geographies is:", clicked_id))
            selected_geographies(
              selected_geographies() %>%
                dplyr::bind_rows(dplyr::tibble(geo_uid = clicked_id))
            )
          }
        }
      }
    )

    # Send polygon selection to javascript (to turn on drawing) ---
    shiny::observeEvent(
      input_selection_tool(),
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      {
        session$sendCustomMessage("selection_tool", input_selection_tool())
      }
    )

    # Send aggregate area event to javascript (to clear drawn polygon when area changes, to show/hide Zoom message) ---
    shiny::observeEvent(
      input_aggregate_area(),
      ignoreInit = FALSE,
      {
        shiny::req(map_rendered())
        rlog::log_info("Aggregate area event, clear drawn polygon")
        session$sendCustomMessage("aggregate_area", input_aggregate_area())
      }
    )
  })
}

population_density_legend <- function(geography, ns, display = "none") {
  legend_text <- switch(geography,
    "csd" = censusaggregatorapp::csd_quantiles_text,
    "ct" = censusaggregatorapp::ct_quantiles_text,
    "ridings" = censusaggregatorapp::ridings_quantiles_text
  )

  shiny::div(
    id = ns(glue::glue("{geography}-legend")),
    class = "legend map-overlay",
    style = glue::glue("display: {display};"),
    shiny::tags$b(shiny::HTML("Population density (people/km<sup>2</sup>)")),
    purrr::map2(
      rev(fill_palette), rev(legend_text),
      function(color, text) {
        shiny::div(
          shiny::span(
            class = "legend-key",
            style = glue::glue("background-color: {color};")
          ),
          shiny::span(text)
        )
      }
    ) %>%
      shiny::tagList()
  )
}
