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
    population_density_legend("ct", ns, display = "block"),
    population_density_legend("csd", ns)
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
        show_census_layers("ct") %>%
        hide_census_layers("csd") %>%
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

    # Use bounds from any bookmarking to fit the bounds of the map ----
    shiny::observeEvent(
      bookmark_bounds(),
      {
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
        shiny::req(map_rendered())
        shiny::req(input_aggregate_area())

        # Change which legend is shown
        if (input_aggregate_area() == "csd") {
          shinyjs::show("csd-legend")
          shinyjs::hide("ct-legend")
        } else if (input_aggregate_area() == "ct") {
          shinyjs::hide("csd-legend")
          shinyjs::show("ct-legend")
        }

        filter_list <- append(
          list("in", "geo_uid"),
          as.list(selected_geographies()[["geo_uid"]])
        )

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
            hide_census_layers("ct"),
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
            hide_census_layers("csd"),
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

          if (clicked_id %in% selected_geographies()[["geo_uid"]]) {
            selected_geographies(
              selected_geographies() %>%
                dplyr::filter(.data$geo_uid != clicked_id)
            )
          } else {
            # Otherwise, set current value of selected_geographies to be existing tibble, plus new geographies
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
        session$sendCustomMessage("aggregate_area", input_aggregate_area())
      }
    )
  })
}

population_density_legend <- function(geography, ns, display = "none") {
  palette <- c("#dbf0ec", "#afe9de", "#6bc7b5", "#3a9281", "#155e4f")

  legend_text <- switch(geography,
    "csd" = censusaggregatorapp::csd_quantiles_text,
    ct = censusaggregatorapp::ct_quantiles_text
  )

  shiny::div(
    id = ns(glue::glue("{geography}-legend")),
    class = "legend map-overlay",
    style = glue::glue("display: {display};"),
    shiny::tags$b("Population density"),
    purrr::map2(
      palette, legend_text,
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
