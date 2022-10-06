#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    height = "calc(100vh - 106px)",
    shiny::div(
      mapboxer::mapboxerOutput(ns("map"), height = "calc(100vh - 40px - 106px)")
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, inputs, selected_geographies, map_rendered, bookmark_bounds) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- mapboxer::renderMapboxer(
      map() %>%
        add_census_tooltips("csd") %>%
        add_census_tooltips("ct") %>%
        show_census_layers("csd") %>%
        hide_census_layers("ct") %>%
        htmlwidgets::onRender("
    function() {

    var map = mapboxer._widget['map-map'].map;
    // disable map rotation using right click + drag
    map.dragRotate.disable();

    // disable map rotation using touch rotation gesture
    map.touchZoomRotate.disableRotation();

    // Send an indicator to shiny that the widget has been rendered, so other reactives don't run until it's rendered
    Shiny.setInputValue('map_rendered', true);

    // Highlight / fill geography on hover
    highlightGeographyOnHover(map);

    // Draw a polygon

    var draw = new MapboxDraw({
        displayControlsDefault: false,
        controls: {
            polygon: true,
            trash: true
        },
        defaultMode: 'draw_polygon'
    });

    // When the selection tool changes
    Shiny.addCustomMessageHandler('selection_tool', function(message) {

    if (message === 'polygon') {

    // If the map does not already have a drawing control
      if (!(map._draw)) {
        map.addControl(draw);
      }

      map._draw = true; // Flag drawing control for above check

    // Clear everything any time the aggregate area changes - by removing and adding the control back on
    // This ensures that the polygon is deleted and the features are cleared
    Shiny.addCustomMessageHandler('aggregate_area', function(message) {
        map.removeControl(draw);
        map.addControl(draw);
    })

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.delete', (e) => clearFeatures(e, 'csd'));
      // Only allows drawing one polygon at a time - clears existing polygon and features if you go to draw a new one
      map.on('draw.modechange', (e) => clearPolygonAndFeatures(e, map, 'csd', draw));

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.delete', (e) => clearFeatures(e, 'ct'));
      map.on('draw.modechange', (e) => clearPolygonAndFeatures(e, map, 'ct', draw));
    }

    // Remove drawing control and set to false if the selection tool is click
      if (message === 'click' && map._draw) {
        map.removeControl(draw);
        map._draw = false;
      }

  })

}
")
    )

    # Use bounds from any bookmarking to fit the bounds of the map ----
    shiny::observeEvent(
      bookmark_bounds(), {
        mapboxer::mapboxer_proxy(ns("map")) %>%
          mapboxer::fit_bounds(bookmark_bounds()) %>%
          mapboxer::update_mapboxer()
      }
    )

    # Update map based on inputs (CSD/CT) and geographies to be shown ----
    # Geographies to be shown determined via click or bookmark
    shiny::observeEvent(
      {
        inputs()[["aggregate_area"]]
        selected_geographies()
      },
      {
        # Only run these once the map has been rendered for the first time
        shiny::req(map_rendered())
        shiny::req(inputs()$aggregate_area)

        filter_list <- append(
          list("in", "geo_uid"),
          as.list(selected_geographies()[["geo_uid"]])
        )

        switch(inputs()$aggregate_area,
          csd = mapboxer::mapboxer_proxy(ns("map")) %>%
            mapboxer::set_filter(
              layer_id = "csd_fill_show",
              filter = filter_list
            ) %>%
            show_census_layers("csd") %>%
            mapboxer::set_filter(
              layer_id = "ct_fill_show",
              filter = list("in", "geo_uid", "")
            ) %>%
            hide_census_layers("ct"),
          ct = mapboxer::mapboxer_proxy(ns("map")) %>%
            show_census_layers("ct") %>%
            mapboxer::set_filter(
              layer_id = "ct_fill_show",
              filter = filter_list
            ) %>%
            # mapboxer::set_filter(
            #   layer_id = "csd_fill_show",
            #   filter = list("in", "geo_uid", "")
            # ) %>%
            hide_census_layers("csd"),
        ) %>%
          mapboxer::update_mapboxer()
      }
    )

    # Reset geographies clicked when aggregate_area input changes ----
    shiny::observeEvent(
      inputs()[["aggregate_area"]],
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

        # Only do this if the selection tool is click (not polygon)

        if (inputs()[["selection_tool"]] == "click") {

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
      inputs()[["selection_tool"]],
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      {
        session$sendCustomMessage("selection_tool", inputs()[["selection_tool"]])
      }
    )

    # Send aggregate area event to javascript (to clear drawn polygon when area changes) ---
    shiny::observeEvent(
      inputs()[["aggregate_area"]],
      {
        session$sendCustomMessage("aggregate_area", inputs()[["aggregate_area"]])
      }
    )
  })
}
