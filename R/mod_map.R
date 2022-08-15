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
    style = "width: 70%; padding-right: 30px;",
    mapboxer::mapboxerOutput(ns("map"), height = "1200px")
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, inputs, selected_geographies, map_rendered,
                           polygon_filter) {
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

      let hoveredGeoUid = null;
      // When the user moves their mouse over the csd_line_hover or ct_line_hover layer, we'll update the
      // feature state for the feature under the mouse.
      // Note that the mouse is observed on csd_fill_click and ct_fill_click, which are fill layers, so it's observed when their mouse is inside the geography
      // But the layer that is actually updated is csd_line_hover or ct_line_hover, in order to just update the border of the geography
      map.on('mousemove', 'csd_fill_click', (e) => {
        map.getCanvas().style.cursor = 'pointer';
        if (e.features.length > 0) {
          if (hoveredGeoUid !== null) {
            map.setFeatureState(
              { source: '2016_csd', sourceLayer: '2016_census_csd', id: hoveredGeoUid },
              { hover: false }
            );
          }
        hoveredGeoUid = e.features[0].id;
          map.setFeatureState(
            { source: '2016_csd', sourceLayer: '2016_census_csd', id: hoveredGeoUid },
            { hover: true }
          );
        }
      });
      // When the mouse leaves the csd_fill_click layer, update the feature state of the
      // previously hovered feature.
      map.on('mouseleave', 'csd_fill_click', () => {
        if (hoveredGeoUid !== null) {
          map.setFeatureState(
            { source: '2016_csd', sourceLayer: '2016_census_csd', id: hoveredGeoUid },
            { hover: false }
          );
        }
        hoveredGeoUid = null;
        // Reset the cursor style
        map.getCanvas().style.cursor = '';
      })

      map.on('mousemove', 'ct_fill_click', (e) => {
        map.getCanvas().style.cursor = 'pointer';
        if (e.features.length > 0) {
          if (hoveredGeoUid !== null) {
            map.setFeatureState(
              { source: '2016_ct', sourceLayer: '2016_census_ct', id: hoveredGeoUid },
              { hover: false }
            );
          }
        hoveredGeoUid = e.features[0].id;
          map.setFeatureState(
            { source: '2016_ct', sourceLayer: '2016_census_ct', id: hoveredGeoUid },
            { hover: true }
          );
        }
      });
      map.on('mouseleave', 'ct_fill_click', () => {
        if (hoveredGeoUid !== null) {
          map.setFeatureState(
            { source: '2016_ct', sourceLayer: '2016_census_ct', id: hoveredGeoUid },
            { hover: false }
          );
        }
        hoveredGeoUid = null;
        // Reset the cursor style
        map.getCanvas().style.cursor = '';
      })

    // Draw a polygon

    var draw = new MapboxDraw({
        displayControlsDefault: false,
        controls: {
            polygon: true,
            trash: true
        },
        defaultMode: 'draw_polygon'
    });

    Shiny.addCustomMessageHandler('selection_tool', function(message) {

    if (message === 'polygon') {

      if (!(map._draw)) {
        map.addControl(draw);
      }

      map._draw = true;

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.delete', (e) => getFeaturesFromPolygon(e, map, 'csd'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'csd'));

      map.on('draw.create', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.delete', (e) => getFeaturesFromPolygon(e, map, 'ct'));
      map.on('draw.update', (e) => getFeaturesFromPolygon(e, map, 'ct'));

    }

    if (message === 'click' && map._draw) {
      map.removeControl(draw);
      map._draw = false;
    }

  })
}
")
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
          as.list(selected_geographies()$geo_uid)
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
      {
        session$sendCustomMessage("selection_tool", inputs()[["selection_tool"]])
      }
    )

    # Keep track of geographies that are selected via polygon ----
    shiny::observeEvent(
      polygon_filter(),
      {
        selected_geographies(
          tibble::tibble(geo_uid = polygon_filter())
        )
      }
    )
  })
}
