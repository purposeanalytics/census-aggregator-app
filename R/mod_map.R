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
mod_map_server <- function(id, input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, bookmark_bounds) {
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

    // Add popup to map that says to zoom in when CSDs/CTs aren't shown
    const CSDpopup = new mapboxgl.Popup({ closeOnClick: false, closeButton: false, maxWidth: 'none'})
      .setLngLat([-96, 37.8]) // Doesn't matter where this is since with CSS it is in top left corner
      .setHTML('Please zoom in to see Census Subdivisions.')
      .addTo(map)

    CSDpopup.addClassName('csd-popup')

    const CTpopup = new mapboxgl.Popup({ closeOnClick: false, closeButton: false, maxWidth: 'none' })
      .setLngLat([-96, 37.8])
      .setHTML('Please zoom in to see Census Tracts.')
      .addTo(map)

    CTpopup.addClassName('ct-popup')

    var csdPopup = document.getElementsByClassName('csd-popup')[0];
    var ctPopup = document.getElementsByClassName('ct-popup')[0];

    // Initialize as not visible
    csdPopup.style.display = 'none';
    ctPopup.style.display = 'none';

    // Set parameters
    var csdZoom = 5;
    var ctZoom = 6;

    // Listen to area changing to control zoom popup visibility
    Shiny.addCustomMessageHandler('aggregate_area', function(aggregateArea) {

    console.log(aggregateArea);
// Hide both popups when area changes
csdPopup.style.display = 'none';
ctPopup.style.display = 'none';

// Get current zoom
var curZoom = map.getZoom();

// If zoom < csdZoom, area == csd, show popup
if (curZoom < csdZoom && aggregateArea == 'csd') {
console.log('current zoom too small, show csd');
  csdPopup.style.display = '';
}

// If zoom < ctZoom, area == ct, show popup
if (curZoom < ctZoom && aggregateArea == 'ct') {
console.log('current zoom too small, show ct');
  ctPopup.style.display = '';
}

// Get zoom on zoomEnd
map.on('zoomend', function () {
  var newZoom = map.getZoom();

if (newZoom < csdZoom && aggregateArea == 'csd') {
console.log('aggregateArea according to here is');
console.log(aggregateArea);
console.log(aggregateArea == 'csd');
// If zoom < csdZoom, area == csd, show popup
console.log('new zoom too small, show csd');
  csdPopup.style.display = '';
} else if (newZoom >= csdZoom && aggregateArea == 'csd') {
console.log('new zoom large, hide csd');
// If zoom >= csdZoom, area == csd, hide popup
  csdPopup.style.display = 'none';
}

if (newZoom < ctZoom && aggregateArea == 'ct') {
// If zoom < ctZoom, area == ct, show popup
console.log('new zoom too small, show ct');
  ctPopup.style.display = '';
} else if (newZoom >= ctZoom && aggregateArea == 'ct') {
console.log('new zoom large, hide ct');
// If zoom >= ctZoom, area == ct, hide popup
  ctPopup.style.display = 'none';
}
});
    })

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

    // Also just do this when 'reset' button is clicked - easiest way to reset everything
    Shiny.addCustomMessageHandler('reset', function(message) {
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
        input_aggregate_area()
        selected_geographies()
      },
      priority = 100,
      {
        # browser()
        # Only run these once the map has been rendered for the first time
        shiny::req(map_rendered())
        shiny::req(input_aggregate_area())

        filter_list <- append(
          list("in", "geo_uid"),
          as.list(selected_geographies()[["geo_uid"]])
        )

        switch(input_aggregate_area(),
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
        # browser()
        req(input_selection_tool())

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
        session$sendCustomMessage("aggregate_area", input_aggregate_area())
      }
    )
  })
}
