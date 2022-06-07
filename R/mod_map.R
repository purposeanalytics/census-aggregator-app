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
mod_map_server <- function(id, inputs, selected_geographies) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- mapboxer::renderMapboxer(
      map() %>%
        show_census_layers("csd") %>%
        hide_census_layers("ct") %>%
        htmlwidgets::onRender("
function() {

    var map = mapboxer._widget['map-map'].map;
    // disable map rotation using right click + drag
    map.dragRotate.disable();

    // disable map rotation using touch rotation gesture
    map.touchZoomRotate.disableRotation();

    // Listen to aggregate-area to that when CSD/CT is changed, all the 'click' feature states are removed
    Shiny.addCustomMessageHandler('geography', function(geography) {

      map.removeFeatureState({source: '2016_ct', sourceLayer: '2016_census_ct', stateKey: 'click'});
      map.removeFeatureState({source: '2016_csd', sourceLayer: '2016_census_csd', stateKey: 'click'});

    });

    // When the user clicks the ct_fill layer, update the 'click' feature state for that geo_uid
    map.on('click', 'ct_fill', (e) => {

        // Cycle through rendered features to find the clicked one, and see what its 'click' value is
        // If true, it has already been clicked and needs to be 'unclicked' - set to false
        // If false, has already been clicked + unclicked, needed to be 'clicked' - set to true
        // If null, has not been clicked, needs to be 'clicked' - set to true

        var clicked_geo_uid = e.features[0].id;

        var features = map.queryRenderedFeatures({ layers: ['ct_fill'] });

        for (var i = 0; i < features.length; i++) {

            var current_geo_uid = features[i].id;

            if (clicked_geo_uid === current_geo_uid) {
                var geo_uid_already_clicked = features[i].state.click;

                if (geo_uid_already_clicked) {
                    console.log('already clicked');

                    map.setFeatureState(
                        { source: '2016_ct', sourceLayer: '2016_census_ct', id: clicked_geo_uid },
                        { click: false }
                    );

                } else if (!geo_uid_already_clicked) {
                    console.log('not already clicked');

                    map.setFeatureState(
                        { source: '2016_ct', sourceLayer: '2016_census_ct', id: clicked_geo_uid },
                        { click: true }
                    );

                }
            }
        }
    });

    // Same for csd_fill
    map.on('click', 'csd_fill', (e) => {

        // Cycle through rendered features to find the clicked one, and see what its 'click' value is
        // If true, it has already been clicked and needs to be 'unclicked' - set to false
        // If false, has already been clicked + unclicked, needed to be 'clicked' - set to true
        // If null, has not been clicked, needs to be 'clicked' - set to true

        var clicked_geo_uid = e.features[0].id;

        console.log(e.features[0]);

        var features = map.queryRenderedFeatures({ layers: ['csd_fill'] });

        for (var i = 0; i < features.length; i++) {

            var current_geo_uid = features[i].id;

            if (clicked_geo_uid === current_geo_uid) {
                var geo_uid_already_clicked = features[i].state.click;

                if (geo_uid_already_clicked) {
                    console.log('already clicked');

                    map.setFeatureState(
                        { source: '2016_csd', sourceLayer: '2016_census_csd', id: clicked_geo_uid },
                        { click: false }
                    );

                } else if (!geo_uid_already_clicked) {
                    console.log('not already clicked');

                    map.setFeatureState(
                        { source: '2016_csd', sourceLayer: '2016_census_csd', id: clicked_geo_uid },
                        { click: true }
                    );

                }
            }
        }
    });


}
")
    )

    # Update map based on inputs ----
    shiny::observeEvent(inputs()[["aggregate_area"]], {
      switch(inputs()[["aggregate_area"]],
        csd = mapboxer::mapboxer_proxy(ns("map")) %>%
          show_census_layers("csd") %>%
          hide_census_layers("ct"),
        ct = mapboxer::mapboxer_proxy(ns("map")) %>%
          show_census_layers("ct") %>%
          hide_census_layers("csd"),
      ) %>%
        mapboxer::update_mapboxer()
    })

    shiny::observeEvent(inputs()[["aggregate_area"]],
      # Priority = 2 ensures this happens before the bookmark query parsing, which parses out the geography etc - we want that to happen AFTER, so that any geo_uids are retained and not reset
      priority = 2,
      {
        # Reset geographies clicked when aggregate_area input changes
        selected_geographies(dplyr::tibble())

        # Send aggregate_area input to JS, to reset the click feature state whenever the aggregate_area input changes
        session$sendCustomMessage("geography", inputs()[["aggregate_area"]])
      }
    )

    # Keep track of geographies that are clicked ----
    shiny::observeEvent(
      input$map_onclick,
      {

        # Check if clicked area is already in selected geographies
        # If it is, clicking again should *deselect* it - remove from the existing tibble
        clicked_id <- input$map_onclick$props$geo_uid

        if (clicked_id %in% selected_geographies()[["geo_uid"]]) {
          selected_geographies(
            selected_geographies() %>%
              dplyr::filter(geo_uid != clicked_id)
          )
        } else {
          # Otherwise, set current value of selected_geographies to be existing tibble, plus new geographies
          selected_geographies(
            selected_geographies() %>%
              dplyr::bind_rows(dplyr::tibble(geo_uid = clicked_id))
          )
        }
      }
    )
  })
}
