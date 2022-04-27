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
    mapboxer::mapboxerOutput(ns("map"), height = "800px")
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
        # Observe zoom-out level, once rendered, to know whether to zoom back out to "city view"
        htmlwidgets::onRender("function() {

      var map = mapboxer._widget['map-map'].map;
      // disable map rotation using right click + drag
      map.dragRotate.disable();

      // disable map rotation using touch rotation gesture
      map.touchZoomRotate.disableRotation();

      // Listen for what geographies have been clicked
      Shiny.addCustomMessageHandler('clicked_id', function(clicked_id) {
        console.log(clicked_id);

      // Set the Click feature of those geographies to true
          map.setFeatureState(
            { source: '2016_ct', sourceLayer: '2016_census_ct', id: clicked_id },
            { click: true }
          );
      });
        }")
    )


    # Update map based on inputs ----
    shiny::reactive({
      switch(inputs()[["aggregate_area"]],
        csd = mapboxer::mapboxer_proxy(ns("map")) %>%
          toggle_layer_visible("csd") %>%
          toggle_layer_invsibile("ct"),
        ct = mapboxer::mapboxer_proxy(ns("map")) %>%
          toggle_layer_visible("ct") %>%
          toggle_layer_invsibile("csd")
      ) %>%
        mapboxer::update_mapboxer()
    })

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
              dplyr::bind_rows(input$map_onclick$props %>% dplyr::as_tibble())
          )
        }

        # Sent the IDs to javascript
        session$sendCustomMessage("clicked_id", selected_geographies()[["geo_uid"]])
      }
    )
  })
}
