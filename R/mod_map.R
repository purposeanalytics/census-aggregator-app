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
}
")
    )

    # Update map based on inputs (CSD/CT) and geographies to be shown ----
    # Geographies to be shown determined via click (or bookmark, TODO)
    shiny::observeEvent(
      {
        inputs()[["aggregate_area"]]
        selected_geographies()
      },
      {
        filter_list <- append(
          list("in", "geo_uid"),
          as.list(selected_geographies()[["geo_uid"]])
        )

        switch(inputs()[["aggregate_area"]],
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
            mapboxer::set_filter(
              layer_id = "csd_fill_show",
              filter = list("in", "geo_uid", "")
            ) %>%
            hide_census_layers("csd"),
        ) %>%
          mapboxer::update_mapboxer()
      }
    )

    # Reset geographies clicked when aggregate_area input changes ----
    shiny::observeEvent(inputs()[["aggregate_area"]],
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
