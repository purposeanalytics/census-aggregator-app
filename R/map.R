map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(sf::st_bbox(censusaggregatorapp::ct), pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    mapboxer::add_source(censusaggregatorapp::ct, id = "ct") %>%
    mapboxer::add_layer(
      list(
        "id" = "ct_line",
        "type" = "line",
        "paint" = list(
          "line-color" = "red",
          "line-width" = 1.5,
          "line-opacity" = 0.5
        ),
        "layout" = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = "csd_line",
        "type" = "line",
        "paint" = list(
          "line-color" = "blue",
          "line-width" = 1.5,
          "line-opacity" = 0.5
        ),
        "layout" = list(
          "visibility" = "none"
        )
      )
    )
}

toggle_layer_visible <- function(map, id) {
  map %>% mapboxer::set_layout_property(
    layer_id = id, "visibility",
    "visible"
  )
}

toggle_layer_invsibile <- function(map, id) {
  map %>% mapboxer::set_layout_property(
    layer_id = id, "visibility",
    "none"
  )
}
