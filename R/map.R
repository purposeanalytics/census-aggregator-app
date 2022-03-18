map_ct <- function() {
  censusaggregatorapp::ct %>%
    mapboxer::as_mapbox_source() %>%
    mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(sf::st_bbox(censusaggregatorapp::ct), pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    mapboxer::add_layer(
      list(
        "id" = "ct_line",
        "type" = "line",
        "paint" = list(
          "line-color" = "red",
          "line-width" = 1.5,
          "line-opacity" = 0.5
        )
      )
    )
}

map_csd <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(sf::st_bbox(censusaggregatorapp::ct), pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE)
}
