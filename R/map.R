map_ct <- function() {
  censusaggregatorapp::ct %>%
    mapboxer::as_mapbox_source() %>%
    mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
      mapboxer::fit_bounds(sf::st_bbox(censusaggregatorapp::ct), pitch = 0) %>%
      mapboxer::add_navigation_control(showCompass = FALSE) %>%
      mapboxer::add_line_layer(line_color = "white", line_width = 1.5)
  }
