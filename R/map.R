map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(sf::st_bbox(censusaggregatorapp::ct), pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE)
}
