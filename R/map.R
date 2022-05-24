map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    # mapboxer::fit_bounds(canada, pitch = 0) %>%
    set_view_state(-106.6702, 52.1579, zoom = 8) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    add_census_layer("ct") %>%
    add_census_layer("csd")
}

add_census_layer <- function(map, geography) {
  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = glue::glue("mapbox://purposeanalytics.2016_{geography}")
    ),
    id = glue::glue("2016_{geography}")
    ) %>%
    add_census_fill_layer(glue::glue("2016_{geography}"), glue::glue("2016_census_{geography}")) %>%
    add_census_line_layer(glue::glue("2016_{geography}"), glue::glue("2016_census_{geography}"))
}

add_census_fill_layer <- function(map, source, source_layer) {
  map %>%
    mapboxer::add_layer(
      list(
        "id" = glue::glue("{source}_fill"),
        "type" = "fill",
        "source" = source,
        "source-layer" = source_layer,
        "paint" = list(
          "fill-opacity" = 0.25,
          "fill-color" = list(
            "case",
            list("boolean", c("feature-state", "click"), FALSE), "white",
            "blue"
          )
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    )
}

add_census_line_layer <- function(map, source, source_layer) {
  map %>%
    mapboxer::add_layer(
    list(
      "id" = glue::glue("{source}_line"),
      source = source,
      "source-layer" = source_layer,
      type = "line",
      paint = list(
        "line-color" = "red",
        "line-width" = 1.5,
        "line-opacity" = 0.5
      ),
      layout = list(
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
