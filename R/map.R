map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::set_view_state(-106.6702, 52.1579, zoom = 8) %>%
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
    id = geography_to_source_id(geography)
    ) %>%
    add_census_fill_layer(geography) %>%
    add_census_line_layer(geography)
}

add_census_fill_layer <- function(map, geography) {
  map %>%
    mapboxer::add_layer(
      list(
        "id" = geography_to_layer_id(geography, "fill"),
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
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

add_census_line_layer <- function(map, geography) {
  map %>%
    mapboxer::add_layer(
      list(
        "id" = geography_to_layer_id(geography, "line"),
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
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

geography_to_layer_id <- function(geography, type) {
  glue::glue("{geography}_{type}")
}

geography_to_source_id <- function(geography) {
  glue::glue("2016_{geography}")
}

geography_to_source_layer_id <- function(geography) {
  glue::glue("2016_census_{geography}")
}

show_census_layers <- function(map, geography) {
  map %>%
    toggle_layer_visible(geography_to_layer_id(geography, "fill")) %>%
    toggle_layer_visible(geography_to_layer_id(geography, "line"))
}

hide_census_layers <- function(map, geography) {
  map %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "fill")) %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "line"))
}

toggle_layer_visible <- function(map, id) {
  map %>%
    mapboxer::set_layout_property(
      layer_id = id,
      "visibility",
      "visible"
    )
}

toggle_layer_invisible <- function(map, id) {
  map %>%
    mapboxer::set_layout_property(
      layer_id = id,
      "visibility",
      "none"
    )
}
