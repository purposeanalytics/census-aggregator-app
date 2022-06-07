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
      url = glue::glue("mapbox://purposeanalytics.2016_{geography}"),
      promoteId = "geo_uid"
    ),
    id = geography_to_source_id(geography)
    ) %>%
    add_census_fill_layer(geography) %>%
    add_census_line_layer(geography)
}

add_census_fill_layer <- function(map, geography) {
  click_layer_id <- geography_to_layer_id(geography, "fill_click")
  show_layer_id <- geography_to_layer_id(geography, "fill_show")

  map %>%
    mapboxer::add_layer(
      list(
        "id" = click_layer_id,
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "fill-opacity" = 0,
          "fill-color" = "white"
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = show_layer_id,
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "fill-opacity" = 1,
          "fill-color" = "white"
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::set_filter(
      layer_id = show_layer_id,
      # Start with all data filtered OUT
      filter = list("in", "geo_uid", "")
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
    toggle_layer_visible(geography_to_layer_id(geography, "fill_click")) %>%
    toggle_layer_visible(geography_to_layer_id(geography, "fill_show")) %>%
    toggle_layer_visible(geography_to_layer_id(geography, "line"))
}

hide_census_layers <- function(map, geography) {
  map %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "fill_click")) %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "fill_show")) %>%
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
