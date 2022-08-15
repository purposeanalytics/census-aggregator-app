map <- function() {
  mapboxer::mapboxer(style = "mapbox://styles/purposeanalytics/cl6mafpzk002r14pdbda7la8r") %>%
    mapboxer::set_view_state(-92, 52, zoom = 5) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    # mapboxer::add_draw_control(
    #   displayControlsDefault = FALSE,
    #   controls = list(
    #     polygon = TRUE,
    #     trash = TRUE
    #   )
    # ) %>%
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
        ),
        popup = "{{geo_uid}}"
      )
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = show_layer_id,
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "fill-opacity" = 0.75,
          "fill-color" = "black"
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
  line_layer_id <- geography_to_layer_id(geography, "line")
  hover_layer_id <- geography_to_layer_id(geography, "line_hover")

  map %>%
    mapboxer::add_layer(
      list(
        "id" = line_layer_id,
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        paint = list(
          "line-color" = "#0745a8",
          "line-width" = 1.5,
          "line-opacity" = 0.25
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = hover_layer_id,
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "line-color" = "#0745a8",
          "line-opacity" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 1,
            0
          ),
          "line-width" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 2,
            1.5
          )
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

add_census_tooltips <- function(map, geography) {
  tooltip_text <-
    "
  <b>Region</b>: {{region_name}}<br>
  <b>Population</b>: {{population}}<br>
  <b>Households</b>: {{households}}<br>
  <b>Area</b>: {{area_sq_km}} km<sup>2</sup><br>
  <b>Population Density</b>: {{population_density}} people per km<sup>2</sup>
  "

  map %>%
    mapboxer::add_tooltips(geography_to_layer_id(geography, "fill_click"), tooltip_text)
}
