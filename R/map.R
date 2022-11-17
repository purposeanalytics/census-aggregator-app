map <- function() {
  mapboxer::mapboxer(style = "mapbox://styles/purposeanalytics/cl6mafpzk002r14pdbda7la8r") %>%
    mapboxer::set_view_state(-79.38, 43.8, zoom = 10) %>%
    mapboxer::add_navigation_control(showCompass = FALSE, pos = "top-right") %>%
    add_census_layer("ct") %>%
    add_census_layer("csd")
}

add_census_layer <- function(map, geography) {
  map %>%
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = glue::glue("mapbox://purposeanalytics.2021_{geography}"),
      promoteId = "geo_uid"
    ),
    id = geography_to_source_id(geography)
    ) %>%
    add_census_fill_layer(geography) %>%
    add_census_line_layer(geography)
}

add_census_fill_layer <- function(map, geography) {
  click_layer_id <- geography_to_layer_id(geography, "fill_click")
  quantiles <- switch(geography,
    csd = censusaggregatorapp::csd_population_density_quantiles,
    ct = censusaggregatorapp::ct_population_density_quantiles
  )
  palette <- c("#dbf0ec", "#9de2d4", "#5fd3bc", "#349b87", "#086351")

  map %>%
    mapboxer::add_layer(
      list(
        "id" = click_layer_id,
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "fill-color" = list(
              "case",
              list("<", c("get", "population_density"), quantiles[2]), palette[1],
              list("<", c("get", "population_density"), quantiles[3]), palette[2],
              list("<", c("get", "population_density"), quantiles[4]), palette[3],
              list("<", c("get", "population_density"), quantiles[5]), palette[4],
              list("<", c("get", "population_density"), quantiles[6]), palette[5],
              # Default - should never come up
              palette[1]
          ),
          "fill-opacity" = 0.75
        ),
        layout = list(
          "visibility" = "none"
        ),
        popup = "{{geo_uid}}"
      )
    )
}

add_census_line_layer <- function(map, geography) {
  line_layer_id <- geography_to_layer_id(geography, "line")
  click_layer_id <- geography_to_layer_id(geography, "line_click")
  hover_layer_id <- geography_to_layer_id(geography, "line_hover")

  map %>%
    mapboxer::add_layer(
      list(
        "id" = line_layer_id,
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        paint = list(
          "line-color" = "white",
          "line-width" = 1,
          "line-opacity" = 0.25
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = click_layer_id,
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        paint = list(
          "line-color" = "#086351",
          "line-width" = 3,
          "line-opacity" = 1
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    ) %>%
    mapboxer::set_filter(
      layer_id = click_layer_id,
      # Start with all data filtered OUT
      filter = list("in", "geo_uid", "")
    ) %>%
    mapboxer::add_layer(
      list(
        "id" = hover_layer_id,
        "type" = "line",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "line-color" = "#fff703",
          "line-opacity" = list(
            "case",
            list("boolean", c("feature-state", "hover"), FALSE), 1,
            0
          ),
          "line-width" = 3
        )
      )
    )
}

geography_to_layer_id <- function(geography, type) {
  glue::glue("{geography}_{type}")
}

geography_to_source_id <- function(geography) {
  glue::glue("2021_{geography}")
}

geography_to_source_layer_id <- function(geography) {
  glue::glue("2021_census_{geography}")
}

show_census_layers <- function(map, geography) {
  map %>%
    toggle_layer_visible(geography_to_layer_id(geography, "fill_click")) %>%
    toggle_layer_visible(geography_to_layer_id(geography, "line_click")) %>%
    toggle_layer_visible(geography_to_layer_id(geography, "line"))
}

hide_census_layers <- function(map, geography) {
  map %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "fill_click")) %>%
    toggle_layer_invisible(geography_to_layer_id(geography, "line_click")) %>%
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
  <b>Census Unit ID</b>: {{geo_uid}}<br>
  Region: {{region_name}}<br>
  Population: {{population_fmt}}<br>
  Households: {{households_fmt}}<br>
  Area: {{area_sq_km_fmt}} km<sup>2</sup><br>
  Population density: {{population_density_fmt}} people/km<sup>2</sup>
  "

  map %>%
    mapboxer::add_tooltips(geography_to_layer_id(geography, "fill_click"), tooltip_text)
}
