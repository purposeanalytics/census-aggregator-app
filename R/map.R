map <- function() {
  mapboxer::mapboxer(style = "mapbox://styles/purposeanalytics/cl6mafpzk002r14pdbda7la8r") %>%
    mapboxer::set_view_state(-80, 45, zoom = 6) %>%
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
  quantiles <- get(paste0(geography, "_population_density_quantiles"))
  palette <- c("#dbf0ec", "#afe9de", "#6bc7b5", "#3a9281", "#155e4f")


  map %>%
    mapboxer::add_layer(
      list(
        "id" = click_layer_id,
        "type" = "fill",
        "source" = geography_to_source_id(geography),
        "source-layer" = geography_to_source_layer_id(geography),
        "paint" = list(
          "fill-color" = list(
            "property" = "population_density",
            stops = list(
              list(quantiles[1], palette[1]),
              list(quantiles[2], palette[2]),
              list(quantiles[3], palette[2]),
              list(quantiles[4], palette[4]),
              list(quantiles[5], palette[5])
            )
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
          "line-color" = "#155e4f",
          "line-width" = 2,
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
          "line-color" = "#155e4f",
          "line-width" = 2,
          "line-opacity" = 0.75
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
  <b>Region</b>: {{region_name}}<br>
  <b>Census ID</b>: {{geo_uid}}<br>
  <b>Population</b>: {{population_fmt}}<br>
  <b>Households</b>: {{households_fmt}}<br>
  <b>Area</b>: {{area_sq_km_fmt}} km<sup>2</sup><br>
  <b>Population Density</b>: {{population_density_fmt}} people/km<sup>2</sup>
  "

  map %>%
    mapboxer::add_tooltips(geography_to_layer_id(geography, "fill_click"), tooltip_text)
}
