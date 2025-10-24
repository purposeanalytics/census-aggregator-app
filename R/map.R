map <- function() {

  selected_municipality <- municipalities |>
    dplyr::filter(city_province == "Ottawa, ON")

  mapboxer::mapboxer(style = "mapbox://styles/purposeanalytics/cl6mafpzk002r14pdbda7la8r") %>%
    mapboxer::set_view_state(selected_municipality$longitude, selected_municipality$latitude, zoom = 8.5) %>%
    mapboxer::add_navigation_control(showCompass = FALSE, pos = "top-right") %>%
    add_census_layer("ct") %>%
    add_census_layer("csd")  %>%
    add_census_layer("ridings")
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

fill_palette <- c("#dbf0ec", "#9de2d4", "#5fd3bc", "#349b87", "#086351")

add_census_fill_layer <- function(map, geography) {
  click_layer_id <- geography_to_layer_id(geography, "fill_click")
  quantiles <- switch(geography,
    csd = censusaggregatorapp::csd_population_density_quantiles,
    ct = censusaggregatorapp::ct_population_density_quantiles,
    ridings = censusaggregatorapp::ridings_population_density_quantiles
  )

  fill_opacity <- ifelse(geography == "ridings", 0.75/1.5, 0.75)

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
            list("<", c("get", "population_density"), quantiles[2]), fill_palette[1],
            list("<", c("get", "population_density"), quantiles[3]), fill_palette[2],
            list("<", c("get", "population_density"), quantiles[4]), fill_palette[3],
            list("<", c("get", "population_density"), quantiles[5]), fill_palette[4],
            list("<", c("get", "population_density"), quantiles[6]), fill_palette[5],
            # Default - should never come up
            fill_palette[1]
          ),
          "fill-opacity" = fill_opacity
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

 rlog::log_info(paste("line layer id" ,  line_layer_id))
 rlog::log_info(paste("click layer id" ,  click_layer_id))
 rlog::log_info(paste("line layer id" ,  hover_layer_id))

 line_opacity <- ifelse(geography == "ridings", 0.25/2, 0.25)

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
          "line-opacity" = line_opacity
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

add_place_names <- function(map){

  map %>%
    mapboxer::add_source(
      id = "mapbox-streets",
      source = mapboxer::mapbox_source(
        type = "vector",
        url = "mapbox://mapbox.mapbox-streets-v8"
      )
      )%>%
    mapboxer::add_layer(
      list(
        id = "place-labels",
        type = "symbol",
        source = "mapbox-streets",
        "source-layer" = "place_label",
        filter = list(
          "all",
          list(
            "<=", list("get", "filterrank"), 3
          ),
          list("!=", list("get", "class"), "country"),
          list("==", list("get", "iso_3166_1"), "CA"),
          list("!=", list("get", "class"), "state"),
          list(">", list("zoom"), 4)  # Adjust zoom level as needed
        ),
                layout = list(
          "text-field" = "{name}",
          "text-font" = c("DIN Pro Medium", "Arial Unicode MS Bold"),
          "text-size" = list(
            "interpolate",
            list("linear"),
            list("get", "symbolrank"),
            5, 20,
            10, 12
          )
        ),
        paint = list(
          "text-color" = "#555555",
          "text-opacity" = list(
            "interpolate",
            list("linear"),
            list("get", "symbolrank"),
            3, 0.95,  # Fully opaque for highest rank
            10, 0.65  # Semi-transparent for lowest rank
          ),
          "text-halo-color" = "rgba(132, 219, 202, 0.3)",  # Outline color
          "text-halo-width" = 1  # Outline width
        ),
        layout = list(
          "visibility" = "none"
        )
      )
    )

}
