map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(canada, pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    # CT
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "mapbox://purposeanalytics.2016_ct"
    ),
    id = "2016_ct"
    ) %>%
    mapboxer::add_layer(
      list(
        id = "ct_line",
        source = "2016_ct",
        "source-layer" = "2016_census_ct",
        type = "line",
        paint = list(
          "line-color" = "red",
          "line-width" = 1.5,
          "line-opacity" = 0.5
        )
        # ,
        # layout = list(
        #   "visibility" = "none"
        # )
      )
    ) %>%
    ## Add a "blank" layer for clicking on, that contains all CTs
    mapboxer::add_layer(
      list(
        "id" = "ct_fill",
        "type" = "fill",
        "source" = "2016_ct",
        "source-layer" = "2016_census_ct",
        "paint" = list(
          "fill-color" = "white",
          "fill-opacity" = 0.0001
        )
      )
    ) %>%
    # CSD
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "purposeanalytics.2016_csd_test"
    ),
    id = "2016_csd"
    ) %>%
    mapboxer::add_layer(
      list(
        id = "csd_line",
        source = "2016_csd",
        "source-layer" = "2016_census_csd_test",
        type = "line",
        paint = list(
          "line-color" = "blue",
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
