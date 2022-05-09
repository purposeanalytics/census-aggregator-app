map <- function() {
  mapboxer::mapboxer(style = mapboxer::basemaps$Mapbox$streets_v11) %>%
    mapboxer::fit_bounds(canada, pitch = 0) %>%
    mapboxer::add_navigation_control(showCompass = FALSE) %>%
    # CT
    mapboxer::add_source(mapboxer::mapbox_source(
      type = "vector",
      url = "mapbox://purposeanalytics.2016_ct_polygon",
      promoteId = "geo_uid"
    ),
    id = "2016_ct_polygon"
    ) %>%
    ## Add a "blank" layer for clicking on, that contains all CTs
    mapboxer::add_layer(
      list(
        "id" = "ct_fill",
        "type" = "fill",
        "source" = "2016_ct_polygon",
        "source-layer" = "2016_census_ct_polygon",
        "paint" = list(
          # "fill-color" = "white",
          "fill-opacity" = 0.25,
          "fill-color" = list(
            "case",
            list("boolean", c("feature-state", "click"), FALSE), "white",
            "blue"
          )
        )
      )
    ) %>%
    mapboxer::add_layer(
      list(
        id = "ct_line",
        source = "2016_ct_polygon",
        "source-layer" = "2016_census_ct_polygon",
        type = "line",
        paint = list(
          "line-color" = "red",
          # "line-color" = list(
          #     "case",
          #     list("boolean", c("feature-state", "click"), FALSE), "blue",
          #     "red"
          #   ),
          "line-width" = 1.5,
          "line-opacity" = 0.5
        )
        # ,
        # layout = list(
        #   "visibility" = "none"
        # )
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
