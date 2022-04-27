#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "width: 70%; padding-right: 30px;",
    mapboxer::mapboxerOutput(ns("map"), height = "800px")
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, inputs, selected_geographies) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- mapboxer::renderMapboxer(
      map()
    )

    # Update map based on inputs ----
    shiny::reactive({
      switch(inputs()[["aggregate_area"]],
        csd = mapboxer::mapboxer_proxy(ns("map")) %>%
          toggle_layer_visible("csd") %>%
          toggle_layer_invsibile("ct"),
        ct = mapboxer::mapboxer_proxy(ns("map")) %>%
          toggle_layer_visible("ct") %>%
          toggle_layer_invsibile("csd")
      ) %>%
        mapboxer::update_mapboxer()
    })

    # Keep track of geographies that are clicked ----
    shiny::observeEvent(
      input$map_onclick,
      {
        # Set current value of selected_geographies to be existing tibble, plus new geographies
        selected_geographies(
          selected_geographies() %>%
            dplyr::bind_rows(input$map_onclick$props %>% dplyr::as_tibble())
        )
      }
    )
  })
}
