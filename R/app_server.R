#' App server
#'
#' @param input, output, session Internal parameters for {shiny}.
#' (do not remove)
#' @noRd
app_server <- function(input, output, session) {
  selected_geographies <- shiny::reactiveVal(dplyr::tibble())

  # Initialize that the map has not been rendered
  map_rendered <- shiny::reactiveVal(FALSE)

  # Set it to TRUE once it has
  shiny::observeEvent(input$map_rendered,
    priority = 100,
    {
      map_rendered(TRUE)
    }
  )

  # Initialize that the bookmarks need to be parsed - will set to FALSE once they have been, so don't repeat it
  boomarks_to_be_parsed <- shiny::reactiveVal(TRUE)

  inputs <- mod_sidebar_server("sidebar", selected_geographies, map_rendered, boomarks_to_be_parsed)

  # Observe geographies selected via polygon
  polygon_filter <- shiny::reactiveVal()
  shiny::observeEvent(input$polygon_filter, {
    polygon_filter(input$polygon_filter)
  })

  mod_map_server(
    "map", inputs, selected_geographies, map_rendered,
    polygon_filter
  )
}
