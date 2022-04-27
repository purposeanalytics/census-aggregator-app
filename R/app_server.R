#' App server
#'
#' @param input, output, session Internal parameters for {shiny}.
#' (do not remove)
#' @noRd
app_server <- function(input, output, session) {
  selected_geographies <- shiny::reactiveVal(dplyr::tibble())

  inputs <- mod_sidebar_server("sidebar", selected_geographies)
  mod_map_server("map", inputs, selected_geographies)
}
