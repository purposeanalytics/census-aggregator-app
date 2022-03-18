#' App server
#'
#' @param input, output, session Internal parameters for {shiny}.
#' (do not remove)
#' @noRd
app_server <- function(input, output, session) {

  inputs <- mod_sidebar_server("sidebar")
  mod_map_server("map", inputs)
}
