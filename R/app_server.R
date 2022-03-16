#' App server
#'
#' @param input, output, session Internal parameters for {shiny}.
#' (do not remove)
#' @noRd
app_server <- function(input, output, session) {

  mod_map_server("map")
  mod_sidebar_server("sidebar")

}
