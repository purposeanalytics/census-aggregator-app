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
mod_map_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- mapboxer::renderMapboxer(
      map_ct()
    )
  })
}