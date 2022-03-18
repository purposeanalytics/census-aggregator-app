#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  shiny::div(
    style = "width: 30%",
    shinyWidgets::prettyRadioButtons(
      ns("aggregate_area"),
      "Aggregate area",
      choices = list(
        "Census Subdivisions" = "csd",
        "Census Tracts" = "ct"
      )
    ),
    shinyWidgets::prettyRadioButtons(
      ns("selection_tool"),
      "Choose selection tool",
      choices = list(
        "Click to select geographies" = "click",
        "Draw a polygon" = "polygon"
      )
    ),
    shiny::div(
      shinyWidgets::actionBttn(
        ns("export_data"),
        "Export data"
      )
    ),
    shiny::div(
      shinyWidgets::actionBttn(
        ns("export_geography"),
        "Export boundary",
        style = "minimal",
        color = "danger"
      )
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update `aggregate_area` reactive value with input

    inputs <- shiny::reactive({
      list(
        aggregate_area = input$aggregate_area
      )
    })

    return(inputs)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
