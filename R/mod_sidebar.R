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
    ),
    shiny::div(
      shiny::tableOutput(ns("summary_statistics"))
    )
  )
}

#' sidebar Server Functions
#'
#' @noRd
mod_sidebar_server <- function(id, selected_geographies) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update inputs with aggregate_area

    inputs <- shiny::reactive({
      list(
        aggregate_area = input$aggregate_area
      )
    })

    shiny::observeEvent(selected_geographies(), ignoreInit = FALSE, {

      if (nrow(selected_geographies()) == 0) {
        summary_statistics <- dplyr::tibble(name = c("population", "households", "area_sq_km", "population_density")) %>%
          dplyr::mutate(value = "---")
      } else {

      summary_statistics <- selected_geographies() %>%
        dplyr::select(population, households, area_sq_km) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::mutate(population_density = round(population / area_sq_km)) %>%
        tidyr::pivot_longer(dplyr::everything())
      }

      output$summary_statistics <- shiny::renderTable(summary_statistics)
    })

    return(inputs)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
