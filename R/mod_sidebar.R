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
      shiny::textOutput(ns("population")),
      shiny::textOutput(ns("households")),
      shiny::textOutput(ns("area_sq_km")),
      shiny::textOutput(ns("population_density"))
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

    shiny::observeEvent(selected_geographies(), ignoreInit = TRUE, {

      shiny::req(nrow(selected_geographies()) > 0)
      summary_statistics <- selected_geographies() %>%
        dplyr::select(population, households, area_sq_km) %>%
        dplyr::summarise_all(sum) %>%
        dplyr::mutate(population_density = round(population / area_sq_km))

      output$population <- shiny::renderText(
        glue::glue("Total population: {population}",
          population = scales::comma(summary_statistics[["population"]])
        )
      )

      output$households <- shiny::renderText(
        glue::glue("Total households: {households}",
          households = scales::comma(summary_statistics[["households"]])
        )
      )

      output$area_sq_km <- shiny::renderText(
        glue::glue("Total area: {area_sq_km} square kilometres",
          area_sq_km = scales::comma(summary_statistics[["area_sq_km"]], accuracy = 0.01)
        )
      )

      output$population_density <- shiny::renderText(
        glue::glue("Average population density: {population_density} people per square kilometre",
          population_density = scales::comma(summary_statistics[["population_density"]])
        )
      )
    })

    return(inputs)
  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
