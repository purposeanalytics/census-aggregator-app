#' App user interface
#'
#' @param request Internal parameter for `{shiny}`.
#' (do not remove)
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    shiny::fluidPage(
      shinyWidgets::prettyRadioButtons(
        "aggregate_geography",
        "Aggregate area",
        choices = list(
          "Census Subdivisions" = "csd",
          "Census Tracts" = "ct"
        )
      ),
      shinyWidgets::prettyRadioButtons(
        "selection_tool",
        "Choose selection tool",
        choices = list(
          "Click to select geographies" = "click",
          "Draw a polygon" = "polygon"
        )
      ),
      shiny::div(
        shinyWidgets::actionBttn(
          "export_data",
          "Export data"
        )
      ),
      shiny::div(
        shinyWidgets::actionBttn(
          "export_geography",
          "Export boundary",
          style = "minimal",
          color = "danger"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "censusaggregationapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
