#' App user interface
#'
#' @param request Internal parameter for `{shiny}`.
#' (do not remove)
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    # shiny::div(
    # style = "display: flex;",
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 4),
      shiny::fluidRow(
        shiny::div(
          class = "col-sm-8 censusagg-col",
          mod_map_ui("map")
        ),
        shiny::div(
          class = "col-sm-4 censusagg-col",
          mod_sidebar_ui("sidebar")
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
      app_title = "Census Aggregator"
    ),
    shinyjs::useShinyjs(),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
