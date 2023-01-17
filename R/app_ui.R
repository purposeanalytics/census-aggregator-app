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
    shiny::tags$link(rel = "icon", type = "png", href = "https://purposeanalytics.ca/user/themes/purpose-analytics/img/favicon.png"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "CensusAggregator"
    ),
    shinyjs::useShinyjs(),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip(),
    shiny::tags$head(shiny::HTML("
      <!-- Google tag (gtag.js) -->
      <script async src='https://www.googletagmanager.com/gtag/js?id=G-JFNPH9EW2R'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());

        gtag('config', 'G-JFNPH9EW2R');
      </script>
    ")),
    shiny::tags$head(HTML("
    <meta name='description' content='CensusAggregator makes it easier to aggregate and retrieve common census variables for custom regions that span multiple census geographic areas.'>

    <!-- Facebook Meta Tags -->
    <meta property='og:url' content='https://censusaggregator.ca'>
    <meta property='og:type' content='website'>
    <meta property='og:title' content='CensusAggregator'>
    <meta property='og:description' content='CensusAggregator makes it easier to aggregate and retrieve common census variables for custom regions that span multiple census geographic areas.'>
    <meta property='og:image' content='https://purposeanalytics.ca/blog/introducing-censusaggregator/censusaggregator.gif'>

    <!-- Twitter Meta Tags -->
    <meta name='twitter:card' content='summary_large_image'>
    <meta property='twitter:domain' content='censusaggregator.ca'>
    <meta property='twitter:url' content='https://censusaggregator.ca'>
    <meta name='twitter:title' content='CensusAggregator'>
    <meta name='twitter:description' content='CensusAggregator makes it easier to aggregate and retrieve common census variables for custom regions that span multiple census geographic areas.'>
    <meta name='twitter:image' content='https://purposeanalytics.ca/blog/introducing-censusaggregator/censusaggregator.gif'>
    "))
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
