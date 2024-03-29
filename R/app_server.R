#' App server
#'
#' @param input, output, session Internal parameters for {shiny}.
#' (do not remove)
#' @noRd
app_server <- function(input, output, session) {
  selected_geographies <- shiny::reactiveVal(dplyr::tibble())

  # Initialize that the map has not been rendered
  map_rendered <- shiny::reactiveVal(FALSE)

  # Set it to TRUE once it has
  shiny::observeEvent(input$map_rendered,
    priority = 100,
    {
      map_rendered(TRUE)
    }
  )

  # Initialize that the bookmarks need to be parsed - will set to FALSE once they have been, so don't repeat it
  boomarks_to_be_parsed <- shiny::reactiveVal(TRUE)
  bookmark_bounds <- shiny::reactiveVal() # Initializing empty bookmark bounds to be set

  input_aggregate_area <- shiny::reactiveVal("csd")
  input_selection_tool <- shiny::reactiveVal("click")
  mod_sidebar_server("sidebar", input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, boomarks_to_be_parsed, bookmark_bounds)

  # Observe geographies selected via polygon ----

  # Keep track of geographies that are selected via polygon ----
  shiny::observeEvent(
    input$ct_polygon_filter, # Set in JS
    ignoreNULL = FALSE,
    {
      if (input_aggregate_area() == "ct") {
        if (all(input$ct_polygon_filter == "") | is.null(input$ct_polygon_filter)) {
          selected_geographies(
            tibble::tibble()
          )
        } else {
          selected_geographies(
            tibble::tibble(geo_uid = unique(input$ct_polygon_filter))
          )
        }
      }
    }
  )

  shiny::observeEvent(
    input$csd_polygon_filter, # Set in JS
    ignoreNULL = FALSE,
    {
      if (input_aggregate_area() == "csd") {
        if (all(input$csd_polygon_filter == "" | is.null(input$csd_polygon_filter))) {
          selected_geographies(
            tibble::tibble()
          )
        } else {
          selected_geographies(
            tibble::tibble(geo_uid = unique(input$csd_polygon_filter))
          )
        }
      }
    }
  )

  mod_map_server(
    "map", input_aggregate_area, input_selection_tool, selected_geographies, map_rendered, bookmark_bounds
  )
}
