#' generate_pdf_report
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

generate_pdf_report <- function(file, ns, selected_geographies, input_aggregate_area, bookmark_query) {

  shinyjs::disable(ns("download_report"))
  shinyjs::runjs("document.getElementById('sidebar-download_report').innerText = 'Processing...';")

  # Move to tempdir to save files
  original_wd <- setwd(tempdir())

  # Go back to working directory after function
  on.exit(setwd(original_wd))

  temp_template <- "report.Rmd"
  file.copy(app_sys("report/style.css"), "style.css", overwrite = TRUE)
  file.copy(app_sys("report/report.Rmd"), temp_template, overwrite = TRUE)

  # Set up parameters to pass to Rmd document
  params <- list(
    geo_uid = selected_geographies()$geo_uid,
    geography = input_aggregate_area(),
    bookmark = bookmark_query()
  )

  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(temp_template,
                    output_file = "CensusAggregator Report.html",
                    params = params,
                    envir = new.env(parent = globalenv()),
                    quiet = TRUE
  )

  print_report(input = "CensusAggregator Report.html", output = file)

  shinyjs::runjs("document.getElementById('sidebar-download_report').innerText = 'Download PDF';")
  shinyjs::enable(ns("download_report"))
}

print_report <- function(input = "inst/report/report.html", output = "report.pdf") {
  pagedown::chrome_print(
    input,
    output = output,
    options = list(
      displayHeaderFooter = TRUE,
      footerTemplate = format(
        shiny::div(
          style = "width: 100%; font-size: 10pt; font-family: 'Lato'; float: right; text-align: right; padding-right: 2.1cm; padding-bottom: 0.5cm;",
          shiny::span(class = "pageNumber")
        ),
        indent = FALSE
      ),
      headerTemplate = format(shiny::div(), indent = FALSE),
      marginTop = 0.5,
      marginBottom = 0.75
    ),
    extra_args = chrome_extra_args(),
    verbose = FALSE
  )
}

# Via: https://github.com/RLesur/chrome_print_shiny
#' Return Chrome CLI arguments
#'
#' This is a helper function which returns arguments to be passed to Chrome. This function includes Chrome arguments for running on Shinyapps or just for when you need them in general - e.g. we are running this app in a Docker container, but not on shinyapps
#'
#' @param default_args Arguments to be used in any circumstances.
#'
#' @return A character vector with CLI arguments to be passed to Chrome.
#' @noRd
chrome_extra_args <- function(default_args = c("--disable-gpu")) {
  args <- c(
    default_args,
    "--no-sandbox", # required because we are in a container
    "--disable-dev-shm-usage" # in case of low available memory
  )
  args
}

print_report <- function(input = "inst/report/report.html", output = "report.pdf") {
  pagedown::chrome_print(
    input,
    output = output,
    options = list(
      displayHeaderFooter = TRUE,
      footerTemplate = format(
        shiny::div(
          style = "width: 100%; font-size: 10pt; font-family: 'Lato'; float: right; text-align: right; padding-right: 2.1cm; padding-bottom: 0.5cm;",
          shiny::span(class = "pageNumber")
        ),
        indent = FALSE
      ),
      headerTemplate = format(shiny::div(), indent = FALSE),
      marginTop = 0.5,
      marginBottom = 0.75
    ),
    extra_args = chrome_extra_args(c("--headless", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage")),
    verbose = FALSE
  )
}
