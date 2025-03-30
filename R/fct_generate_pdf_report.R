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

  w <- waiter::Waiter$new(html = tagList(waiter::spin_4(), h4("Preparing report...")), color = "rgba(0,0,0,0.5)")
  w$show()  # Show the waiter spinner

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


  # Update waiter message
  w$update(html = tagList(waiter::spin_4(), h4("Saving as PDF...")))


  print_report(input = "CensusAggregator Report.html", output = file)

  w$hide()  # Hide the waiter spinner

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
    extra_args = c("--headless", "--disable-gpu", "--no-sandbox", "--disable-dev-shm-usage"),
    verbose = FALSE
  )
}
