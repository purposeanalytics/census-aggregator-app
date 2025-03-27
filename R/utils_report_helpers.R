#' report-helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
title_heading <- function(title) {
  div(
    div(
      class = "title-heading",
      h1(title),
      hr()
    )
  )
}
section_heading <- function(title) {
  div(
    hr(),
    div(
      class = "section-heading",
      h2(title),
      hr()
    )
  )
}

subsection_heading <- function(title, class = NULL) {
  div(
    class = "subsection-heading",
    h3(title)
  )
}

snapshot_card_value <- function(value) {
  div(
    class = "card-value",
    value
  )
}

snapshot_card <- function(title, value, ..., class = NULL) {
  div(
    class = paste("box", class),
    div(HTML(title)),
    snapshot_card_value(value),
    div(...)
  )
}

header_and_barchart <- function(...) {
  column(
    width = 6,
    ...
  )
}

pull_and_format_value <- function(data, format = "percent", column = "value") {
  value <- data[[column]]

  if (is.na(value)) {
    return(HTML("&#8212;"))
  }

  switch(format,
         percent = scales::percent(value, 0.1),
         comma = scales::comma(value, 1),
         comma_decimal = scales::comma(value, 0.1),
         dollar = scales::dollar(value)
  )
}

logo <- function(pagebreak = TRUE) {
  div(
    class = "print-header",
    div(
      class = "left-logo",
      img(
        src = system.file("app/www/logo.png", package = "censusaggregatorapp"),
        alt = "CensusAggregator logo"
      )
    ),
    div(
      class = "right-logo",
      img(
        src = system.file("app/www/pa-full-logo.png", package = "censusaggregatorapp"),
        alt = "Purpose Analytics logo"
      )
    )
  )
}

suppressed_note <- function(...) {
  data <- list(...)

  if (any(data == "&#8212;")) {
    div(
      HTML("<i>Note: &#8212; indicates data for one or more of the selected areas is not available or is suppressed due to confidentiality.</i>")
    )
  }
}
