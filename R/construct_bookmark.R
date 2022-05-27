construct_bookmark <- function(input, session, exclude = NULL, selected_geographies) {

  # Get names of inputs for bookmarking
  inputs <- names(input)
  # Exclude any supplied
  inputs <- setdiff(inputs, exclude)
  inputs <- setdiff(inputs, "bookmark")

  # Prep input values for including in a URL, and collapse multiple using ,
  bookmark_inputs <- purrr::map(inputs, function(x) {
    input[[x]] %>%
      curl::curl_escape() %>%
      paste0(collapse = ",")
  })
  names(bookmark_inputs) <- inputs

  # Set up geo_uid bookmarking
  if (nrow(selected_geographies) != 0) {
    bookmark_geo_uid <- selected_geographies[["geo_uid"]] %>%
      paste0(collapse = ",")
    names(bookmark_geo_uid) <- "geo_uid"
  } else {
    bookmark_geo_uid <- NULL
  }

  # Combine inputs and geo_uid bookmarking
  bookmark <- append(bookmark_inputs, bookmark_geo_uid)

  bookmark_names <- names(bookmark)
  bookmark_values <- unname(unlist(bookmark))

  # Create the query string
  url_query <- paste0(bookmark_names, "=", bookmark_values, collapse = "&")

  # Parse the rest of the URL data, then combine with the query string for serving back up
  # Much of this is taken directly from how it's done in Shiny: https://github.com/rstudio/shiny/blob/master/R/shiny.R#L1671
  clientData <- session$clientData

  paste0(
    clientData$url_protocol, "//",
    clientData$url_hostname,
    ifelse(nzchar(clientData$url_port), paste0(":", clientData$url_port), ""),
    clientData$url_pathname,
    "?", url_query
  )
}

# Split string containing , into multiple elements
split_query <- function(query, sep = ",") {
  purrr::map(query, ~ strsplit(.x, sep)[[1]])
}
