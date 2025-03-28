library(tidyverse)

extract_municipalities <- read_csv(here::here("data-raw", "canadian_municipalities.csv"))

municipalities <- extract_municipalities |>
  janitor::clean_names() |>
  arrange(city_province)

usethis::use_data(municipalities, overwrite = TRUE)

