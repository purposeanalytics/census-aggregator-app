library(readxl)
library(dplyr)
library(stringr)

variable_definitions <- read_excel(here::here("data-raw", "variable_definitions.xlsx"))

variable_definitions <- variable_definitions %>%
  mutate(variable_definition = glue::glue("<b>{Variable}</b>: {Definition}"),
         variable_definition = str_squish(variable_definition)) %>%
  select(-Variable, -Definition)

usethis::use_data(variable_definitions)
