rm(list = ls())
devtools::load_all()
load("R/sysdata.rda")

PARAMETERS <- data.frame(
  readxl::read_excel("data-raw/PARAMETERS.xlsx"),
  stringsAsFactors = FALSE
)

RECORDS <- data.frame(
  readxl::read_excel("data-raw/RECORDS.xlsx"),
  stringsAsFactors = FALSE
)

devtools::use_data(
  modes, PARAMETERS, RECORDS,
  internal = TRUE, overwrite = TRUE
)
