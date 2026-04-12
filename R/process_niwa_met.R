dir <- here::here("data", "raw", "niwa_climate")
fils <- list.files(dir, full.names = TRUE, pattern = "\\.csv$")
niwa_climate <- purrr::map_dfr(fils, readr::read_csv, .id = "source_file")
