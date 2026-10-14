read_bop_csv_url <- function(url, skip = 5) {
  res <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  
  dat <- readr::read_csv(
    I(res),
    skip = skip,
    show_col_types = FALSE
  ) |> 
    dplyr::mutate(
      `Timestamp (UTC+12:00)` = lubridate::as_datetime(`Timestamp (UTC+12:00)`, tz = "Etc/GMT-12")
    )
  
  return(dat)
}
