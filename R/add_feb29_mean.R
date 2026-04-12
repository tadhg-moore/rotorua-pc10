add_feb29_mean <- function(df) {
  
  df <- df |>
    dplyr::mutate(Date = as.Date(Date))
  
  out <- df |>
    dplyr::mutate(year = lubridate::year(Date)) |>
    dplyr::group_by(year) |>
    dplyr::group_modify(function(.x, .y) {
      
      yr <- .y$year
      
      if (lubridate::leap_year(yr)) {
        
        feb28 <- .x |>
          dplyr::filter(
            lubridate::month(Date) == 2,
            lubridate::day(Date) == 28
          )
        
        mar01 <- .x |>
          dplyr::filter(
            lubridate::month(Date) == 3,
            lubridate::day(Date) == 1
          )
        
        if (nrow(feb28) == 1 && nrow(mar01) == 1) {
          
          feb29 <- feb28
          feb29$Date <- as.Date(paste0(yr, "-02-29"))
          
          met_vars <- setdiff(names(df), "Date")
          
          feb29[, met_vars] <-
            (feb28[, met_vars] + mar01[, met_vars]) / 2
          
          feb29$MET_pprain <- 0
          
          .x <- dplyr::bind_rows(.x, feb29)
        }
      }
      
      .x
    }) |>
    dplyr::ungroup() |>
    dplyr::arrange(Date) |>
    dplyr::select(-year)
  
  return(out)
}
