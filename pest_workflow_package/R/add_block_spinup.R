#' Add a block bootstrap spin-up period to a time series
#'
#' @param df Data frame containing a Date column and any number of other variables
#' @param n_years Number of years to prepend as spin-up
#' @param southern_hemisphere Logical. If TRUE, shifts seasons by 6 months.
#' @param seed Optional random seed for reproducibility
#'
#' @return Data frame with bootstrapped years prepended
#' @examples
#' spinup_timeseries(df, n_years = 5, southern_hemisphere = TRUE)
#'

# add_block_spinup <- function(df, n_years = 5, southern_hemisphere = FALSE, 
#                              seed = NULL) {
#   spinup_mapping <- generate_spinup_mapping(df, n_years, southern_hemisphere, seed)
#   new_df <- apply_spinup_mapping(df, spinup_mapping, southern_hemisphere)
#   return(new_df)
# }


generate_spinup_mapping <- function(df, n_years = 5, southern_hemisphere = TRUE,
                                    seed = NULL) {
  if (!"Date" %in% names(df)) stop("Data frame must contain a 'Date' column.")
  if (!inherits(df$Date, "Date")) stop("'Date' column must be of class Date.")
  if (!is.null(seed)) set.seed(seed)
  
  first_date <- min(df$Date)
  spinup_dates <- seq.Date(from = first_date - lubridate::years(n_years), 
                           to = first_date - lubridate::days(1), by = "day")
  spinup <- data.frame(Date = spinup_dates)
  
  # Apply hydrological year/day logic
  if (southern_hemisphere) {
    df <- df |> dplyr::mutate(
      hyd_year = lubridate::year(Date + lubridate::dmonths(6)),
      hyd_doy = lubridate::yday(Date + lubridate::dmonths(6))
    )
    spinup <- spinup |> dplyr::mutate(
      hyd_year = lubridate::year(Date + lubridate::dmonths(6)),
      hyd_doy = lubridate::yday(Date + lubridate::dmonths(6))
    )
  } else {
    df <- df |> dplyr::mutate(
      hyd_year = lubridate::year(Date),
      hyd_doy = lubridate::yday(Date)
    )
    spinup <- spinup |> dplyr::mutate(
      hyd_year = lubridate::year(Date),
      hyd_doy = lubridate::yday(Date)
    )
  }
  
  years <- sort(unique(df$hyd_year))
  complete_years <- years[sapply(years, function(y) {
    n_days <- ifelse(lubridate::leap_year(y), 366, 365)
    sum(df$hyd_year == y) >= n_days
  })]
  
  if (length(complete_years) == 0) stop("No complete years found in dataset.")
  
  new_years <- sort(unique(spinup$hyd_year))
  leap_years <- complete_years[lubridate::leap_year(complete_years)]
  non_leap_years <- complete_years[!lubridate::leap_year(complete_years)]
  
  # Record which historical year each new year came from
  year_map <- purrr::map_dfr(new_years, function(y) {
    sample_year <- if (lubridate::leap_year(y)) {
      sample(leap_years, 1)
    } else {
      sample(non_leap_years, 1)
    }
    data.frame(new_year = y, sample_year = sample_year)
  })
  
  list(spinup = spinup, year_map = year_map)
}
# spinup_mapping <- generate_spinup_mapping(df, n_years = 5, southern_hemisphere = TRUE,
                                   # seed = 42)

# df <- inp$meteo

apply_spinup_mapping <- function(df, spinup_mapping, southern_hemisphere = FALSE) {
  spinup <- spinup_mapping$spinup
  year_map <- spinup_mapping$year_map
  
  if (southern_hemisphere) {
    df <- df |> dplyr::mutate(
      hyd_year = lubridate::year(Date + lubridate::dmonths(6)),
      hyd_doy = lubridate::yday(Date + lubridate::dmonths(6))
    )
  } else {
    df <- df |> dplyr::mutate(
      hyd_year = lubridate::year(Date),
      hyd_doy = lubridate::yday(Date)
    )
  }
  
  spinup_df <- purrr::map_dfr(year_map$new_year, function(y) {
    sample_year <- year_map$sample_year[year_map$new_year == y]
    df_y <- df[df$hyd_year == sample_year, , drop = FALSE] |>
      dplyr::mutate(hyd_year = y) |>
      dplyr::select(-Date)
    df_y
  })
  
  sp <- spinup |> 
    dplyr::left_join(spinup_df, by = c("hyd_year", "hyd_doy"))
  
  new_df <- dplyr::bind_rows(sp, df) |>
    dplyr::arrange(Date) |>
    dplyr::select(-hyd_year, -hyd_doy)
  
  return(new_df)
}


