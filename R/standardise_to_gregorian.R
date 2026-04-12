standardise_to_gregorian <- function(
    data,
    date_col = "date",
    vars = c("hurs", "pr", "rsds", "sfcWind", "tas"),
    metadata
) {
  
  gcm <- data$gcm[1]
  scenario <- data$scenario[1]
  
  data_wide <- data |> 
    dplyr::filter(gcm == !!gcm,
                  scenario == !!scenario) |> 
    dplyr::select(dplyr::all_of(c(date_col, "variable", "value"))) |> 
    tidyr::pivot_wider(names_from = variable, values_from = value)

  calendar <- metadata |> 
    dplyr::filter(gcm == !!gcm,
                  !is.na(calendar),
                  # variable %in% !!cmip_vars,
                  scenario == !!scenario) |> 
    dplyr::pull(calendar) |> 
    unique()
  
  # calendar <- match.arg(calendar)
  
  stopifnot(date_col %in% names(data_wide))
  stopifnot(all(vars %in% names(data_wide)))
  
  data_wide[[date_col]] <- as.Date(data_wide[[date_col]])
  data_wide <- data_wide[order(data_wide[[date_col]]), ]
  # data_wide <- data_wide |> 
  #   dplyr::mutate(
  #     gcm = gcm,
  #     scenario = scenario
  #   )
  
  if (calendar == "gregorian") {
    return(data_wide)
  }
  
  # ----------------------------
  # 365_day calendar
  # ----------------------------
  if (calendar == "365_day") {
    
    full_dates <- data.frame(
      date = seq(min(data_wide[[date_col]]),
                 max(data_wide[[date_col]]),
                 by = "day")
    )
    
    names(full_dates)[1] <- date_col
    
    out <- merge(full_dates, data_wide, by = date_col, all.x = TRUE)
    
    is_feb29 <- format(out[[date_col]], "%m-%d") == "02-29"
    
    for (v in vars) {
      idx <- which(is_feb29 & is.na(out[[v]]))
      for (i in idx) {
        out[[v]][i] <-
          mean(c(out[[v]][i - 1], out[[v]][i + 1]), na.rm = TRUE)
      }
    }
    
    return(out)
  }
  
  # ----------------------------
  # 360_day calendar
  # ----------------------------
  if (calendar == "360_day") {
    
    data_wide$year <- format(data_wide[[date_col]], "%Y")
    
    out_list <- lapply(split(data_wide, data_wide$year), function(df_year) {
      
      yr <- unique(df_year$year)
      yr_num <- as.numeric(yr)
      
      # Determine if Gregorian year is leap
      is_leap <- (yr_num %% 4 == 0 & yr_num %% 100 != 0) |
        (yr_num %% 400 == 0)
      
      n_target <- ifelse(is_leap, 366, 365)
      
      # Source index: 1–360
      source_day <- seq_len(nrow(df_year))
      
      # Target index: 1–365/366
      target_day <- seq_len(n_target)
      
      # Scale 360 → 365/366
      scaled_source <- seq(
        from = 1,
        to = 360,
        length.out = n_target
      )
      
      new_dates <- seq(
        from = as.Date(paste0(yr, "-01-01")),
        length.out = n_target,
        by = "day"
      )
      
      new_df <- data.frame(date = new_dates)
      names(new_df)[1] <- date_col
      
      for (v in vars) {
        new_df[[v]] <- approx(
          x = source_day,
          y = df_year[[v]],
          xout = scaled_source,
          rule = 2
        )$y
      }
      
      new_df
    })
    
    out <- do.call(rbind, out_list)
    rownames(out) <- NULL
    return(out)
  }
}
