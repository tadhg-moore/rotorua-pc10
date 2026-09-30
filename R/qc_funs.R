# Function to calculate circular average
avg_circ <- function(x) {
  as.numeric(mean(circular::circular(x[is.finite(x)], units = "degrees",
                                     modulo = '2pi')))
}

# Function to standardise timestamps to user defined minute and aggregate
standardise <- function(data, minute = 15, FUN = mean, ...) {
  data |> 
    dplyr::mutate(datetime = lubridate::round_date(datetime, 
                                                   paste0(minute, 
                                                          " minutes"))) |> 
    dtplyr::lazy_dt() |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   function(x) suppressWarnings(FUN(x, ...)))) |> 
    tibble::as_tibble()
  
}

# qc_code_colour_scale
qc_code_col_scale = c(
  "QC 100" = "#FF0000",
  "QC 200" = "#8B5A00",
  "QC 300" = "#D3D3D3",
  "QC 400" = "#FFA500",
  "QC 500" = "#00BFFF",
  "QC 600" = "#006400"
)

subset_data <- function(data, sub) {
  if (sub > 1) {
    data <- data |> 
      # dplyr::slice(seq(1, nrow(data), sub))
      # Sequence from 1 to nrow(data) by sub
      dplyr::mutate(index = rep(1:ceiling(dplyr::n() / sub), each = sub,
                                length.out = dplyr::n())) |> 
      dtplyr::lazy_dt() |>
      dplyr::group_by(index) |>
      # Calculate min, max and median for each group but just the mean for datetime
      dplyr::summarise(dplyr::across(dplyr::everything(), 
                                     list(min = ~suppressWarnings(min(., na.rm = TRUE)), 
                                          max = ~suppressWarnings(max(., na.rm = TRUE)), 
                                          median = ~suppressWarnings(median(., na.rm = TRUE)))),
                       datetime = list(mean = mean(datetime))) |>
      tibble::as_tibble() |> 
      dplyr::mutate(datetime = datetime_median) |> 
      # Remove all other datetime cols
      dplyr::select(-starts_with("datetime_")) |>
      tidyr::pivot_longer(cols = -c(index, datetime), names_to = "variable", 
                          values_to = "value") |>
      dplyr::filter(!is.na(value)) |>
      # Extract variable names from the variable column without min/max/median
      dplyr::mutate(variable = factor(gsub("(.*)_.*", "\\1", variable))) 
  } else {
    data <- data |> 
      tidyr::pivot_longer(cols = -datetime, names_to = "variable", 
                          values_to = "value") |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(variable = factor(gsub("(.*)_.*", "\\1", variable)))
  }
  return(data)
}

plot_sensor <- function(data, var_ref_id, date_range, variable_ref,
                        sensor_calibrations = NULL,
                        sensor_reference = NULL, sensor_scaling = NULL,
                        site_events = NULL, clip_ylim = TRUE, buffer = 0, 
                        sub = 10, colour = "device_id") {
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    x0 <- date_range[1]
    x1 <- date_range[2]
    date_range <- coerce_date_range(date_range, tzone = tzone, buffer = buffer)
  }
  
  
  sel_vars <- var_ref_id
  
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars)
  devices <- df |> 
    dplyr::select(device_id, var_ref_id) |>
    dplyr::distinct()
  
  sensor_calibs <- data.frame()
  if (!is.null(sensor_calibrations)) {
    sensor_calibs <- sensor_calibrations |> 
      dplyr::filter(device_id %in% df$device_id) |> 
      dplyr::left_join(devices, by = "device_id")
    
  }
  
  sensor_refs <- data.frame()
  if (!is.null(sensor_reference)) {
    sensor_refs <- sensor_reference |> 
      dplyr::filter(device_id %in% df$device_id) |> 
      dplyr::left_join(devices, by = "device_id")
    
    # ggplot() +
    #   geom_point(data = sensor_refs, aes((date), value_measured, colour = factor(value_actual))) +
    #   geom_smooth(data = sensor_refs, aes((date), value_measured, colour = factor(value_actual)), method = "lm") +
    #   # scale_colour_viridis_d() +
    #   # facet_wrap(value_actual~units_sensor, scales = "free")
    #   theme_bw()
  }
  
  sensor_scales <- data.frame()
  if (!is.null(sensor_scaling)) {
    sensor_scales <- sensor_scaling |> 
      dplyr::filter(device_id %in% df$device_id) |> 
      dplyr::left_join(devices, by = "device_id")
    
    if (nrow(sensor_scales) > 0) {
      # df <- scale_data(df, sensor_scales)
    }
  }
  
  site_visits <- data.frame()
  if (!is.null(site_events)) {
    site_visits <- site_events |> 
      dplyr::filter(site_visit)
  }
  
  # df <- subset_data(df, sub = sub)
  
  title_name <- data.frame(
    var_ref_id = sel_vars,
    title = make_var_name(var_ref_id, variable_ref = variable_ref)
  )
  
  df <- df |> 
    dplyr::left_join(title_name, by = "var_ref_id") 
  
  p <- ggplot() +
    geom_scattermore(data = df, aes(x = datetime, y = qc_value, 
                                    colour = !!sym(colour)), pointsize = 3,
                     pixels = c(2048, 512)) +
    facet_wrap(~title, scales = "free_y", ncol = 1) +
    labs(linetype = "Sensor events", y = "QC Value") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  if (colour == "qc_code") {
    p <- p + 
      labs(colour = "QC Code") +
      ggplot2::scale_colour_manual(values = qc_code_col_scale) 
  } else if (colour == "device_id") {
    p <- p +
      labs(colour = "Device ID") +
      ggplot2::scale_color_brewer(palette = "Paired") 
  }
  
  if (clip_ylim) {
    p <- p + 
      coord_cartesian(ylim = quantile(df$qc_value, c(0.01, 0.99), na.rm = TRUE))
  }
  
  if (nrow(sensor_calibs) > 0) {
    p <- p +
      geom_vline(data = sensor_calibs, aes(xintercept = date,
                                           linetype = "Calibraton"))
  }
  
  if (nrow(sensor_refs) > 0) {
    p <- p +
      geom_vline(data = sensor_refs, aes(xintercept = date,
                                         linetype = "Reference"))
  }
  
  if (nrow(sensor_scales) > 0) {
    p <- p +
      geom_vline(data = sensor_scales, aes(xintercept = date, 
                                           linetype = "Scaling"))
  }
  
  if (nrow(site_visits) > 0) {
    p <- p +
      geom_vline(data = site_visits, aes(xintercept = date, colour = action,
                                         linetype = "Site visit"))
  }
  
  return(p)
}

# Function to build date_interval
build_date_interval <- function(data) {
  
  if (!"date" %in% colnames(data)) {
    stop("data must contain a column named 'date'")
  }
  tzone <- lubridate::tz(data$date)
  
  data <- data |> 
    # Add a column which is date shifted up 1
    dplyr::rename(date_from = date) |> 
    dplyr::mutate(
      date_to = dplyr::lead(date_from, default = max(date_from)),
      date_interval = lubridate::interval(date_from, date_to, tzone = tzone)
    ) 
}

# Function to generate column name
generate_var_ref <- function(var, z_relative, reference = "d") {
  ref_str <- paste0(reference, z_relative * 100)  # Convert meters to cm for d, h, or e
  paste(var, ref_str, sep = "_")
}

# Function to decode column name and return a tibble
decode_var_ref <- function(var_ref, variable_ref = NULL) {
  lapply(var_ref, \(v) {
    parts <- strsplit(v, "_")[[1]]
    # Check if the last part is a reference (d, h, or e)
    if (grepl("z", parts[length(parts)])) {
      parts <- parts[1:(length(parts) - 1)]
    }
    reference <- substr(parts[length(parts)], 1, 1)  # Get the reference (d, h, or e)
    value_cm <- as.numeric(sub("[a-z]", "", parts[length(parts)]))  # Remove reference and get numeric value
    variable <- paste(parts[1:(length(parts) - 1)], collapse = "_")
    if (!is.null(variable_ref)) {
      sel_var <- variable
      variable <- variable_ref |> 
        dplyr::filter(abbr == sel_var) |> 
        dplyr::pull(label)
    }
    
    data.frame(
      var_abbr = variable,
      reference = reference,
      value_m = value_cm / 100  # Convert cm back to meters
    )
  }) |> 
    dplyr::bind_rows()
}

make_var_name <- function(var_ref, variable_ref) {
  name <- tryCatch({
    decode_var_ref(var_ref = var_ref, variable_ref = variable_ref) |> 
      dplyr::mutate(
        pos = dplyr::case_when(
          reference == "d" ~ "depth",
          reference == "h" ~ "height",
          reference == "e" ~ "elevation"
        ),
        var_name = paste0(var_abbr, " at ", value_m, "m ", pos)
      ) |> 
      dplyr::pull(var_name)
  }, error= function(e) {
    message("Error in make_var_name: ", e$message, "\nReturning var_ref")
    return(var_ref)
  })
}

# Function to decode depth from column name
decode_depth <- function(var_ref) {
  sapply(var_ref, \(v) {
    parts <- strsplit(v, "_")[[1]]
    reference <- substr(parts[length(parts)], 1, 1)  # Get the reference (d, h, or e)
    value_cm <- as.numeric(sub("[a-z]", "", parts[length(parts)]))  # Remove reference and get numeric value
    # tibble::tibble(
    value_m = value_cm / 100  # Convert cm back to meters
    # )
    return(value_m)
  })
}

# Function to decode reference from column name
decode_reference <- function(var_ref) {
  sapply(var_ref, \(v) {
    parts <- strsplit(v, "_")[[1]]
    reference <- substr(parts[length(parts)], 1, 1)  # Get the reference (d, h, or e)
    return(reference)
  })
}

# Function to generate QC filters data frame
get_filters <- function(filters, site) {
  
  site_sel <- site
  
  default_filters <- filters |> 
    dplyr::filter(site == "default") |> 
    dplyr::select(-site)
  
  if (!site_sel %in% filters$site) {
    return(default_filters)
  }
  
  # Subset site speific filters
  site_filters <- filters |> 
    dplyr::filter(site == site_sel)|> 
    dplyr::select(-site)
  
  upd_filters <- default_filters |> 
    dplyr::filter(!var_abbr %in% site_filters$var_abbr) |>
    dplyr::bind_rows(site_filters)
  
  return(upd_filters)
}

# Function to format qc_flag
format_flag <- function(qc_flag, new_flag) {
  dplyr::if_else(qc_flag == "" | is.na(qc_flag), new_flag, paste0(qc_flag, "|", new_flag))
}

# Function to apply filters to data
apply_filters <- function(data, filters) {
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  # Check all variables in data are in filters
  if (!all(data$var_abbr %in% filters$var_abbr)) {
    # Error and return variables not in filters
    stop("Variables not in filters: ", paste(setdiff(data$var_abbr, filters$var_abbr), collapse = ", "))
  }
  
  data <- data |> 
    dplyr::left_join(filters, by = c("var_abbr" = "var_abbr")) |>
    # low/high filter
    dplyr::mutate(
      # qc_value = value,
      qc_flag = dplyr::case_when(
        qc_value < low ~ format_flag(qc_flag, "low"),
        qc_value > high ~ format_flag(qc_flag, "high"),
        TRUE ~ qc_flag
      ),
      qc_value = dplyr::case_when(
        grepl("low", qc_flag) ~ NA_real_,
        grepl("high", qc_flag) ~ NA_real_,
        TRUE ~ qc_value
      ),
      qc_code = dplyr::case_when(
        grepl("low", qc_flag) ~ "QC 200",
        grepl("high", qc_flag) ~ "QC 200",
        TRUE ~ qc_code
      )
    ) |> 
    dplyr::arrange(datetime) |> 
    dplyr::group_by(var_ref_id) |> 
    # roc filter
    dplyr::mutate(value_roc = c(NA, abs(diff(qc_value, lag = 1, 
                                             na.pad = TRUE)))) |> 
    dplyr::mutate(
      qc_flag = dplyr::case_when(
        value_roc > roc ~ format_flag(qc_flag, "roc"),
        TRUE ~ qc_flag
      ),
      qc_value = dplyr::case_when(
        grepl("roc", qc_flag) ~ NA_real_,
        TRUE ~ qc_value
      ),
      qc_code = dplyr::case_when(
        grepl("roc", qc_flag) ~ "QC 200",
        TRUE ~ qc_code
      )
    ) |> 
    # consec filter
    dplyr::mutate(
      # Create a temporary clean version of 'qc_value' that fills NAs with the previous valid qc_value
      value_clean = ifelse(is.na(qc_value), 1e11, qc_value),
      lag_value = lag(value_clean, default = first(value_clean)),
      # Create a group ID based on consecutive repeating values
      grp = cumsum(value_clean != lag_value),
      
      # Calculate the length of each consecutive sequence
      seq_length = ave(value_clean, grp, FUN = length),
      
      qc_flag = dplyr::case_when(
        seq_length > consec ~ format_flag(qc_flag, "consec"),
        TRUE ~ qc_flag
      ),
      qc_value = dplyr::case_when(
        grepl("consec", qc_flag) ~ NA_real_,
        TRUE ~ qc_value
      ),
      qc_code = dplyr::case_when(
        grepl("consec", qc_flag) ~ "QC 200",
        TRUE ~ qc_code
      )
    ) |> 
    dplyr::ungroup() |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, raw_value,
                  qc_value, qc_flag, qc_code) 
  
  
  # Print a summary of flags per var_abbr
  p <- plot_flag_summary(data)
  print(p)
  
  return(data)
}

# Function for plotting qc_flag summary
plot_flag_summary <- function(data) {
  
  # Check if var_ref_id and qc_flag columns exist
  if (!all(c("var_ref_id", "qc_flag") %in% colnames(data))) {
    stop("data must contain columns 'var_ref_id' and 'qc_flag'")
  }
  
  data |> 
    dplyr::filter(qc_flag != "") |> 
    dplyr::group_by(var_ref_id, qc_flag) |>
    dplyr::summarise(n = n()) |> 
    dplyr::mutate(var_ref_id = factor(var_ref_id)) |> 
    # print(n = 100)
    
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = var_ref_id, y = n, fill = qc_flag), stat = "identity") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = "Flag summary", x = "var_ref_id", y = "Count") +
    ggplot2::coord_flip()  +
    # ggplot2::scale_fill_manual(values = c("low" = "blue", "high" = "red",
    #                                       "roc" = "green", 
    #                                       "consec" = "orange",
    #                                       "drift_corr" = "purple",
    #                                       "buoy_out" = "black")) +
    ggplot2::theme_bw()
  
}

# Function for plotting qc_flag timeseries
plot_flag_ts <- function(data) {
  
  # Check if datetime and qc_flag columns exist
  if (!all(c("datetime", "qc_flag") %in% colnames(data))) {
    stop("data must contain columns 'datetime' and 'qc_flag'")
  }
  
  data |> 
    dplyr::filter(qc_flag != "") |> 
    dplyr::slice(1:500000) |>
    ggplot2::ggplot() +
    scattermore::geom_scattermore(ggplot2::aes(x = datetime, y = var_ref_id, 
                                               colour = qc_flag), pointsize = 3) +
    # ggplot2::geom_point(ggplot2::aes(x = datetime, y = var_ref_id, colour = qc_flag), 
    #                     size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Flag timeseries", x = "Datetime", y = "Variable") #+
  # ggplot2::scale_colour_manual(values = c("low" = "blue", "high" = "red",
  #                                         "roc" = "green", "consec" = "orange",
  #                                         "drift_corr" = "purple",
  #                                         "buoy_out" = "black"))
  
}

plot_qc_data <- function(data) {
  
  # Check if datetime and qc_value columns exist
  if (!all(c("datetime", "qc_value") %in% colnames(data))) {
    stop("data must contain columns 'datetime' and 'qc_value'")
  }
  flag_summ <- data |> 
    dplyr::group_by(var_ref_id) |>
    dplyr::filter(!is.infinite(qc_value)) |>
    dplyr::mutate(y_min = min(qc_value, na.rm = TRUE) - 1) #|> 
  # dplyr::filter(y_min = ifelse(is.infinite(y_min), 0, y_min))
  summary(flag_summ)
  flag_summ |> 
    dplyr::filter(is.infinite(y_min))
  
  
  data |> 
    # dplyr::slice(1:500000) |>
    dplyr::filter(qc_flag %in% c("", "drift_corr")) |> 
    ggplot2::ggplot() +
    scattermore::geom_scattermore(ggplot2::aes(x = datetime, y = qc_value,
                                               colour = qc_flag), pointsize = 1) +
    scattermore::geom_scattermore(data = flag_summ, ggplot2::aes(x = datetime, 
                                                                 y = y_min, 
                                                                 colour = qc_flag), 
                                  pointsize = 4) +
    ggplot2::facet_wrap(~var_ref_id, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "QC timeseries", x = "Datetime", y = "QC value")
}

plot_var_ts_qc <- function(data, var_ref_id, #variable_ref,
                           FUN = median, 
                           days_offset = 184) {
  sel_vars <- var_ref_id
  
  # label_df <- dplyr::tibble(var_ref_id = sel_vars,
  #                           label = make_var_name(var_ref_id, 
  #                                                 variable_ref = variable_ref))
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  # QC Plot
  p2 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id,
                                      levels = device_pos$var_ref_id),
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime, var_ref_id) |>
    dplyr::summarise(qc_code = qc_code[1], .groups = "drop") |>
    dplyr::mutate(datetime = as.POSIXct(datetime, 
                                        format = "%Y-%m-%d", tz = "UTC")) |> 
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id,
                                 fill = qc_code)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "QC Code") +
    ggplot2::scale_fill_manual(values = qc_code_col_scale) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank())
  
  # Device Plot
  p3 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id,
                                      levels = device_pos$var_ref_id),
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime, var_ref_id) |>
    dplyr::summarise(device_id = device_id[1], .groups = "drop") |>
    dplyr::mutate(datetime = as.POSIXct(datetime, 
                                        format = "%Y-%m-%d", tz = "UTC")) |> 
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id,
                                 fill = device_id)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "Device") +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank())
  
  # Timeseries Plot
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::left_join(device_pos, by = c("var_ref_id")) |> 
    # dplyr::left_join(label_df, by = c("var_ref_id")) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id, 
                                      levels = rev(device_pos$var_ref_id))) 
  
  p1 <- ggplot2::ggplot(df) +
    scattermore::geom_scattermore(aes(datetime, qc_value, colour = var_ref_id), 
                                  pointsize = 0.8) +
    ggplot2::scale_colour_viridis_d(direction = -1, end = 0.8) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "", y = "QC Value", colour = "Variable") +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey", linewidth = 0.1)
    )
  
  # Combine with patchwork
  g <- p1 / p2 / p3 + 
    patchwork::plot_layout(heights = c(0.6, 0.2, 0.2), guides = "collect") & 
    patchwork::plot_annotation(tag_levels = "A") 
  
  return(g)
}

plot_var_qc <- function(data, var_ref_id) {
  sel_vars <- var_ref_id
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  
  
  # Timeseries Plot
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::left_join(device_pos, by = c("var_ref_id")) |> 
    dplyr::mutate(var_ref_id = factor(var_ref_id, 
                                      levels = rev(device_pos$var_ref_id))) 
  
  p1 <- ggplot2::ggplot(df) +
    scattermore::geom_scattermore(aes(datetime, qc_value, colour = var_ref_id),
                                  pointsize = 0.8) +
    ggplot2::scale_colour_viridis_d(direction = -1, end = 0.8) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "", y = "QC Value", colour = "Variable") +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey", linewidth = 0.1)
    )
  
  return(p1)
  
}


plot_qc_code_ts <- function(data, var_ref_id, FUN = median, days_offset = 184) {
  sel_vars <- var_ref_id
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  # QC Plot
  p2 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id, levels = device_pos$var_ref_id),
                  datetime = as.Date(datetime)) |>
    dplyr::group_by(datetime, var_ref_id) |>
    dplyr::summarise(qc_code = qc_code[1]) |>
    dplyr::mutate(datetime = as.POSIXct(datetime, 
                                        format = "%Y-%m-%d", tz = "UTC")) |> 
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id, fill = qc_code)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "QC Code") +
    ggplot2::scale_fill_manual(values = qc_code_col_scale) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank())
  
  return(p2)
}

plot_device_ts <- function(data, var_ref_id, FUN = median, days_offset = 184) {
  sel_vars <- var_ref_id
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  # Device Plot
  p3 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id, levels = device_pos$var_ref_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id, fill = device_id)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "Device") +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank())
  
  return(p3)
}

plot_var_ts_qc2 <- function(data, var_ref_id, FUN = median, days_offset = 184) {
  sel_vars <- var_ref_id
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  p2 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id, 
                                      levels = device_pos$var_ref_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id, fill = qc_code)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "QC Code") +
    ggplot2::scale_fill_manual(values = qc_code_col_scale) +
    # Remove x and y axis 
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank()) 
  
  p3 <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(var_ref_id = factor(var_ref_id, 
                                      levels = device_pos$var_ref_id)) |>
    ggplot2::ggplot(ggplot2::aes(x = datetime, y = var_ref_id,
                                 fill = device_id)) +
    ggplot2::geom_raster() +
    ggplot2::labs(x = "", y = "", fill = "Device") +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    # Remove x and y axis 
    ggplot2::theme_classic() +
    ggplot2::theme(axis.line = ggplot2::element_blank())
  
  
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::left_join(device_pos, by = c("var_ref_id")) |> 
    dplyr::mutate(var_ref_id = factor(var_ref_id, 
                                      levels = rev(device_pos$var_ref_id))) 
  
  p1 <- ggplot2::ggplot(df) +
    scattermore::geom_scattermore(aes(datetime, qc_value, colour = var_ref_id), 
                                  pointsize = 0.8) +
    ggplot2::scale_colour_viridis_d(direction = -1, end = 0.8) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = "", y = "QC Value", colour = "Variable") +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
                   # Add guide lines
                   panel.grid.major = ggplot2::element_line(colour = "grey", 
                                                            linewidth = 0.1)
    )
  
  g <- ggpubr::ggarrange(p1, p2, p3, ncol = 1,
                         heights = c(0.6, 0.2, 0.2), 
                         labels = c("Timeseries", "QC", "Device"), align = "v")
  return(g)
}

plot_ts_qc <- function(data) {
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  data |> 
    ggplot(aes(x = datetime, y = var_ref_id, fill = qc_code)) +
    geom_raster() +
    labs(x = "", y = "", fill = "QC Code") +
    scale_fill_manual(values = qc_code_col_scale) +
    # Remove x and y axis 
    theme_classic() +
    theme(axis.line = element_blank()) 
}

plot_var_doy <- function(data, var_ref_id, days_offset = 184, FUN = mean, 
                         facet = TRUE) {
  
  
  sel_vars <- var_ref_id
  
  device_pos <- decode_var_ref(sel_vars) |> 
    dplyr::mutate(var_ref_id = sel_vars) |> 
    dplyr::arrange(dplyr::desc(value_m))
  
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::mutate(date = lubridate::round_date(datetime, "day")) |>
    dplyr::group_by(date, var_ref_id) |>
    dplyr::summarise(dplyr::across(qc_value, \(x) FUN(x, na.rm = TRUE))) |> 
    dplyr::ungroup() |>
    dplyr::mutate(doy = lubridate::yday(date + lubridate::ddays(days_offset)),
                  year = lubridate::year(date + lubridate::ddays(days_offset)),
                  year = factor(year)
    ) |> 
    dplyr::left_join(device_pos, by = c("var_ref_id")) 
  
  z <- unique(df$value_m)
  ord_z <- sort(z)
  
  df <- df |> 
    dplyr::mutate(z_relative = factor(value_m, levels = ord_z)) |> 
    dplyr::arrange(date, z_relative)
  
  colour_lab <- dplyr::case_when(
    all(device_pos$reference == "d") ~ "Depth (m)",
    all(device_pos$reference == "h") ~ "Height (m)",
    all(device_pos$reference == "e") ~ "Elevation (m)",
    .default = "Z-relative (m)"
  )
  
  # data |> 
  #   dplyr::filter(var_ref_id %in% sel_vars) |>
  #   dplyr::mutate(year = lubridate::year(datetime)) |>
  #   group_by(year, var_ref_id) |>
  #   summarise(median = median(raw_value, na.rm = TRUE)) 
  
  p <- ggplot2::ggplot(df)
  
  if (length(z) > 1) {
    facet <- TRUE
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x = doy, y = qc_value, 
                                      colour = z_relative)) 
  } else if (length(z) == 1 & !facet) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x = doy, y = qc_value, 
                                      colour = year)) 
    colour_lab <- "Year"
  } else if (length(z) == 1 & facet) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(x = doy, y = qc_value)) 
  } else {
    stop("No data to plot")
  }
  
  if (facet) {
    p <- p +
      ggplot2::facet_wrap(~year)
  }
  
  if (length(z) > 2) {
    p <- p +
      ggplot2::scale_colour_viridis_d(direction = -1)
  } else if (length(z) == 2) {
    p <- p +
      ggplot2::scale_colour_viridis_d(direction = -1, end = 0.8)
  }
  
  p <- p +
    ggplot2::labs(x = "Day of year", y = "Value", colour = colour_lab) +
    
    ggplot2::theme_bw()
  p
  return(p)
}

# Function to remove data when the buoy is out of the water
remove_site_events <- function(data, site_events, sensor_map) {
  
  buoy_remove <- site_events |> 
    dplyr::select(site, date, action) |> 
    dplyr::filter(action %in% c("deployed", "removed")) |> 
    # if first acctions is "deployed" then remove it
    dplyr::filter(!(action == "deployed" & dplyr::row_number() == 1)) |>
    dplyr::group_by(action) |>
    dplyr::mutate(n = 1:dplyr::n()) |> 
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = action, values_from = date) |> 
    dplyr::mutate(
      device_id = "all",
      var_ref = "all",
      qc_flag = "buoy_out",
      date_interval = lubridate::interval(removed, deployed)
      ) |> 
    dplyr::filter(!is.na(deployed)) 
  
  # Calculate number of days
  n_days <- buoy_remove |> 
    dplyr::mutate(n_days = as.numeric(deployed - removed)) |> 
    dplyr::pull(n_days) |> 
    sum()
  
  # Battery failure dates
  batt_failure <- site_events |> 
    dplyr::select(site, date, action) |> 
    dplyr::filter(action %in% c("battery_failed", "battery_replaced")) |> 
    dplyr::arrange(date) |> 
    # dplyr::mutate(lead_action = dplyr::lag(action)) 
    # Check for repeat battery replaced
    dplyr::filter(!(action == "battery_replaced" 
                    & dplyr::lag(action) == "battery_replaced")) |> 
    dplyr::group_by(action) |>
    dplyr::mutate(n = 1:dplyr::n()) |> 
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = action, values_from = date) |> 
    dplyr::mutate(
      device_id = "all",
      var_ref = "all",
      qc_flag = "battery_failure",
      date_interval = lubridate::interval(battery_failed, battery_replaced)
      )
  
  # Faulty sensor  ariablesdates
  pot_vars <- site_events |> 
    dplyr::filter(grepl("fault", action), !variables %in% c("all", "?")) |> 
    dplyr::pull(variables) |> 
    # Split into multiple strings by ", "
    stringr::str_split(", ") |>
    unlist() |> 
    unique() 
  
  # Extract the faulty sensors from the sensor map
  sensors <- sensor_map |>
    dplyr::filter(grepl(paste0(pot_vars, collapse = "|"), var_abbr) |
                     grepl(paste0(pot_vars, collapse = "|"), var_ref_id)
                  ) |> 
    dplyr::mutate(date_interval = lubridate::interval(date_from, date_to)) 
  
  faulty_sensors <- lapply(pot_vars, \(v) {
    site_events |> 
      dplyr::filter(grepl("fault", action), 
                    grepl(v, variables)) |> 
      dplyr::mutate(variables = v) |> 
      map_events_device(sensor_map = sensor_map) |> 
      # dplyr::select(site, variables, date, action) |>
      dplyr::arrange(date) |> 
      dplyr::group_by(action) |>
      dplyr::mutate(n = 1:dplyr::n(), var_ref = v) |> 
      dplyr::ungroup() |> 
      tidyr::pivot_wider(id_cols = c(site, device_id, n, var_ref), names_from = action, 
                         values_from = date) |>
      dplyr::select(site, device_id, var_ref, fault_start, fault_end)
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(date_interval = lubridate::interval(fault_start, fault_end),
                  qc_flag = "sensor_fault"
                  ) 
  
  
  
  # removal data frame
  remove_df <- dplyr::bind_rows(buoy_remove, batt_failure, faulty_sensors) |> 
    dplyr::select(site, device_id, var_ref, qc_flag, date_interval)
  
  print(remove_df, n = 50)
  
  
  # Set value_qc to NA if within the interval
  data <- remove_data(data, remove_df)
  
  return(data)
}

# Function to remove faulty sensors

#' @importFrom data.table :=
#' @importFrom data.table setkey as.data.table
remove_data <- function(data, remove_df) {

  # Set value_qc to NA if within the interval
  # Convert to data.tables
  data_dt <- data.table::as.data.table(data)
  remove_dt <- data.table::as.data.table(remove_df)
  
  # Set key for fast lookup
  data.table::setkey(data_dt, datetime)
  data.table::setkey(data_dt, device_id)
  data.table::setkey(data_dt, var_ref_id)
  
  # Loop over fault periods and update in place
  for (i in seq_len(nrow(remove_df))) {
    
    if (remove_df$device_id[i] == "all" & 
        remove_df$var_ref[i] == "all") {
      
      # sub <- data_dt[datetime %within% remove_df$date_interval[i], ]
      # Remove all data
      data_dt[datetime %within% remove_df$date_interval[i] &
                !is.na(qc_value),
              c("qc_value", "qc_code", "qc_flag") := list(NA, "QC 200", 
                                                          remove_df$qc_flag[i])]
    # } else if (remove_df$device_id[i] == "all") {
      # Remove all data for the variable
      # data_dt[datetime %within% remove_df$date_interval[i] &
      # #           grepl(remove_df$var_ref[i], var_ref_id),
      #         c("qc_value", "qc_code", "qc_flag") := list(NA, "QC 200", 
      #                                                     remove_df$qc_flag[i])]
    } else if (remove_df$var_ref[i] == "all") {
      # Remove all data for the device
      data_dt[datetime %within% remove_df$date_interval[i] &
                !is.na(qc_value) &
                device_id == remove_df$device_id[i],
              c("qc_value", "qc_code", "qc_flag") := list(NA, "QC 200", 
                                                          remove_df$qc_flag[i])]
    } else {
      data_dt[datetime %within% remove_df$date_interval[i] &
                # !is.na(qc_value) &
                device_id == remove_df$device_id[i] &
                grepl(remove_df$var_ref[i], var_ref_id), ]
      
      data_dt[datetime %within% remove_df$date_interval[i] &
                !is.na(qc_value) &
                device_id == remove_df$device_id[i] &
                grepl(remove_df$var_ref[i], var_ref_id),
              c("qc_value", "qc_code", "qc_flag") := list(NA, "QC 200", 
                                                          remove_df$qc_flag[i])]
    }
  }
  
  data <- tibble::as_tibble(data_dt)
  # unique(data$qc_flag)
  # 
  # data |> 
  #   filter(qc_flag == "sensor_fault") 
  
  return(data)
}


# Function to scale data
scale_data <- function(data, var_ref_id, sensor_scaling) {
  
  sel_vars <- var_ref_id
  
  sel_devices <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars) |>
    dplyr::pull(device_id) |>
    unique()
  
  sensor_scales <- sensor_scaling |> 
    dplyr::filter(device_id %in% sel_devices)
  
  sensor_scales_dates <- sensor_scales |> 
    build_date_interval() |> 
    dplyr::select(device_id, offset, multiplier, date_from, date_to, date_interval)
  
  
  data$offset <- NA
  data$multiplier <- NA
  for (i in 1:nrow(sensor_scales_dates)) {
    data <- data |>
      dplyr::mutate(
        offset = dplyr::case_when(
          var_ref_id %in% sel_vars &
            datetime %within% sensor_scales_dates$date_interval[i] ~ sensor_scales_dates$offset[i],
          TRUE ~ offset
        ),
        multiplier = dplyr::case_when(
          var_ref_id %in% sel_vars &
            datetime %within% sensor_scales_dates$date_interval[i] ~ sensor_scales_dates$multiplier[i],
          TRUE ~ multiplier
        )
      )
  }
  data <- data |> 
    dplyr::mutate(
      qc_value = dplyr::case_when(
        var_ref_id %in% sel_vars ~ (qc_value - offset) / (multiplier),
        # var_ref_id %in% sel_vars ~ (qc_value) / multiplier) - offset,
        .default = qc_value
      ),
      qc_code = dplyr::case_when(
        var_ref_id %in% sel_vars ~ "QC 300",
        TRUE ~ qc_code
      )
    )
  # plot(data$adj_value2, ylim = c(0, 1500))
  
  # summary(data)
  return(data)
}

# Function to correct drift
adjust_linear_drift <- function(data, var_ref_id, sensor_refs, 
                                date_range = NULL, expand_coeffs = FALSE) {
  
  if (is.null(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_ranges <- range(sensor_refs[["date"]])
    tzone <- lubridate::tz(date_ranges)
    date_interval <- coerce_date_range(date_range = date_ranges, tzone = tzone)
  } else {
    date_ranges <- date_range
    tzone <- lubridate::tz(data[["datetime"]])
    date_interval <- coerce_date_range(date_range = date_ranges, tzone = tzone)
  }
  
  if (expand_coeffs) {
    sens_ref <- sensor_refs |> 
      dplyr::select(date, value_actual, value_measured) |> 
      tidyr::pivot_wider(names_from = value_actual, 
                         values_from = c(value_measured), names_prefix = "val_") 
    # Add a row for the last date
    sens_ref <- sens_ref |> 
      dplyr::bind_rows(
        dplyr::tibble(date = date_range[2])
      )
    
    # Function to predict the final NA value using a linear model
    predict_na <- function(dates, values) {
      # Filter out NAs
      non_na <- !is.na(values)
      # Fit linear model on numeric date
      df <- data.frame(dates = dates[non_na], values = values[non_na])
      fit <- lm(values ~ dates, data = df)
      # Predict for the final date
      newdata <- data.frame(dates = dates)
      pred <- predict(fit, newdata = newdata)
      values[!non_na] <- pred[!non_na]
      return(values)
    }
    
    # Fit a linear model and extract values for NA
    sensor_refs <- sens_ref |> 
      dplyr::mutate(
        dplyr::across(dplyr::starts_with("val_"), 
                      ~ predict_na(date, .x)
        ),
      ) |> 
      tidyr::pivot_longer(cols = dplyr::starts_with("val_"), 
                          names_to = "value_actual", 
                          values_to = "value_measured") |> 
      dplyr::mutate(value_actual = as.numeric(gsub("val_", "", value_actual))) 
    
  }
  
  coef_table <- sensor_refs |> 
    dplyr::select(date, value_actual, value_measured) |> 
    dplyr::group_by(date) |>
    # Calculate m an b for each date
    dplyr::summarise(model = list(lm(value_actual ~ value_measured,
                                     data = dplyr::pick(value_actual, value_measured)))) |> 
    dplyr::ungroup() |>
    # extract coefficients
    dplyr::mutate(intercept = purrr::map_dbl(model, ~ coef(.x)[1]),
                  slope = purrr::map_dbl(model, ~ coef(.x)[2])) |> 
    dplyr::select(date, intercept, slope) |> 
    dplyr::filter(!is.na(intercept), !is.na(slope))
  
  df <- coef_table |> 
    tidyr::complete(date = seq(date_ranges[1], date_ranges[2], by = "15 min")) |> 
    dplyr::mutate(
      intercept = stats::approx(date, intercept, date,
                                method = "linear", rule = 2)$y,
      slope = stats::approx(date, slope, date,
                            method = "linear", rule = 2)$y
      
    )
  
  ggplot(df) +
    geom_point(aes(date, intercept)) +
    theme_bw()
  
  # models <- sensor_refs |> 
  #   dplyr::select(date, value_actual, value_measured) |> 
  #   dplyr::group_by(value_actual) |>
  #   tidyr::complete(date = seq(date_ranges[1], date_ranges[2], by = "15 min")) |>
  #   dplyr::arrange(date) |>
  #   dplyr::mutate(
  #     value_measured = stats::approx(date, value_measured, date,
  #                                    method = "linear", rule = 2)$y
  #   ) |>
  #   dplyr::ungroup() |> 
  #   dtplyr::lazy_dt() |>
  #   dplyr::group_by(date) |>
  #   # Calculate m and bb for each date
  #   summarise(model = list(lm(value_actual ~ value_measured,
  #                             data = cur_data()))) |> 
  #   data.table::as.data.table()
  # 
  # # If you just want coefficients without additional statistics
  # coef_table <- models[, .(coef = list(coef(model[[1]]))), by = date] |> 
  #   tidyr::unnest_wider(coef, names_sep = "_")
  # names(coef_table) <- c("date", "intercept", "slope")
  
  ggplot() +
    geom_point(data = coef_table, aes(date, intercept)) +
    theme_bw()
  
  sel_var <- var_ref_id
  
  data <- data |> 
    dplyr::left_join(df, by = c("datetime" = "date")) |> 
    dplyr::mutate(
      qc_value = dplyr::case_when(
        var_ref_id == sel_var & datetime %within% date_interval ~ intercept +
          (slope * qc_value),
        TRUE ~ qc_value
      ),
      qc_flag = dplyr::case_when(
        var_ref_id == sel_var & datetime %within% date_interval ~
          format_flag(qc_flag, "drift_corr"),
        TRUE ~ qc_flag
      ),
      qc_code = dplyr::case_when(
        var_ref_id == sel_var & datetime %within% date_interval ~ "QC 300",
        TRUE ~ qc_code
      )
    ) |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr , raw_value, 
                  qc_value, qc_flag, qc_code)
  
  data |> 
    mutate(diff = qc_value - raw_value) |>
    summary()
  
  ggplot() +
    geom_point(data = data, aes(datetime, raw_value)) +
    # coord_cartesian(ylim = c(0, 15)) +
    theme_bw()
  
  return(data)
  
  # ggplot(dat) +
  #   geom_point(aes(date, value_measured, colour = factor(value_actual))) +
  #   theme_bw()
  
  dt <- data.frame(datetime = unique(data$datetime)) |> 
    dplyr::left_join(sensor_refs, by = c("datetime" = "date")) |>
    dplyr::select(datetime, device_id, value_actual, value_measured)
  # Create breakpoints at the midpoint between reference dates
  uniq_dates <- unique(sensor_refs$date)
  breakpoints <- uniq_dates[-1] - diff(uniq_dates) / 2
  breakpoints <- c(min(uniq_dates), breakpoints, max(uniq_dates))
  
  # Assign each raw data point to a calibration period
  data <- data |> 
    dplyr::mutate(period = cut(datetime, breaks = breakpoints, labels = FALSE,
                               include.lowest = TRUE))
  
  
  # Function to fit a model for each period
  fit_models <- sensor_refs |> 
    dplyr::group_by(period = cut(date, breaks = breakpoints, labels = FALSE,
                                 include.lowest = TRUE)) |> 
    tidyr::nest()  |> 
    dplyr::mutate(model = purrr::map(data, ~ lm(value_actual ~ value_measured, 
                                                data = .x)))
  
  # Apply the correct model to raw data
  # Join the models with raw data, using the last known model for extrapolation
  dc_data <- data |> 
    dplyr::left_join(dat)
  # dplyr::left_join(fit_models, by = "period") |> 
  dplyr::mutate(
    value_measured = raw_value,
    qc_value = purrr::map2_dbl(model, value_measured, ~ if (!is.null(.x)) {
      stats::predict(.x, newdata = data.frame(value_measured = .y))
    } else {
      NA_real_  # Handle missing cases (optional: fill with nearest model)
    })
  )
  
  # Fill missing corrections with the closest available model
  dc_data <- dc_data |> 
    tidyr::fill(qc_value, .direction = "downup") |> 
    dplyr::select(-model, -data)
  
  
  
  return(dc_data)
}

# Function used to correct drift using quantile mapping
adjust_qmap <- function(data, var_ref_id, ref_period,
                        adj_period, qstep = 0.1) {
  
  
  tzone <- lubridate::tz(data$datetime)
  
  adj_interval <- coerce_date_range(adj_period, tzone = tzone)
  sel_var <- var_ref_id
  
  adj_data <- data |> 
    dplyr::filter(datetime %within% adj_interval, var_ref_id == sel_var,
                  !is.na(qc_value)) |> 
    dplyr::mutate(yday = lubridate::yday(datetime))
  
  ref_interval <- coerce_date_range(ref_period, tzone = tzone)
  # Extract the reference and adjustment periods
  ref_data <- data |>
    dplyr::filter(datetime %within% ref_interval, var_ref_id == sel_var,
                  !is.na(qc_value)) |> 
    dplyr::mutate(yday = lubridate::yday(datetime)) |> 
    dplyr::filter(yday %in% adj_data$yday)
  
  
  fit_qmap <- qmap::fitQmapQUANT(obs = ref_data$qc_value, wet.day = FALSE, 
                                 mod = adj_data$qc_value, qstep = qstep)
  
  # Apply quantile mapping correction
  adj_data$corrected_data <- qmap::doQmapQUANT(adj_data$qc_value, fit_qmap)
  
  # Plot the quantile mapping
  p1 <- ggplot() +
    geom_point(data = adj_data, aes(qc_value, corrected_data)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    theme_bw()
  
  # Plot the time series
  p2 <- ggplot() +
    geom_point(data = adj_data, aes(datetime, qc_value)) +
    geom_point(data = adj_data, aes(datetime, corrected_data), colour = "red") +
    theme_bw()
  
  g <- ggpubr::ggarrange(p1, p2, ncol = 2)
  print(g)
  
  adj_data <- adj_data |> 
    dplyr::select(-qc_value) |> 
    dplyr::rename(qc_value = corrected_data)
  
  data |> 
    dplyr::anti_join(adj_data, by = c("datetime", "var_ref_id", "device_id",
                                      "raw_value")) |>
    dplyr::bind_rows(adj_data) |>
    dplyr::arrange(var_ref_id, datetime)
  
}

# Function used to correct drift using earlier data
adjust_drift_quantiles <- function(data, var_ref_id, adj_period, quantiles = c(0.01, 0.99)) {
  
  # Extract the reference and adjustment periods
  # ref_data <- data |> 
  #   dplyr::filter(datetime %within% lubridate::interval(ref_period[1], ref_period[2])) |> 
  #   dplyr::mutate(ref = TRUE)
  tzone <- lubridate::tz(data$datetime)
  adj_interval <- coerce_date_range(adj_period, tzone = tzone)
  sel_var <- var_ref_id
  
  adj_data <- data |> 
    dplyr::filter(datetime %within% adj_interval, var_ref_id == sel_var)
  
  adj_summ_list <- lapply(quantiles, function(q) {
    adj_data |>
      dplyr::mutate(
        year = lubridate::year(datetime)
      ) |> 
      dplyr::group_by(year) |>
      dplyr::summarise(
        q = quantile(qc_value, q, na.rm = TRUE)
      ) |> 
      dplyr::rename(!!paste0("p", q * 100) := q)
  })
  
  # merge list by year column
  adj_summ <- Reduce(function(x, y) merge(x, y, by = "year"), adj_summ_list)
  
  use_quantreg <- FALSE
  if (use_quantreg) {
    # Fit quantile regression for 1% and 99% quantiles
    q1_model <- quantreg::rq(qc_value ~ as.numeric(datetime), tau = quantiles[1], data = adj_data)
    q2_model <- quantreg::rq(qc_value ~ as.numeric(datetime), tau = quantiles[2], data = adj_data)
    
    adj_data <- adj_data |> 
      dplyr::mutate(Q1_t = predict(q1_model, newdata = adj_data),
                    Q2_t = predict(q2_model, newdata = adj_data))
    
    # Compute long-term mean quantiles (anchor for correction)
    mean_q1 <- mean(adj_data$Q1_t, na.rm = TRUE)
    mean_q2 <- mean(adj_data$Q2_t, na.rm = TRUE)
    
    adj_data <- adj_data |> 
      dplyr::mutate(normalized_value = (qc_value - Q1_t) / (Q2_t - Q1_t),
                    drift_correction = normalized_value * (mean_q2 - mean_q1),
                    corrected_value = normalized_value * (mean_q2 - mean_q1) + mean_q1)
    
    # Plot the quantile mapping
    ggplot() +
      geom_point(data = adj_data, aes(datetime, qc_value, colour = "green")) +
      geom_point(data = adj_data, aes(datetime, corrected_value)) +
      # geom_line(data = adj_data, aes(datetime, Q1_t), colour = "blue") +
      # geom_line(data = adj_data, aes(datetime, Q2_t), colour = "red") +
      theme_bw()
  }
  
  
  
  
  
  # Calculate the slope for each quantile
  # Compute slopes for each quantile column
  slope_means <- adj_summ |> 
    dplyr::summarise(
      # Apply lm to each quantile column and extract the slope (b1)
      slope = purrr::map(dplyr::select(adj_summ, dplyr::starts_with("p")), 
                         ~ {
                           lm_model <- lm(.x ~ year, data = adj_summ)
                           tibble(slope = coef(lm_model)[2])  # Extract the slope (second coefficient)
                         }),
      .groups = "drop"
    ) |> 
    unlist()
  
  slope_mean <- mean(slope_means)
  
  # Extract lm coefficeints
  # Extract slope (b1) from lm models
  # slope_low <- coef(adj_low)[2]  # Slope of the p01 trend
  # slope_high <- coef(adj_high)[2]  # Slope of the p99 trend
  
  # Define a reference time (e.g., first datetime in dataset)
  ref_time <- min(adj_data$datetime, na.rm = TRUE)
  
  
  # Apply drift correction continuously
  adj_data_corrected <- adj_data |> 
    dplyr::mutate(
      time_since_ref = as.numeric(difftime(datetime, ref_time, units = "days")) / 365.25,  # Convert to years
      drift_correction = time_since_ref * (slope_mean),  # Apply continuous drift correction
      qc_value_corrected = qc_value - drift_correction  # Adjust sensor values
    ) |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, qc_value_corrected)
  
  # Merge back with the original data
  upd_data <- data |> 
    dplyr::left_join(adj_data_corrected, by = c("datetime", "var_ref_id", 
                                                "site", "device_id",
                                                "var_abbr")) |> 
    dplyr::mutate(
      qc_value = dplyr::case_when(
        datetime %within% adj_interval ~ qc_value_corrected,
        TRUE ~ qc_value
      ), 
      qc_code = dplyr::case_when(
        datetime %within% adj_interval ~ "QC 300",
        TRUE ~ qc_code
      ),
      qc_flag = dplyr::case_when(
        datetime %within% adj_interval ~ format_flag(qc_flag, "drift_corr"),
        TRUE ~ qc_flag
      )
    ) |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, raw_value, qc_value, qc_flag, qc_code)
  
  return(upd_data)
  
}


# Functo to extract signal from noise
extract_signal <- function(data, var_ref_id, date_range, width = "5 day", threshold = 15) {
  
  sel_vars <- var_ref_id
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  # Convert width to seconds
  width <- lubridate::as.duration(width)
  # Calculate how many rows are in the width
  n_vec <- data |> 
    dplyr::filter(datetime <= (datetime[1] + width)) |>
    nrow()
  
  
  df <- data |> 
    dplyr::filter(var_ref_id %in% sel_vars,
                  datetime %within% date_range) |>
    dplyr::mutate(
      sd = zoo::rollapply(qc_value, width = n_vec, FUN = sd, fill = NA),
      median = zoo::rollapply(qc_value, width = n_vec, FUN = median, fill = NA,
                              align = "right")
    )
  # detect when the threshold is first crossed
  # Detect the biofouling event (first occurrence of high noise level)
  event_index <- which(df$sd > threshold)[1]
  
  ggplot() +
    geom_point(data = df, aes(datetime, sd)) 
  
  if (!is.na(event_index)) {
    # Apply a smoothing filter to reduce noise
    df2 <- df |> 
      dplyr::mutate(qc_value = ifelse(dplyr::row_number() >= event_index, 
                                      zoo::rollapply(qc_value, n_vec, median,
                                                     fill = NA, align = "right"), 
                                      qc_value))
  } 
  
  
  ggplot() +
    geom_point(data = df, aes(datetime, sd)) 
  
  
  ggplot() +
    geom_point(data = df, aes(datetime, qc_value)) +
    geom_point(data = df2, aes(datetime, qc_value), colour = "red") +
    geom_point(data = df, aes(datetime, median), colour = "blue") +
    theme_bw()
  
  return(df)
  
}

# Function to adjust variables
apply_adjustment <- function(data, var_ref_id, date_range, FUN = \(x) x,
                             qc_flag = "adjusted") {
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  sel_vars <- var_ref_id
  
  flag_inp <- qc_flag
  
  adj_data <- data |> 
    dplyr::filter(datetime %within% date_range & var_ref_id %in% sel_vars) |> 
    dplyr::mutate(orig_value = qc_value) |> 
    dplyr::mutate(
      qc_value = dplyr::case_when(
        datetime %within% date_range & var_ref_id %in% sel_vars ~ FUN(qc_value),
        TRUE ~ qc_value
      ),
      # Check if the value has been adjusted and or if both are NA
      check = dplyr::case_when(
        qc_value != orig_value & !is.na(qc_value) ~ "adjusted",
        is.na(qc_value) & !is.na(orig_value) ~ "adjusted",
        is.na(qc_value) & is.na(orig_value) ~ "missing",
        TRUE ~ "unchanged"
      ),
      
      qc_flag = dplyr::case_when(
        check == "adjusted" ~ format_flag(qc_flag, flag_inp),
        TRUE ~ qc_flag
      ),
      qc_code = dplyr::case_when(
        check == "adjusted" & !is.na(qc_value) & flag_inp == "adjusted" ~ "QC 300",
        check == "adjusted" & is.na(qc_value)  ~ "QC 200",
        TRUE ~ qc_code
      )
    ) |> 
    dplyr::filter(check == "adjusted")
  
  adj_data$qc_value[1] == FUN(adj_data$orig_value[1])
  adj_data$orig_value[1] == adj_data$qc_value[1] #FUN(adj_data$qc_value[1])
  adj_data$orig_value[1] == FUN(adj_data$qc_value[1])
  
  # Remove the adjusted data from the original dataset and add the adjusted data
  data2 <- data |> 
    dplyr::anti_join(adj_data, by = c("datetime", "var_ref_id", "site", "device_id",
                                      "var_abbr", "raw_value")) |> 
    dplyr::bind_rows(adj_data) |> 
    dplyr::arrange(var_ref_id, datetime) |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, raw_value, 
                  qc_value, qc_flag, qc_code)
  return(data2)
  
  data2 <- data2 |> 
    
    
    # mutate(qc_value = round(qc_value, 2)) |> 
    # dplyr::filter(var_ref_id %in% sel_vars) |> 
    dplyr::mutate(
      orig_value = qc_value,
      qc_value = dplyr::case_when(
        datetime %within% date_range & var_ref_id %in% sel_vars ~ FUN(raw_value),
        TRUE ~ qc_value
      ),
      # Check if the value has been adjusted and or if both are NA
      check = dplyr::case_when(
        round(qc_value, 2) != round(orig_value, 2) & !is.na(qc_value) ~ "adjusted",
        is.na(qc_value) & !is.na(orig_value) ~ "adjusted",
        is.na(qc_value) & is.na(orig_value) ~ "missing",
        TRUE ~ "unchanged"
      ),
      
      qc_flag = dplyr::case_when(
        check == "adjusted" ~ format_flag(qc_flag, flag_inp),
        TRUE ~ qc_flag
      ),
      qc_code = dplyr::case_when(
        check == "adjusted" & !is.na(qc_value) ~ "QC 300",
        check == "adjusted" & is.na(qc_value)  ~ "QC 200",
        TRUE ~ qc_code
      )
    )# |> 
  # dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, raw_value, 
  # qc_value, qc_flag, qc_code)
  
  #   data2 |>
  #     filter(orig_value < 0, datetime %within% date_range & var_ref_id %in% sel_vars )
  #   #   mutate(diff = qc_value - orig_value, fun_chk = FUN(orig_value)) 
  #   #   # dplyr::filter(qc_value < 0, datetime %within% date_range) |>
  #   #   mutate(test = FUN(qc_value))
  # 
  tst <- data2 |>
    filter(check == "adjusted") |>
    mutate(diff = qc_value - orig_value)
  tst
  # round(tst$qc_value[1], 2)
  # round(tst$orig_value[1], 2)
  # 
  return(data)
}

# Function to plotly data
plotly_data <- function(data, y1, y2 = NULL, date_range, buffer = 0, sub = 10,
                        site_events = NULL, value_col = "qc_value") {
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    x0 <- date_range[1]
    x1 <- date_range[2]
    date_range <- coerce_date_range(date_range, tzone = tzone, buffer = buffer)
  }
  
  if (y1 %in% names(data)) {
    df <- data |> 
      dplyr::select(dplyr::all_of(c("datetime", y1, y2))) |>
      dplyr::filter(datetime %within% date_range)
  } else {
    
    # Check if value_col is in data
    if (!value_col %in% colnames(data)) {
      stop("value_col must be a column in data")
    }
    
    sel_vars <- c(y1, y2)
    df <- data |> 
      dplyr::filter(datetime %within% date_range,
                    var_ref_id %in% sel_vars
      ) |> 
      dplyr::select(datetime, var_ref_id, dplyr::contains(value_col)) |> 
      dplyr::rename(variable = var_ref_id) |> 
      tidyr::pivot_wider(names_from = variable, 
                         values_from = dplyr::all_of(value_col)) |> 
      dplyr::select(dplyr::all_of(c("datetime", y1, y2)))
  }
  
  df <- subset_data(df, sub = sub)
  
  p <- plotly::plot_ly() 
  
  p <- p |> 
    plotly::add_trace(x = ~df$datetime, y = ~df$value, color = ~df$variable,
                      type = "scatter", mode = "markers",
                      text = ~format(df$datetime, "%Y-%m-%d %H:%M"), # Format
                      hoverinfo = "text+y" # Show formatted date + y value
    ) |> 
    # Add label to y axis
    plotly::layout(yaxis = list(title = y1))
  
  # If buffer != 0 then add vertical lines for the date range
  if (buffer != 0) {
    p <- p |> 
      plotly::layout(
        shapes = list(vline(x0, "red"), vline(x1, "red"))
      )
  }
  
  if (!is.null(site_events)) {
    site_visit_date <- site_events |> 
      dplyr::filter(site_visit) |> 
      dplyr::pull(date)
    p <- p |> 
      plotly::layout(
        shapes = list(vline(site_visit_date[1], "red"))
      )
  }
  
  return(p)
  
  p <- p |> 
    plotly::add_trace(x = ~df$datetime, y = ~df[[y1]], name = y1, 
                      type = "scatter", mode = "lines",
                      text = ~format(df$datetime, "%Y-%m-%d %H:%M"), # Format
                      hoverinfo = "text+y" # Show formatted date + y value
    ) |> 
    # Add label to y axis
    plotly::layout(yaxis = list(title = y1)) 
  # Format date hover text for date
  
  
  
  
  if (!is.null(y2)) {
    p <- p |> 
      plotly::add_trace(x = ~df$datetime, y = ~df[[y2]], name = y2, 
                        type = "scatter", mode = "lines",
                        text = ~format(df$datetime, "%Y-%m-%d %H:%M"), # Format
                        hoverinfo = "text+y" # Show formatted date + y value
      ) |> 
      # Add label to y axis
      plotly::layout(yaxis2 = list(title = y2, overlaying = "y", 
                                   side = "right")) 
    # Format date hover text for date
  }
  p
}

# Add vertical line plotly
vline <- function(x = 0, colour = "black", dash = "dot") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = colour, dash = dash)
  )
}

# Function to reassign column values within a date range, or replace with NA's

reassign <- function(data, var1, var2 = NULL, date_range) {
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  
  
  data[data$datetime %within% date_range, ]
  # If var2 is NULL, replace var1 with NA's
  if (is.null(var2)) {
    data |> 
      dplyr::mutate(!!sym(var1) := dplyr::if_else(datetime %within% date_range,
                                                  NA_real_, !!sym(var1)))
  } else {
    data |> 
      dplyr::mutate(!!sym(var1) := dplyr::if_else(datetime %within% date_range, 
                                                  !!sym(var2), !!sym(var1))
      )
  }
}

# Function to swap values between two columns
swap_cols <- function(data, var1, var2, date_range) {
  data |> 
    dplyr::mutate(temp = !!sym(var1)) |> 
    reassign(var1, var2, date_range) |> 
    reassign(var2, "temp", date_range) |> 
    dplyr::select(-temp)
}


# Function to coerce date range to lubridate interval
coerce_date_range <- function(date_range, tzone, buffer = 0) {
  
  # Ensure date_range is a lubridate interval
  if (all(class(date_range) != "Interval")) {
    if (length(date_range) == 2) {
      
      # Ensure they are in the correct format
      date_range <- lubridate::parse_date_time(date_range, 
                                 orders = c("ymd HMS", "ymd HM", "ymd", 
                                            "ymd HMS", "ymd HM", "ymd"), 
                                 tz = tzone)
      
      # Apply buffer in hours (convert to seconds)
      if (buffer != 0) {
        # Convert hours to seconds (3600 seconds in 1 hour)
        buffer_seconds <- buffer * 3600
        
        # Apply the buffer to both ends of the interval
        date_range <- lubridate::interval(
          start = lubridate::with_tz(lubridate::as_datetime(date_range[1]) -
                                       buffer_seconds, tzone),
          end = lubridate::with_tz(lubridate::as_datetime(date_range[2]) + 
                                     buffer_seconds, tzone)
        )
      } else {
        date_range <- lubridate::interval(date_range[1], date_range[2], 
                                          tzone = tzone)
      }
    }
  }
  
  return(date_range)
}

# Function to standardise column names based on the mapping
standardise_columns <- function(data, col_mapping) {
  new_names <- sapply(colnames(data), function(x) {
    if (x %in% col_mapping) {
      return(names(col_mapping[col_mapping == x]))
    } else {
      return(x)
    }
  })
  colnames(data) <- new_names
  return(data)
}

# Function to assign qc_code to data
assign_qc_code <- function(data, var_ref_id) {
  
  if (missing(var_ref_id)) {
    sel_vars <- unique(data$var_ref_id)
  } else {
    sel_vars <- var_ref_id
  }
  
  qc_codes <- c("Missing Record" = "QC 100",
                "No Quality or Non Verified" = "QC 200",
                "Synthetic" = "QC 300",
                "Poor Quality" = "QC 400",
                "Fair Quality" = "QC 500",
                "Good Quality" = "QC 600")
  
  # Create a shiny app with a dropdown menu to select the variable
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("Select Variable"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("var", "Select Variable", choices = sel_vars)
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output) {
    output$plot <- shiny::renderPlot({
      data |> 
        dplyr::filter(var_ref_id == input$var) |> 
        # subset_data(sub = 20) |>
        ggplot() +
        # geom_point(aes(datetime, qc_value, colour = qc_code)) +
        scattermore::geom_scattermore(aes(datetime, qc_value, colour = qc_code)) +
        theme_bw()
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

# viz_data <- function(data, y_var = NULL) {
#   
#   # Which column is the dates (check for POSIXct)
#   col_dt <- find_posixct_column(data)
#   
#   # set initial variable if none supplied
#   if(is.null(y_var)) { y_var = names(data)[sapply(data, is.numeric)][1] }
#   
#   
#   # Get the list of available variables (exclude the DateTime column)
#   available_vars <- setdiff(names(data), col_dt)
#   
#   ui <- miniUI::miniPage(  # miniPage ensures it works well in RStudio Viewer
#     miniUI::gadgetTitleBar("Interactive Plot", left = NULL, 
#                            right = shiny::actionButton("done_btn", "Done")),
#     br(),
#     fluidRow(
#       column(1),
#       column(4,
#              # Dropdown menu to select the variable to plot (exclude DateTime)
#              selectInput("y_var_select", "Display variable", 
#                          choices = available_vars, 
#                          selected = y_var)
#       ),
#       column(3,
#              # Dropdown menu for locking the y-axis zoom
#              radioButtons("lock_y", "Lock y-axis", 
#                           choices = c("Yes" = TRUE, "No" = FALSE),
#                           selected = TRUE, inline = TRUE)
#       ),
#       column(3,
#              # Radio button to select the plot type (line or point)
#              radioButtons("plot_type", "Plot type",
#                           choices = c("Line" = "line", "Point" = "point"),
#                           selected = "line", inline = T)
#       )
#     ),
#     fluidRow(
#       column(1),
#       column(10,
#              plotOutput("myplot",
#                         click = "plot_click",
#                         dblclick = "plot_dblclick",
#                         brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
#                         hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
#              
#              verbatimTextOutput("hover_info"),
#              verbatimTextOutput("click_info"),
#              verbatimTextOutput("brush_info")  # Text output for the brush info
#       ),
#       column(1)
#     )
#   )
#   
#   server <- function(input, output, session) {
#     
#     # Ensure variables exist in the dataset
#     req(col_dt %in% names(data), y_var %in% names(data))
#     
#     # Set initial zoom range
#     ranges <- reactiveValues(
#       x = range(data[[col_dt]], na.rm = TRUE),
#       y = range(data[[y_var]], na.rm = TRUE)
#     )
#     
#     output$myplot <- renderPlot({
#       
#       # Filter data based on the current zoom range
#       df <- data |>
#         rename("value" = input$y_var_select,
#                "datetime" = col_dt) |>
#         filter(datetime >= ranges$x[1],
#                datetime <= ranges$x[2]) 
#       
#       # Choose plot type based on user input
#       if(input$plot_type == "point") {
#         
#         p <-  ggplot(df) +
#           scattermore::geom_scattermore(aes(x = datetime, y = value), 
#                                         pixels = c(1024,512), pointsize = 2, colour = "blue3")
#         
#       } else {
#         
#         # Standardize to 1000 points or so for quick plotting
#         rows = nrow(df)
#         
#         if(rows > 3000) {
#           
#           agg = (as.numeric(max(df$datetime)) - as.numeric(min(df$datetime))) / 1000
#           
#           df <- data |>
#             rename("value" = input$y_var_select,
#                    "datetime" = col_dt) |>
#             filter(datetime >= ranges$x[1],
#                    datetime <= ranges$x[2]) |>
#             select(datetime, value) |>
#             mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / agg) * agg, origin = "1970-01-01")) |>
#             dtplyr::lazy_dt(.) |>
#             group_by(datetime) |>
#             dplyr::summarise(
#               min_val = min(value, na.rm = TRUE),
#               max_val = max(value, na.rm = TRUE),
#               med_val = median(value, na.rm = TRUE)
#             ) |>
#             as_tibble(.) |>
#             pivot_longer(cols = 2:4, names_to = "stat", values_to = "value") |>
#             mutate(value = ifelse(!is.finite(value), NA, value)) 
#         } else {
#           
#           df <- filter(df, !is.na(value))
#         }
#         
#         p <- ggplot(df) +
#           geom_line(aes(x = datetime, y = value), colour = "blue3") 
#       }
#       
#       if(input$lock_y == FALSE) {
#         p <- p +
#           lims(y = c(ranges$y[1], ranges$y[2]))
#       }
#       
#       p +
#         labs(y = input$y_var_select, x = NULL) +
#         theme_bw() +  # Apply the black-and-white theme
#         theme(
#           text = element_text(size = 14),       # Set overall text size
#           plot.title = element_text(size = 18),  # Set title size
#           axis.title = element_text(size = 16),  # Set axis titles size
#           axis.text = element_text(size = 12)    # Set axis tick labels size
#         )
#     })
#     
#     # Zoom when double-clicking inside brushed area
#     observeEvent(input$plot_dblclick, {
#       brush <- input$plot_brush
#       if (is.null(brush)) {
#         # Reset zoom if no brush selection is present
#         ranges$x <- range(data[[col_dt]], na.rm = TRUE)
#         ranges$y <- range(data[[input$y_var_select]], na.rm = TRUE)
#       } else {
#         # Otherwise, update zoom based on brush selection
#         ranges$x <- c(brush$xmin, brush$xmax)
#         ranges$y <- c(brush$ymin, brush$ymax)
#       }
#     })
#     
#     # Show hover coordinates with POSIXct formatted X value
#     output$hover_info <- renderText({
#       if (is.null(input$plot_hover)) return("Hover over the plot to see coordinates")
#       
#       # Convert the x coordinate to POSIXct and format it
#       x_time <- as.POSIXct(input$plot_hover$x, origin = "1970-01-01", tz = "UTC")
#       x_time_formatted <- format(x_time, "%Y-%m-%d %H:%M:%S")
#       
#       # Return formatted X and Y hover coordinates
#       paste0("Cursor position:\n",
#              "Time: ", x_time_formatted, ", ", input$y_var_select, ": ", round(input$plot_hover$y, 2))
#     })
#     
#     # Show click coordinates with POSIXct formatted X value
#     output$click_info <- renderText({
#       if (is.null(input$plot_click)) return("Click on plot to get location data")
#       
#       # Convert the x coordinate to POSIXct and format it
#       x_time <- as.POSIXct(input$plot_click$x, origin = "1970-01-01", tz = "UTC")
#       x_time_formatted <- format(x_time, "%Y-%m-%d %H:%M:%S")
#       
#       # Return formatted X and Y hover coordinates
#       paste0("Plot click:\n",
#              "Time: ", x_time_formatted, ", ", input$y_var_select, " ", round(input$plot_click$y, 2))
#     })
#     
#     
#     # Display brush information (xmin, xmax, ymin, ymax) when a brush selection is made
#     output$brush_info <- renderText({
#       if (is.null(input$plot_brush)) {
#         
#         return("No brush selection")
#       } else {
#         # Convert xmin and xmax to POSIXct and format them
#         xmin_time <- as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01", tz = "UTC")
#         xmax_time <- as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01", tz = "UTC")
#         
#         # Format the brush info
#         brush_info <- paste0(
#           "Selected area:\n",
#           'date_range = c("', 
#           format(xmin_time, "%Y-%m-%d %H:%M:%S"), '", "', format(xmax_time, "%Y-%m-%d %H:%M:%S"), '")\n',
#           "value_range = c(", round(input$plot_brush$ymin, 2), ", ", round(input$plot_brush$ymax, 2),")\n",
#           "(Double-click in the brushed area to zoom in to data)"
#         )
#         return(brush_info)
#       }
#     })
#     
#     # Close the gadget when the "Done" button is clicked
#     observeEvent(input$done_btn, {
#       stopApp()  # This will close the Shiny gadget
#     })
#   }
#   
#   # Run the app using runGadget, ensuring it works properly in RStudio Viewer
#   shiny::runGadget(shinyApp(ui, server), viewer = shiny::paneViewer())
# }

# get the name of the timestamp column
find_posixct_column <- function(data) {
  
  # find columns with class 'POSIXct'
  posix_columns <- which(sapply(data, function(x) inherits(x, "POSIXct")))
  
  # check if more than one column is found
  if (length(posix_columns) > 1) {
    stop("Error: More than one column with class 'POSIXct' found.")
  }
  
  # return the column name if exactly one column is found
  if (length(posix_columns) == 1) {
    return(names(data)[posix_columns])
  } else {
    stop("Error: Could not find a column with class 'POSIXct'. Please check your data")
  }
}

# Function to update qc_code based on a condition
update_qc_code <- function(data, var_ref_id, date_range, value_range,
                           qc_code, filter_type = c("date", "value", "both")) {
  
  filter_type <- match.arg(filter_type)
  sel_vars <- var_ref_id
  tzone <- lubridate::tz(data$datetime)
  
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], tzone = tzone)
  } else {
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  if (missing(value_range)) {
    value_range <- range(data[["qc_value"]], na.rm = TRUE)
  }
  
  new_qc_code <- check_qc_code(qc_code)
  
  # Apply filtering logic based on filter_type
  date_condition <- data$datetime %within% date_range
  value_condition <- data$qc_value >= value_range[1] & data$qc_value < value_range[2]
  
  apply_condition <- dplyr::case_when(
    filter_type == "date"  ~ date_condition,
    filter_type == "value" ~ value_condition,
    filter_type == "both"  ~ date_condition & value_condition
  )
  
  data |> 
    dplyr::mutate(
      qc_code = dplyr::case_when(
        apply_condition & var_ref_id %in% sel_vars ~ new_qc_code,
        TRUE ~ qc_code
      ),
      qc_value = dplyr::case_when(
        apply_condition & var_ref_id %in% sel_vars & 
          new_qc_code == "QC 200" ~ NA_real_,
        TRUE ~ qc_value
      )
    )
}

# Function to update qc_code based on a condition
update_qc_code_vectorized <- function(data, qc_update_df, var_ref_id = NULL) {
  # Ensure datetime is POSIXct
  data <- data |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime))
  
  # Vectors to hold updated values
  new_qc_code_vec <- data |> 
    dplyr::pull(qc_code) 
  new_qc_value_vec <- data |> 
    dplyr::pull(qc_value)
  
  if (!is.null(var_ref_id)) {
    sel_var <- var_ref_id
    qc_update_df <- qc_update_df |>
      dplyr::filter(var_ref_id %in% sel_var)
  }
  
  # Subset the qc_update_df to only include var_ref_id in data
  qc_update_df <- qc_update_df |>
    dplyr::filter(var_ref_id %in% data$var_ref_id)
  if (nrow(qc_update_df) == 0) {
    warning("No matching var_ref_id found in qc_update_df. No updates applied.")
    return(data)
  }
  
  for (i in seq_len(nrow(qc_update_df))) {
    rule <- qc_update_df[i, ]
    
    # Parse date range
    date_start <- lubridate::ymd_hms(rule$date_min, quiet = TRUE)
    date_end <- lubridate::ymd_hms(rule$date_max, quiet = TRUE)
    date_range <- lubridate::interval(date_start, date_end)
    
    # Parse value range
    value_min <- rule$value_min
    value_max <- rule$value_max
    
    # Build condition masks
    date_condition <- data$datetime %within% date_range
    
    value_condition <- if (!is.na(value_min) && !is.na(value_max)) {
      data$qc_value >= value_min & data$qc_value < value_max
    } else {
      rep(TRUE, nrow(data))
    }
    
    var_condition <- data$var_ref_id == rule$var_ref_id
    
    apply_condition <- dplyr::case_when(
      rule$filter_type == "date"  ~ date_condition,
      rule$filter_type == "value" ~ value_condition,
      rule$filter_type == "both"  ~ date_condition & value_condition
    ) & var_condition
    
    # Apply updates
    new_qc_code_vec[apply_condition] <- rule$qc_code
    new_qc_value_vec[apply_condition & rule$qc_code == "QC 200"] <- NA_real_
  }
  
  # Return updated data
  data |>
    dplyr::mutate(
      qc_code = new_qc_code_vec,
      qc_value = new_qc_value_vec
    )
}



check_qc_code <- function(qc_code) {
  
  qc_codes <- c(#"Missing Record" = "QC 100",
    "No Quality or Non Verified" = "QC 200",
    "Synthetic" = "QC 300",
    "Poor Quality" = "QC 400",
    "Fair Quality" = "QC 500",
    "Good Quality" = "QC 600")
  
  if (qc_code %in% qc_codes) {
    return(qc_codes[qc_codes == qc_code])
  } else {
    stop("Error: Invalid qc_code. Please choose from the following options: ",
         paste(qc_codes, collapse = ", "))
  }
}


# Shiny app to visualise data
viz_data <- function(data, qc_update_df = NULL, long = TRUE, variable = "var_ref_id",
                     site_events = NULL,
                     value = "qc_value") {
  
  # Which column is the dates (check for POSIXct)
  col_dt <- find_posixct_column(data)
  
  qc_codes <- c("Missing Record" = "QC 100",
                "No Quality or Non Verified" = "QC 200",
                "Synthetic" = "QC 300",
                "Poor Quality" = "QC 400",
                "Fair Quality" = "QC 500",
                "Good Quality" = "QC 600")
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  qc_code_filter <- c("Date" = "date", "Value" = "value", "Both" = "both")
  
  # set initial variable if none supplied
  # if(is.null(y_var)) { y_var = names(data)[sapply(data, is.numeric)][1] }
  
  # Add label column to site_events
  if (!is.null(site_events)) {
    site_events <- site_events |>
      dplyr::mutate(label = paste0(variables, ": ", comments)) 
  }
  
  
  # Get the list of available variables (exclude the DateTime column)
  if (!long) {

    data <- tidyr::pivot_longer(data, cols = -col_dt, names_to = "variable",
                                values_to = "value") |> 
      dplyr::mutate(qc_code = "QC 200") |> 
      dplyr::rename(!!sym(variable) := variable,
                    !!sym(value) := value)
  }
  available_vars <- unique(data[[variable]])
  
  
  # arrange available_vars in alphabetical order
  available_vars <- sort(available_vars)
  
  if (is.null(qc_update_df)) {
    qc_update_df <- tibble::tibble(
      var_ref_id = character(),
      date_min = as.POSIXct(character()),
      date_max = as.POSIXct(character()),
      value_min = numeric(),
      value_max = numeric(),
      filter_type = character()
    )
  }
  
  ui <- miniUI::miniPage(  # miniPage ensures it works well in RStudio Viewer
    miniUI::gadgetTitleBar("Interactive Plot", left = NULL, 
                           right = shiny::actionButton("done_btn", "Done")),
    shiny::br(),
    shiny::fluidRow(
      # shiny::column(1),
      shiny::column(3,
                    # Dropdown menu to select the variable to plot (exclude DateTime)
                    shiny::selectInput("y_var_select", "Display variable", 
                                       choices = available_vars, 
                                       selected = available_vars[1])
      ),
      shiny::column(3,
                    # Dropdown menu for locking the y-axis zoom
                    shiny::radioButtons("lock_y", "Lock y-axis", 
                                        choices = c("Yes" = TRUE, "No" = FALSE),
                                        selected = TRUE, inline = TRUE),
                    shiny::checkboxInput("add_raw", "Add Raw Data", value = FALSE),
                    shiny::checkboxInput("add_site_events", "Add site events", value = FALSE),
                    shiny::checkboxInput("add_site_events_text", "Add site events text", value = FALSE)
      ),
      shiny::column(3,
                    # Radio button to select the plot type (line or point)
                    shiny::radioButtons("plot_type", "Plot type",
                                        choices = c("Line" = "line", "Point" = "point"),
                                        selected = "point", inline = T),
                    shiny::radioButtons("plot_col", "Colour by",
                                        choices = c("QC Code" = "qc_code", 
                                                    "Device id" = "device_id"),
                                        selected = "qc_code", inline = TRUE),
      ),
      shiny::column(3,
                    # Dropdown menu to select the colour variable
                    shiny::radioButtons("qc_code", "QC Code",
                                        choices = qc_codes, 
                                        selected = qc_codes[2], inline = TRUE),
                    shiny::radioButtons("qc_code_filter", "QC Code Filter", 
                                        choices = qc_code_filter, 
                                        selected = "both", inline = TRUE),
                    shiny::actionButton("update_qc_code", "Update QC Code")
      )
    ),
    shiny::fluidRow(
      shiny::column(1),
      shiny::column(10,
                    shiny::plotOutput("myplot",
                                      click = "plot_click",
                                      dblclick = "plot_dblclick",
                                      brush = shiny::brushOpts(id = "plot_brush", 
                                                               resetOnNew = TRUE),
                                      hover = shiny::hoverOpts(id = "plot_hover", 
                                                               delay = 100, 
                                                               delayType = "debounce")),
                    
                    shiny::verbatimTextOutput("hover_info"),
                    shiny::verbatimTextOutput("click_info"),
                    shiny::verbatimTextOutput("brush_info"),  # Text output for the brush info
                    shiny::verbatimTextOutput("brush_median"),  # Text output for the brush info
                    shiny::verbatimTextOutput("r_log")
      ),
      shiny::column(1)
    )
  )
  
  #' server
  server <- function(input, output, session) {
    
    # Ensure variables exist in the dataset
    shiny::req(col_dt %in% names(data), value %in% names(data))
    
    rv <- reactiveValues(data = data, site_events = site_events)
    
    log <- reactiveValues(text = "data",
                          qc_update_df = qc_update_df)
    
    
    # Set initial zoom range
    ranges <- shiny::reactiveValues(
      x = range(data[[col_dt]], na.rm = TRUE),
      y = range(data[[value]], na.rm = TRUE)
    )
    
    # Plot
    output$myplot <- shiny::renderPlot({
      
      # Filter data based on the current zoom range
      df <- rv$data |>
        dplyr::rename(#"value" = input$y_var_select,
          "datetime" = col_dt) |> 
        dplyr::filter(datetime >= ranges$x[1],
                      datetime <= ranges$x[2],
                      !!sym(variable) == input$y_var_select,
                      !is.na(!!sym(value))
        ) 
      
      p <- ggplot2::ggplot()
      
      if (!is.null(rv$site_events) & input$add_site_events) {
        site_evs <- rv$site_events |>
          dplyr::filter(date >= ranges$x[1],
                        date <= ranges$x[2]) 
        
        p <- p +
          ggplot2::geom_vline(data = site_evs,
                              ggplot2::aes(xintercept = date, 
                                           linetype = action)) 
        
        if (input$add_site_events_text) {
          
          y_pos <- quantile(df[[value]], 0.05, na.rm = TRUE)
          p <- p +
            # ggplot2::geom_text(data = site_evs,
            #                    ggplot2::aes(x = date, y = y_pos,
            #                                 label = comments))
            ggrepel::geom_text_repel(data = site_evs,
                              ggplot2::aes(x = date, y = y_pos,
                                           label = label))
        }
        
      }
      
      
      if (input$add_raw) {
        p <- p + scattermore::geom_scattermore(data = df,
                                               ggplot2::aes(x = datetime, 
                                                            y = raw_value,
                                                            colour = "Raw"), 
                                               pointsize = 1.5)
      }
      
      # Choose plot type based on user input
      if (input$plot_type == "point") {
        
        p <- p +
          scattermore::geom_scattermore(data = df, 
                                        ggplot2::aes(x = datetime, y = .data[[value]],
                                                     colour = .data[[input$plot_col]]),
                                        pixels = c(1024,512), pointsize = 2)
        
      } else {
        
        # Standardize to 1000 points or so for quick plotting
        rows = nrow(df)
        
        if (rows > 3000) {
          
          agg = (as.numeric(max(df$datetime)) - as.numeric(min(df$datetime))) / 1000
          
          df <- rv$data |>
            dplyr::rename(
              "val" = !!sym(value),
              "datetime" = col_dt,
              "colour" = !!sym(input$plot_col)
            ) |>
            dplyr::filter(datetime >= ranges$x[1],
                          datetime <= ranges$x[2],
                          !!sym(variable) == input$y_var_select) |>
            dplyr::select(datetime, val, input$plot_col) |>
            dplyr::mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / agg) *
                                                  agg, origin = "1970-01-01")) |>
            dtplyr::lazy_dt() |>
            dplyr::group_by(datetime) |>
            dplyr::summarise(
              min_val = min(val, na.rm = TRUE),
              max_val = max(val, na.rm = TRUE),
              med_val = median(val, na.rm = TRUE),
              colour = colour[1],
              .groups = "drop"
            ) |>
            dplyr::as_tibble() |>
            tidyr::pivot_longer(cols = c(min_val, max_val, med_val), names_to = "stat",
                                values_to = "value") |>
            dplyr::mutate(value = ifelse(!is.finite(value), NA, value)) 
        } else {
          
          df <- dplyr::filter(df, !is.na(value))
        }
        
        p <- ggplot2::ggplot(df) +
          ggplot2::geom_line(ggplot2::aes(x = datetime, y = value, 
                                          colour = colour)) 
      }
      
      if(input$lock_y == FALSE) {
        p <- p +
          ggplot2::lims(y = c(ranges$y[1], ranges$y[2]))
      }
      
      if (input$plot_col == "qc_code") {
        p <- p + ggplot2::scale_colour_manual(values = qc_code_col_scale)
      }
      
      p +
        ggplot2::labs(y = input$y_var_select, x = NULL) +
        coord_cartesian(xlim = ranges$x) +
        ggplot2::theme_bw() +  # Apply the black-and-white theme
        ggplot2::theme(
          text = ggplot2::element_text(size = 14),       # Set overall text size
          plot.title = ggplot2::element_text(size = 18),  # Set title size
          axis.title = ggplot2::element_text(size = 16),  # Set axis titles size
          axis.text = ggplot2::element_text(size = 12)    # Set axis tick labels size
        )
    })
    
    # Zoom when double-clicking inside brushed area
    shiny::observeEvent(input$plot_dblclick, {
      brush <- input$plot_brush
      if (is.null(brush)) {
        # Reset zoom if no brush selection is present
        ranges$x <- range(rv$data[[col_dt]], na.rm = TRUE)
        rnge_chk <- range(rv$data[[input$y_var_select]], na.rm = TRUE)
        if (any(is.infinite(rnge_chk))) {
          ranges$y <- range(data[[input$y_var_select]], na.rm = TRUE, finite = TRUE)
        } else {
          ranges$y <- rnge_chk
        }
      } else {
        # Otherwise, update zoom based on brush selection
        ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax))
        ranges$y <- c(brush$ymin, brush$ymax)
      }
    })
    
    # Update qc_code based on ranges$x
    shiny::observeEvent(input$update_qc_code, {
      # Convert xmin and xmax to POSIXct and format them
      xmin_time <- as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01", tz = "UTC")
      xmax_time <- as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01", tz = "UTC")
      
      y_min <- plyr::round_any(input$plot_brush$ymin, 0.01)
      y_max <- plyr::round_any(input$plot_brush$ymax, 0.01)
      
      strng <- paste0("update_qc_code(var_ref_id = \"", input$y_var_select, "\", ")
      
      strng2 <- dplyr::case_when(
        input$qc_code_filter == "date"  ~ paste0("date_range = c(\"", 
                                                 format(xmin_time, "%Y-%m-%d %H:%M:%S"), "\", \"", 
                                                 format(xmax_time, "%Y-%m-%d %H:%M:%S"), "\"), "),
        input$qc_code_filter == "value" ~ paste0("value_range = c(", y_min, ", ", y_max, "), "),
        input$qc_code_filter == "both"  ~ paste0("date_range = c(\"", 
                                                 format(xmin_time, "%Y-%m-%d %H:%M:%S"), "\", \"", format(xmax_time, "%Y-%m-%d %H:%M:%S"), "\"),
                                value_range = c(", y_min, ", ", y_max, "), ")
      )
      
      strng_total <- paste0(strng, strng2, "filter_type = \"", input$qc_code_filter, "\", qc_code = \"", input$qc_code, "\")")
      
      df <- tibble::tibble(
        var_ref_id = input$y_var_select,
        date_min = xmin_time,
        date_max = xmax_time,
        value_min = y_min,
        value_max = y_max,
        qc_code = input$qc_code,
        filter_type = input$qc_code_filter
      )
      
      log$qc_update_df <- dplyr::bind_rows(log$qc_update_df, df)
      
      log$text <- c(log$text, strng_total)
      
      
      rv$data <- update_qc_code(data = rv$data, var_ref_id = input$y_var_select, 
                                date_range = c(xmin_time, xmax_time),
                                value_range = c(input$plot_brush$ymin,
                                                input$plot_brush$ymax),
                                filter_type = input$qc_code_filter,
                                qc_code =  input$qc_code)
    })
    
    # Show hover coordinates with POSIXct formatted X value
    output$hover_info <- shiny::renderText({
      if (is.null(input$plot_hover)) return("Hover over the plot to see coordinates")
      
      # Convert the x coordinate to POSIXct and format it
      x_time <- as.POSIXct(input$plot_hover$x, origin = "1970-01-01", tz = "UTC")
      x_time_formatted <- format(x_time, "%Y-%m-%d %H:%M:%S")
      
      # Return formatted X and Y hover coordinates
      paste0("Cursor position:\n",
             "Time: ", x_time_formatted, ", ", input$y_var_select, ": ", round(input$plot_hover$y, 2))
    })
    
    # Show click coordinates with POSIXct formatted X value
    output$click_info <- shiny::renderText({
      if (is.null(input$plot_click)) return("Click on plot to get location data")
      
      # Convert the x coordinate to POSIXct and format it
      x_time <- as.POSIXct(input$plot_click$x, origin = "1970-01-01", tz = "UTC")
      x_time_formatted <- format(x_time, "%Y-%m-%d %H:%M:%S")
      
      # Return formatted X and Y hover coordinates
      paste0("Plot click:\n",
             "Time: ", x_time_formatted, ", ", input$y_var_select, " ", round(input$plot_click$y, 2))
    })
    
    
    # Display brush information (xmin, xmax, ymin, ymax) when a brush selection is made
    output$brush_info <- shiny::renderText({
      if (is.null(input$plot_brush)) {
        
        return("No brush selection")
      } else {
        # Convert xmin and xmax to POSIXct and format them
        xmin_time <- as.POSIXct(input$plot_brush$xmin, origin = "1970-01-01", tz = "UTC")
        xmax_time <- as.POSIXct(input$plot_brush$xmax, origin = "1970-01-01", tz = "UTC")
        
        # Format the brush info
        brush_info <- paste0(
          "Selected area:\n",
          'date_range = c("', 
          format(xmin_time, "%Y-%m-%d %H:%M:%S"), '", "', format(xmax_time, "%Y-%m-%d %H:%M:%S"), '")\n',
          "value_range = c(", round(input$plot_brush$ymin, 2), ", ", round(input$plot_brush$ymax, 2),")\n",
          "(Double-click in the brushed area to zoom in to data)"
        )
        return(brush_info)
      }
    })
    
    # Display median values within the brush
    output$brush_median <- shiny::renderText({
      if (is.null(input$plot_brush)) {
        return("No brush selection")
      } else {
        # Filter data based on the current zoom range
        df <- rv$data |>
          # dplyr::rename("sel_value" = input$y_var_select,
          #               "datetime" = col_dt) |>
          dplyr::filter(datetime >= input$plot_brush$xmin,
                        datetime <= input$plot_brush$xmax,
                        !!sym(value) >= input$plot_brush$ymin,
                        !!sym(value) <= input$plot_brush$ymax,
                        var_ref_id == input$y_var_select,
                        !is.na(!!sym(value))
          )
        
        # Calculate median value
        median_val <- median(df[[value]], na.rm = TRUE)
        
        # Return the median value
        paste0("Median value within the brush: ", round(median_val, 2))
      }
    })
    
    output$r_log <- shiny::renderText({
      paste0(log$text, collapse = " |>\n")
    })
    
    # Capture updated dataframe when app closes
    onStop(function() {
      # updated_df <<- isolate(rv$data)
    })
    
    # Close the gadget when the "Done" button is clicked
    shiny::observeEvent(input$done_btn, {
      # updated_df <<- isolate(rv$data)
      # shiny::stopApp(rv$data)  # This will close the Shiny gadget
      # shiny::stopApp(log$text)  # This will close the Shiny gadget
      shiny::stopApp(log$qc_update_df)  # This will close the Shiny gadget
    })
  }
  
  # Run the app using runGadget, ensuring it works properly in RStudio Viewer
  logs <- shiny::runGadget(shiny::shinyApp(ui, server),
                           viewer = shiny::paneViewer())
  return(logs)
}


low <- c(0, 0, 0)
high <- c(100, 130, 160)
# 
drift_correction <- function(data, var_ref_id, date_range, low, high, 
                             plot = FALSE) {
  
  date_range_orig <- date_range
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = lubridate::tz(data$datetime))
  } else {
    date_range <- coerce_date_range(date_range, tzone = lubridate::tz(data$datetime))
  }
  
  sel_vars <- var_ref_id
  sub_data <- data |> 
    dplyr::filter(
      var_ref_id == sel_vars,
      datetime %within% date_range
    )
  
  # find slope and offset adjustment at start of period
  offsetStart <- low[1] - low[2]
  slopeStart  <- (high[1] - low[1] ) / (high[2] - low[2])
  
  # find slope and offset adjustment at end of period
  offsetEnd <- low[1] - low[3]
  slopeEnd  <- (high[1] - low[1]) / (high[3] - low[3])
  
  # interpolate slope and offset
  sub_data <- sub_data |> 
    dplyr::mutate(
      offsets = seq(offsetStart, offsetEnd, length.out = dplyr::n()),
      slopes = seq(slopeStart, slopeEnd, length.out = dplyr::n()),
      corr_value  = (qc_value * slopes) + offsets,
      qc_code = dplyr::case_when(
        qc_value != corr_value ~ "QC 300",
        TRUE ~ qc_code
      ),
      qc_flag = dplyr::case_when(
        qc_value != corr_value ~ format_flag(qc_flag, "drift_corr"),
        TRUE ~ qc_flag
      )
    )
  
  if (plot) {
    p <- plot_drift_correction(sub_data, var_ref_id, date_range, low, high)
    print(p)
  }
  
  sub_data <- sub_data |> 
    dplyr::mutate(qc_value = corr_value)
  
  # record_qc("drift_correction", date_range_orig, list(var_ref_id = var_ref_id, 
  #                                                     low = low, high = high))
  
  
  # Rejoin the data
  data |> 
    dplyr::anti_join(sub_data, by = c("datetime", "var_ref_id", "device_id",
                                      "raw_value")) |>
    dplyr::bind_rows(sub_data) |> 
    dplyr::arrange(var_ref_id, datetime) |> 
    dplyr::select(datetime, var_ref_id, site, device_id, var_abbr, raw_value, 
                  qc_value, qc_flag, qc_code)
  
  
}

# plot the drift correction
plot_drift_correction <- function(data, var_ref_id, date_range, low, high) {
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  sel_vars <- var_ref_id
  sub_data <- data |> 
    dplyr::filter(
      var_ref_id == sel_vars,
      datetime %within% date_range
    )
  
  if (!"corr_value" %in% sub_data) {
    # find slope and offset adjustment at start of period
    offsetStart <- low[1] - low[2]
    slopeStart  <- (high[1] - low[1] ) / (high[2] - low[2])
    
    start_date <- min(sub_data$datetime)
    end_date <- max(sub_data$datetime)
    
    # find slope and offset adjustment at end of period
    offsetEnd <- low[1] - low[3]
    slopeEnd  <- (high[1] - low[1]) / (high[3] - low[3])
    
    # interpolate slope and offset
    sub_data <- sub_data |> 
      dplyr::mutate(
        offsets = seq(offsetStart, offsetEnd, length.out = dplyr::n()),
        slopes = seq(slopeStart, slopeEnd, length.out = dplyr::n()),
        corr_value  = (qc_value * slopes) + offsets,
        qc_code = dplyr::case_when(
          qc_value != corr_value ~ "QC 300",
          TRUE ~ qc_code
        ),
        qc_flag = dplyr::case_when(
          qc_value != corr_value ~ format_flag(qc_flag, "drift_corr"),
          TRUE ~ qc_flag
        )
      )
  }
  
  p <- ggplot(sub_data) +
    geom_point(aes(datetime, qc_value, colour = "Original")) +
    geom_point(aes(datetime, corr_value, colour = "Drift\ncorrected")) +
    geom_segment(aes(x = min(sub_data$datetime), y = high[2], 
                     xend = max(sub_data$datetime), yend = high[3]),
                 colour = "red", linetype = "dashed") +
    geom_segment(aes(x = min(sub_data$datetime), y = low[2], xend = max(sub_data$datetime), yend = low[3]),
                 colour = "red", linetype = "dashed") +
    theme_bw()
  
  
  return(p)
  
}

# Data frame to log function calls
qc_log <- data.frame(
  function_name = character(),
  timestamp = as.POSIXct(character()),
  date_range = character(),
  arguments = character(),
  stringsAsFactors = FALSE
)

# Function to record function calls
record_qc <- function(func_name, date_range, args) {
  # Ensure qc_log exists
  if (!exists("qc_log", envir = .GlobalEnv)) {
    qc_log <- data.frame(
      function_name = character(),
      timestamp = as.POSIXct(character()),
      date_range = character(),
      arguments = character(),
      stringsAsFactors = FALSE
    )
    assign("qc_log", qc_log, envir = .GlobalEnv)
  }
  
  # Format arguments for easy copy-pasting
  formatted_args <- paste(
    names(args), 
    sapply(args, function(x) paste(deparse(x), collapse = " ")), 
    sep = " = ", 
    collapse = ", "
  )
  
  # Append the new QC record
  new_entry <- data.frame(
    function_name = func_name,
    timestamp = Sys.time(),
    date_range = paste0(date_range, collapse = " to "),
    arguments = formatted_args,
    stringsAsFactors = FALSE
  )
  
  # Append to the global qc_log
  assign("qc_log", rbind(get("qc_log", envir = .GlobalEnv), new_entry), envir = .GlobalEnv)
}

# Function to compare two variables
var_ref_ids <- c("c_do_sat_d100", "c_do_sat_d1000")
compare_variables <- function(data, var_ref_ids) {
  
  var_depths <- sapply(var_ref_ids, function(x) {
    decode_var_ref(x)$value_m
  })
  
  # assign smallest depth to first variable
  if (var_depths[1] > var_depths[2]) {
    var_ref_ids <- rev(var_ref_ids)
  }
  
  
  df <- data |> 
    dplyr::filter(var_ref_id %in% var_ref_ids) |> 
    dplyr::select(datetime, var_ref_id, qc_value) |> 
    tidyr::pivot_wider(names_from = var_ref_id, values_from = qc_value) |> 
    dplyr::mutate(diff = !!sym(var_ref_ids[1]) - !!sym(var_ref_ids[2]))
  
  
  df |> 
    filter(diff < 0) |>
    ggplot() +
    geom_point(aes(x = !!sym(var_ref_ids[1]), y = !!sym(var_ref_ids[2]), colour = diff)) +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw()
  
  df |> 
    filter(diff < 0) |>
    ggplot() +
    geom_point(aes(x = datetime, y = !!sym(var_ref_ids[1]), colour = "1m"), size = 0.1) +
    geom_point(aes(x = datetime, y = !!sym(var_ref_ids[2]), colour = "10m"), size = 0.1) +
    theme_bw()
  
  
  df |> 
    dplyr::mutate(
      # mutate value of smaller depth if diff > -5 and diff <0
      c_do_sat_d100 = dplyr::case_when(
        diff > -5 & diff < 0 ~ c_do_sat_d1000,
        TRUE ~ c_do_sat_d100
      ),
    )
  dplyr::filter(diff < 0) |> 
    summary()
}

# DO saturation calculation from Benson & Krause. Jr. (1980), via In-Situ RDO pro-X manual
calc_DOsat_mg <- function(TmpWtr, PrBaro = 1013.25, Slnty = 0) {
  
  # pressure in atmospheres
  P_atm = PrBaro/1013.25
  
  # water temperature in Kelvin
  T_k = TmpWtr + 273.15
  
  # rho: density of water
  rho = exp( -0.589581 + (326.785/T_k) - (45284.1 / T_k^2) )
  
  # partial pressure of water vapour at saturation
  P_wv = exp( 11.8571 - (3840.70/T_k) - (216961/T_k^2) )
  
  # Henry's constant
  k_o = exp(3.71814 + (5596.17/T_k) - (1049668/T_k^2)  )
  
  # molar mass of water
  M = 18.0152
  
  # negative of the second pressure coefficient
  O_o = 0.000975 - (1.426E-05 * TmpWtr) + (6.436E-8*TmpWtr^2)
  
  # salinity correction
  B0 = -6.246090E-3
  B1 = -7.423444E-3
  B2 = -1.048635E-2
  B3 = -7.987907E-3
  C0 = -4.679983E-7
  T_sc = log((298.15-TmpWtr)/(273.15+TmpWtr))
  
  Sc = exp( Slnty*(B0 + B1*T_sc + B2*T_sc^2 + B3*T_sc^3) + C0*Slnty^2)
  
  # calculate theoretical saturation in mg/L
  31.9988E+06 * (rho*(0.20946 * (P_atm - P_wv))/(k_o*M) * (1-O_o*P_atm) * Sc)
}


# Function to compare temperature at different nodes in a thermistor string
calc_temp_offsets <- function(data) {
  
  temp <- data |> 
    dplyr::filter(var_abbr == "t_wtr")
  
  # Subset temperature when the variation between top and bottom is less than 1
  # degree
  temp2 <- temp |> 
    dplyr::mutate(depth = decode_depth(var_ref_id)) |> 
    dplyr::group_by(datetime) |>
    dplyr::mutate(
      prof_diff = max(qc_value) - min(qc_value),
      depth_diff = qc_value[which.min(depth)] - qc_value
    ) |> 
    dplyr::ungroup() |> 
    dplyr::filter(prof_diff < 1) |> 
    dplyr::mutate(
      year = lubridate::year(datetime)
    ) |> 
    dplyr::arrange(datetime, depth) |>
    dplyr::group_by(year) 
  temp2
}

# This function align temperature profiles by finding the times which are likely to be 
# mixed and theoretically should show indicate identical temperatures

# returns a vector of offsets for the temperature columns named 

calc_temp_drift <- function(data, date_range, 
                            names_tmpwtr = "temp", names_tmpwtr_excl = NULL, 
                            pctile_tdiff = 0.1, name_wndspd = NULL,
                            pctile_wind = 0.99) {
  
  # get the columns
  col_dt <- find_posixct_column(data)
  # cols_tmpwtr <- colname_selector(data, vars = names_tmpwtr, 
  #                                 vars_excl = names_tmpwtr_excl)
  
  tzone <- lubridate::tz(data$datetime)
  if (missing(date_range)) {
    date_ranges <- range(data[["datetime"]])
    date_range <- lubridate::interval(date_ranges[1], date_ranges[2], 
                                      tzone = tzone)
  } else {
    x0 <- date_range[1]
    x1 <- date_range[2]
    date_range <- coerce_date_range(date_range, tzone = tzone)
  }
  
  # filter data for when temperature differences are minimised
  df_tmpwtr <- data |>
    dplyr::filter(!!sym(col_dt) %within% date_range,
                  var_abbr == "t_wtr") |>
    dplyr::mutate(depth = decode_depth(var_ref_id),
                  year = lubridate::year(!!sym(col_dt))) |> 
    dplyr::arrange(!!sym(col_dt), depth) |>
    dplyr::group_by(!!sym(col_dt)) |>
    dplyr::mutate(
      avg_diff = mean(abs(diff(qc_value, na.rm = FALSE)))
    ) |>
    dplyr::filter(!is.na(avg_diff)) |> 
    dplyr::ungroup()
  
  # q_val <- quantile(df_tmpwtr$avg_diff, pctile_tdiff, na.rm = TRUE)
  
  
  sub <- df_tmpwtr |> 
    dplyr::group_by(year) |>
    dplyr::filter(avg_diff < quantile(avg_diff, pctile_tdiff, na.rm = TRUE))
  
  summary(sub)
  sub |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(n = dplyr::n())
  
  # if (!is.null(name_wndspd)) {
  #   col_wndspd = colname_selector(data, vars = name_windspd, exact = TRUE)
  #   
  #   # filter for windspeed if supplied
  #   df_tmpwtr <- df_tmpwtr |>
  #     dplyr::left_join(df[,c(col_dt, col_wndspd)], by = col_dt) |>
  #     dplyr::filter(.data[[col_wndspd]] > quantile(df[,col_wndspd], pctile_wind, na.rm = T)) |>
  #     dplyr::select(-all_of(col_wndspd) )
  # }
  
  # p <- sub |>
  #   # dplyr::select(-col_dt) |>
  #   dplyr::group_by(!!sym(col_dt)) |>
  #   dplyr::mutate(median_all = median(qc_value)) |>
  #   # tidyr::pivot_longer(cols = -median_all,  # Exclude median_all column from pivoting
  #   #                     names_to = "variable",  # New column for variable names
  #   #                     values_to = "value") |>
  #   # dplyr::filter(value > 0 & value < 50) |>
  #   ggplot() +
  #   geom_point(aes(median_all, qc_value, colour = factor(depth)), shape = 1) +
  #   geom_abline(intercept = 0, slope = 1, colour = "black", linetype = "dashed") +
  #   theme_bw()
  # print(p)
  
  # get the average difference from median temperature
  sub <- sub |>
    # dplyr::select(-col_dt) |>
    dplyr::group_by(!!sym(col_dt)) |>
    dplyr::mutate(median_all = median(qc_value)) |> 
    dplyr::ungroup()
  
  # Extract the intercept (offset) and slope (multiplier) from each depth
  out <- sub |> 
    # dplyr::filter(!depth %in% c(1, 10.5), year != 2021) |>
    dplyr::group_by(year, depth) |>
    dplyr::summarise(
      lm_model = list(lm(qc_value ~ median_all)),
      offset = coef(lm_model[[1]])[1],  # Intercept
      slope = coef(lm_model[[1]])[2],  # Slope
      n_profiles = length(unique(datetime))
    ) |> 
    dplyr::select(-lm_model) |> 
    as.data.frame() |> 
    # Calculate temperature at 10 and 25
    dplyr::mutate(
      t_10 = offset + slope * 10,
      t_25 = offset + slope * 25
    ) 
  out
  return(out)
  
  
  p1 <- ggplot(out) +
    geom_line(aes(year, offset, colour = factor(depth)), shape = 1) +
    scale_colour_viridis_d() +
    theme_bw()
  
  p2 <- ggplot(out) +
    geom_line(aes(year, slope, colour = factor(depth)), shape = 1) +
    scale_colour_viridis_d() +
    theme_bw()
  
  ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom") +
    ggplot2::labs(x = "Year", y = "Offset / Slope") 
  
  return(list(timeframe = date_range, offsets = offsets, slopes = slopes ) )
}

plot_temp_drift <- function(data, base_size = 16) {
  
  depths <- unique(data$depth)
  ord_depths <- depths[order(depths)]
  
  data <- data |> 
    dplyr::mutate(depth = factor(depth, levels = ord_depths)) 
  
  p1 <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(year, t_10, colour = depth),  shape = 1) +
    ggplot2::geom_hline(yintercept = 9.9,colour = "black", 
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 10.1,colour = "black",
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 10,colour = "black") +
    ggplot2::labs(x = "Year", y = "Temperature", colour = "Depth (m)") +
    ggplot2::scale_colour_viridis_d(end = 0.9) +
    ggplot2::theme_bw(base_size = base_size)
  
  p2 <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(year, t_25, colour = depth), shape = 1) +
    ggplot2::geom_hline(yintercept = 24.9,colour = "black",
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 25.1,colour = "black", 
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 25,colour = "black") +
    ggplot2::labs(x = "Year", y = "Temperature", colour = "Depth (m)") +
    ggplot2::scale_colour_viridis_d(end = 0.9) +
    ggplot2::theme_bw(base_size = base_size)
  
  ggpubr::ggarrange(p2, p1, ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom")
} 

plot_site_events <- function(site_events, sensor_map) {
  
  site_events <- site_events |> 
    map_events_device(sensor_map) |> 
    dplyr::arrange(date) |>
    dplyr::mutate(index = 1:dplyr::n(), year = lubridate::year(date),
                  doy = lubridate::yday(date)) 
  
  ggplot2::ggplot(data = site_events) +
    ggplot2::geom_point(ggplot2::aes(x = date, y = device_id, colour = action), 
                       size = 5, shape = "|") +
    ggplot2::facet_wrap(~year, scales = "free_x")
    
  
  
  
  ggplot2::ggplot(data = site_events) +
    # ggplot2::geom_vline(ggplot2::aes(xintercept = doy), alpha = 0.2) +
    ggplot2::geom_text(ggplot2::aes(x = 0, y = index, label = date), 
                       size = 3, hjust = 0) +
    ggplot2::geom_text(ggplot2::aes(x = 0.3, y = index, label = comments), 
                       size = 3, hjust = 0) +
    # ggplot2::facet_grid(ggplot2::vars(year), 
    #             scales = "free_y", switch = "y") +
    coord_cartesian(xlim = c(0, 1)) +
    ggplot2::facet_wrap(year ~ device_id, scales = "free",
               ncol = 2, strip.position = "left") +
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Date", y = "Comments") +
    # Move facet labels to y
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
      panel.grid.minor.x = ggplot2::element_line(colour = "grey80"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) 
  
}

map_sensors <- function(site_devices, device_var, device_position) {
  sensor_map <- site_devices |> 
    dplyr::select(site, device_id, date_from, date_to) |> 
    dplyr::left_join(device_var, by = "device_id") |> 
    dplyr::left_join(device_position, by = c("site", "device_id")) |>
    dplyr::mutate(var_ref_id = generate_var_ref(var_abbr, z_relative, reference)) |> 
    dplyr::arrange(var_ref_id, date_from)
  return(sensor_map)
}

map_events_device <- function(site_events, sensor_map) {
  
  sensor_map <- sensor_map |> 
    dplyr::mutate(date_interval = lubridate::interval(date_from, date_to)) |> 
    dplyr::arrange(date_from)
  
  
  
  out <- lapply(1:nrow(site_events), \(i) {
    
    if (site_events$variables[i] == "all") {
      all_devices <- sensor_map |> 
        dplyr::filter(site_events$date[i] %within% date_interval) |> 
        dplyr::select(device_id, var_ref_id) |> 
        dplyr::distinct()
    } else {
      vars <- stringr::str_split(site_events$variables[i], ", ")[[1]]
      all_devices <- sensor_map |> 
        dplyr::filter(site_events$date[i] %within% date_interval,
                      grepl(paste0(vars, collapse = "|"), var_abbr) | 
                        grepl(paste0(vars, collapse = "|"), var_ref_id)) |> 
        dplyr::select(device_id, var_ref_id) |> 
        dplyr::distinct()
    }
    
    all_devices |> 
      dplyr::mutate(
        site = site_events$site[i],
        date = site_events$date[i],
        site_visit = site_events$site_visit[i],
        variables = site_events$variables[i],
        action = site_events$action[i],
        comments = site_events$comments[i]
      )
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::select(dplyr::all_of(c(names(site_events), "device_id", "var_ref_id")))
  return(out)
}

plot_sensor_timeline <- function(sensor_map, variable_ref, site_events, 
                                 add_faults = TRUE) {
  
  if (add_faults) {
    sensor_faults <- site_events |> 
      dplyr::filter(grepl("fault", action)) |> 
      map_events_device(sensor_map)
  }
  
  sensors <- sensor_map |> 
    dplyr::left_join(variable_ref, by = c("var_abbr" = "abbr")) |>
    dplyr::mutate(
      label = dplyr::case_when(
        type == "met" ~ "Met",
        .default = label
      )
    ) |> 
    dplyr::group_by(device_id, date_from, date_to) |> 
    dplyr::summarise(label = paste(unique(label), collapse = " +\n"),
                     .groups = "drop") |> 
    dplyr::mutate(label = dplyr::if_else(label == "NA", NA, label)) |> 
    dplyr::arrange(label) |> 
    dplyr::mutate(label = factor(label))
  sensors$device_id <- factor(sensors$device_id, unique(sensors$device_id))
  p <- sensors |> 
    ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x = date_from, xend = date_to, 
                                       y = device_id, yend = device_id,
                                       colour = label)) +
    ggplot2::geom_point(ggplot2::aes(x = date_from, y = device_id,
                                     colour = label), size = 3, shape = "|") +
    ggplot2::geom_point(ggplot2::aes(x = date_to, y = device_id,
                                     colour = label), size = 3, shape = 9) +
    ggplot2::scale_colour_brewer(palette = "Dark2", na.value = "grey") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Date", y = "Sensor", colour = "Variable") 
  
  if (add_faults) {
    p <- p +
      ggplot2::geom_point(data = sensor_faults,
                         ggplot2::aes(x = date, y = device_id), , colour = "black",
                         size = 2, shape = "|")
  }
  return(p)
}

map_data_to_devices <- function(data, site_devices, device_var, 
                                device_position, variables = NULL) {
  sensor_map <- map_sensors(site_devices = site_devices, 
                            device_var = device_var, 
                            device_position = device_position)
  
  if (!is.null(variables)) {
    variables <- variables |> 
      dplyr::select(abbr, label)
    sensor_map <- sensor_map |> 
      dplyr::left_join(variables, by = c("var_abbr" = "abbr"))
  }
  
  sensor_map |> 
    dplyr::group_by(device_id) |> 
    dplyr::left_join(data, by = c("var_ref_id", "site"), 
                     relationship = "many-to-many") |>
    dplyr::filter(datetime >= date_from & datetime <= date_to) |> 
    dplyr::ungroup() |> 
    dplyr::arrange(var_ref_id, datetime) |> 
    dplyr::select(-c(date_from, date_to, reference, z_relative))
  
}

plot_raw_qc <- function(data, out_dir = "qc_plots", days_offset = 184, 
                        ylim = NULL, variable_ref) {
  
  qc_code_col_scale = c(
    "QC 100" = "#FF0000",
    "QC 200" = "#8B5A00",
    "QC 300" = "#D3D3D3",
    "QC 400" = "#FFA500",
    "QC 500" = "#00BFFF",
    "QC 600" = "#006400"
  )
  
  dir.create(out_dir, showWarnings = FALSE)
  
  sel_vars <- data |> 
    dplyr::select(var_ref_id, var_abbr) |> 
    dplyr::distinct() |> 
    dplyr::pull(var_ref_id)
  
  v <- sel_vars[1]
  for (v in sel_vars) {
    
    df <- data |> 
      dplyr::filter(var_ref_id == v) |> 
      dplyr::mutate(
        doy = lubridate::yday(datetime + lubridate::ddays(days_offset)),
        year = lubridate::year(datetime + lubridate::ddays(days_offset)),
        year = factor(year),
        title = make_var_name(var_ref = v, variable_ref = variable_ref)
      )
    
    n_years <- length(unique(df$year))
    
    min_y <- min(df$qc_value, df$raw_value, na.rm = TRUE)
    max_y <- max(df$qc_value, df$raw_value, na.rm = TRUE)
    adj_y <- min_y - 0.01 * (max_y - min_y)
    height <- 0.05 * (max_y - min_y)
    
    p <- ggplot2::ggplot(df) +
      ggplot2::geom_point(ggplot2::aes(x = doy, y = raw_value, colour = "Raw"),
                          size = 0.1, shape = 19, na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(x = doy, y = qc_value, colour = "QC"), 
                          size = 0.1, shape = 19, na.rm = TRUE) +
      # ggplot2::geom_raster(ggplot2::aes(x = doy, y = adj_y, fill = qc_code),
      #                      height = height) +
      ggplot2::scale_fill_manual(values = qc_code_col_scale) +
      # scattermore::geom_scattermore(ggplot2::aes(x = doy, y = raw_value, colour = "Raw"),
      #                               pointsize = 2.4, pixels = c(600, 3000), alpha = 0.5) +
      # scattermore::geom_scattermore(ggplot2::aes(x = doy, y = qc_value, colour = "QC"), pointsize = 3) +
      # ggplot2::facet_wrap(~year, ncol = 1, scales = "free_x") +
      ggplot2::facet_grid(year ~ title, scales = "free_x") +
      ggplot2::scale_colour_manual(values = c("Raw" = "red", "QC" = "black")) +
      # ggplot2::labs(title = title) +
      ggplot2::theme_bw(base_size = 6) +
      # remove x and y guide lines
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.title = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(size = 12),
        strip.text.y = ggplot2::element_text(size = 8)
      )
      # p
    
    if (!is.null(ylim)) {
      p <- p + 
        ggplot2::coord_cartesian(ylim = ylim)
    }
    
    # Save to png with ggsave
    plot_file <- file.path(out_dir, paste0("qc_", v, ".png"))
    ggsave(plot_file, plot = p, height = 11.7, width = 8.3,
           units = "in", dpi = 300)
    
    
    # Plot to A4 pdf
    # pdf(paste0("qc_", v, ".pdf"), height = 11.7, width = 8.3)
    # print(p)
    # dev.off()
    
    # Plot to png
    # png(paste0("qc_", v, ".png"), height = 11.7, width = 8.3, units = "in", res = 300)
    # print(p)
    # dev.off()
    
    
  }
  
}

generate_readme <- function(metadata_path = "data/metadata/dataspice.json",
                            attributes_path = "data/metadata/attributes.csv",
                            access_path = "data/metadata/access.csv",
                            files_path,
                            output_path,
                            version = "v0.0.1",
                            include_description = TRUE
) {
  
  meta <- metadata_path |> jsonlite::fromJSON()
  attributes <- attributes_path |> readr::read_csv(show_col_types = FALSE)
  access <- access_path |> readr::read_csv(show_col_types = FALSE)
  
  variable_summary <- attributes |>
    dplyr::mutate(desc = paste0(variableName , ": ", description, " [file: ", fileName, ", units: ", unitText, "]")) |>
    dplyr::pull(desc)
  
  variable_summary <- paste("-", variable_summary, collapse = "\n")
  
  authors <- paste0(meta$creator$name, collapse = ", ")
  creator <- meta$creator$name[1]
  contact <- meta$creator$email[1]
  license <- meta$license[1]
  
  description <- meta$description
  
  files <- files_path |>
    list.files(full.names = TRUE) |>
    basename()
  files <- paste0("- ", files) |>
    paste(collapse = "\n")
  
  readme_text <- glue::glue(
    "
DATA PRODUCT README
===================

Title: {meta$name}
Version: {version}
Date Created: {Sys.Date()}
Authors: {authors}
License: {license}

---

DESCRIPTION
-----------
{description}
---

FILES INCLUDED
--------------
{files}
- README.txt

---

VARIABLES
---------
{variable_summary}

---

USAGE
-----
Cite the dataset as:
[Add citation here]

License:
This dataset is shared under a {license} license. See `data/metadata/access.csv` for details.

Contact:
{creator} – {contact}
"
  )
  # cat(readme_text)
  
  
  writeLines(readme_text, file.path(output_path, "README.txt"))
}


