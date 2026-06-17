plot_aeme_loads <- function(aeme, vars = c("HYD_flow", "NIT_amm", "NIT_nit",
                                           "NIT_don", "NIT_pon", "PHS_frp",
                                           "PHS_pop", "PHS_pip"),
                            date_filter = TRUE) {
  df <- AEME::get_inflows(aeme, return_df = TRUE)
  
  if (date_filter) {
    df <- .filter_date(df, aeme)
  }
  plot_loads(df, period = "total", 
             vars = c("HYD_flow", "NIT_amm", "NIT_nit", "NIT_don", "NIT_pon",
                      "PHS_frp", "PHS_pop", "PHS_pip"))
  
}
.filter_date <- function(df, aeme) {
  tme <- AEME::time(aeme)
  t0 <- tme$start - lubridate::ddays(max(unlist(tme$spin_up)))
  t0 <- as.Date(t0)
  t1 <- as.Date(tme$stop)
  
  df |> 
    dplyr::filter(Date >= t0 & Date <= t1)
}

# =============================================================================
# Inflow Load Calculation and Plotting Functions
# =============================================================================
# Load = Flow (m3/day) * Concentration (g/m3 = mg/L) -> result in kg/day
# then aggregated to monthly or annual totals (tonnes/period)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Stream colour palette
#    Fixed colour for each stream (inflow_id), defined once up front so every
#    stream keeps the same colour across every plot, regardless of which
#    variables, time period, or subset of streams is being shown.
#
#    Source pool: the only three RColorBrewer qualitative palettes flagged
#    colourblind-safe (Dark2, Paired, Set2 - see colorblind column of
#    RColorBrewer::brewer.pal.info; Set1, Pastel1/2 and Accent are NOT safe
#    and are deliberately excluded). Even within that safe pool, picking any
#    14 colours isn't automatically fine together - e.g. Dark2's purple
#    (#7570B3) and Paired's blue (#1F78B4) simulate to almost the same colour
#    under deuteranopia/protanopia despite looking distinct normally. The 14
#    colours were chosen by simulating both deficiencies (Machado, Oliveira &
#    Fernandes, 2009) from the pooled 28 candidates and greedily maximising
#    the worst-case distance between every pair, so the whole set stays
#    distinguishable under both conditions, not just under normal vision.
#
#    Assignment is grouped by category rather than alphabetical:
#      - Ungauged-Quick / Ungauged-Slow -> the two greys (modelled, not gauged)
#      - Geothermal                     -> orange (heat-associated)
#      - WWTP                           -> magenta (the one point source -
#                                          made to stand out from the streams)
#      - Rainfall                       -> light blue (direct precipitation)
#      - the 9 named streams            -> remaining hues, alphabetically
STREAM_PALETTE <- c(
  "Rainfall"       = "#A6CEE3",
  "Awahou"         = "#7570B3",
  "Hamurana"       = "#8DA0CB",
  "Ngongotaha"     = "#6A3D9A",
  "Puarenga"       = "#FC8D62",
  "Utuhina"        = "#E6AB02",
  "Waingaehe"      = "#FFD92F",
  "Waiohewa"       = "#FFFF99",
  "Waiowhiro"      = "#33A02C",
  "Waiteti"        = "#E5C494",
  "Ungauged-Quick" = "#666666",
  "Ungauged-Slow"  = "#B3B3B3",
  "Geothermal"     = "#D95F02",
  "WWTP"           = "#E7298A"
)

# -----------------------------------------------------------------------------
# get_stream_colours()
#    Looks up each stream's fixed colour from STREAM_PALETTE by name. Any
#    stream not found there (e.g. a new inflow added later, or test data with
#    different names) falls back to viridis, so the function never errors -
#    it just won't be colourblind-optimised for that particular stream.
# -----------------------------------------------------------------------------
get_stream_colours <- function(stream_names) {
  stream_names <- unique(stream_names)
  known        <- stream_names[stream_names %in% names(STREAM_PALETTE)]
  unknown      <- setdiff(stream_names, known)
  
  cols <- STREAM_PALETTE[known]
  
  if (length(unknown) > 0) {
    message(length(unknown), " stream(s) not found in STREAM_PALETTE (",
            paste(unknown, collapse = ", "), ") - using viridis for ",
            "those instead.")
    cols <- c(cols, setNames(viridisLite::viridis(length(unknown)), unknown))
  }
  
  cols
}

# =============================================================================
# Inflow Load Calculation and Plotting Functions
# =============================================================================
# Load = Flow (m3/day) * Concentration (g/m3 = mg/L) -> result in kg/day
# then aggregated to monthly or annual totals (tonnes/period)
# =============================================================================

# -----------------------------------------------------------------------------
# 1. calculate_loads()
#    Computes daily loads for all CHM/NIT/PHS/CAR/NCS/SIL concentration columns.
#    Flow is expected in m3/day; concentrations in g/m3 (mg/L).
#    Load (kg/day) = Flow (m3/day) * Concentration (g/m3) / 1000
# -----------------------------------------------------------------------------
calculate_loads <- function(df) {
  # Identify concentration columns (everything except Date, HYD_*, inflow_id)
  conc_cols <- names(df)[!names(df) %in% c("Date", "HYD_flow", "HYD_temp", "inflow_id")]
  
  df |>
    dplyr::mutate(
      Year  = lubridate::year(Date),
      Month = lubridate::month(Date),
      dplyr::across(
        dplyr::all_of(conc_cols),
        ~ HYD_flow * .x / 1000,          # kg/day
        .names = "load_{.col}"
      )
    )
}


# -----------------------------------------------------------------------------
# 2. aggregate_loads()
#    Summarises daily loads to "monthly", "annual", or "total" totals.
#    Returns a long-format tibble ready for plotting.
# -----------------------------------------------------------------------------
aggregate_loads <- function(load_df, period = c("monthly", "annual", "total")) {
  period    <- match.arg(period)
  load_cols <- names(load_df)[startsWith(names(load_df), "load_")]
  
  base <- load_df |>
    dplyr::select(Date, Year, Month, inflow_id, HYD_flow, dplyr::all_of(load_cols))
  
  agg <- switch(period,
                monthly = base |>
                  dplyr::group_by(inflow_id, Year, Month) |>
                  dplyr::summarise(
                    HYD_flow = sum(HYD_flow, na.rm = TRUE),                                  # -> m3/month
                    dplyr::across(dplyr::all_of(load_cols), ~ sum(.x, na.rm = TRUE) / 1000), # -> tonnes/month
                    .groups = "drop"
                  ) |>
                  dplyr::mutate(Period = as.Date(paste(Year, Month, "01", sep = "-"))),
                
                annual = base |>
                  dplyr::group_by(inflow_id, Year) |>
                  dplyr::summarise(
                    HYD_flow = sum(HYD_flow, na.rm = TRUE),                                  # -> m3/year
                    dplyr::across(dplyr::all_of(load_cols), ~ sum(.x, na.rm = TRUE) / 1000), # -> tonnes/year
                    .groups = "drop"
                  ) |>
                  dplyr::mutate(Period = as.Date(paste(Year, "01-01", sep = "-"))),
                
                total = base |>
                  dplyr::group_by(inflow_id) |>
                  dplyr::summarise(
                    HYD_flow = sum(HYD_flow, na.rm = TRUE),                                  # -> total m3
                    dplyr::across(dplyr::all_of(load_cols), ~ sum(.x, na.rm = TRUE) / 1000), # -> total tonnes
                    .groups = "drop"
                  ) |>
                  dplyr::mutate(Period = NA_character_, Year = NA_integer_, Month = NA_integer_)
  )
  
  # Pivot load columns and HYD_flow together into long format
  agg |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(c("HYD_flow", load_cols)),
      names_to  = "variable",
      values_to = "load"
    ) |>
    dplyr::mutate(
      variable = sub("^load_", "", variable),   # strip "load_" prefix
      units    = dplyr::case_when(
        variable == "HYD_flow" ~ switch(period,
                                        monthly = "m3 / month",
                                        annual  = "m3 / year",
                                        total   = "m3 total"
        ),
        .default = switch(period,
                          monthly = "tonnes / month",
                          annual  = "tonnes / year",
                          total   = "tonnes total"
        )
      ),
      period   = period
    )
}


# -----------------------------------------------------------------------------
# 3. plot_loads()
#    Creates a faceted ggplot of loads by inflow_id.
#    period:     "monthly" | "annual" | "total"
#    vars:       character vector of variable names to include, e.g. c("NIT_amm","NIT_nit")
#                NULL = all variables
#    group_vars: if TRUE, colour by variable; if FALSE, colour by inflow_id
# -----------------------------------------------------------------------------
plot_loads <- function(df,
                       period     = c("monthly", "annual", "total"),
                       vars       = NULL,
                       group_vars = TRUE) {
  
  period <- match.arg(period)
  
  long <- df |>
    calculate_loads() |>
    aggregate_loads(period = period)
  
  # Order inflow_id factor by descending total HYD_flow
  flow_order <- long |>
    dplyr::filter(variable == "HYD_flow") |>
    dplyr::group_by(inflow_id) |>
    dplyr::summarise(total_flow = sum(load, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_flow))
  
  long <- long |>
    dplyr::mutate(inflow_id = factor(inflow_id, levels = flow_order$inflow_id))
  
  
  if (!is.null(vars)) {
    long <- long |> dplyr::filter(variable %in% vars)
  }
  
  title <- switch(period,
                  monthly = "Monthly Loads by Inflow",
                  annual  = "Annual Loads by Inflow",
                  total   = "Total Loads by Inflow"
  )
  
  # Build facet label: "NIT_amm (tonnes / month)"
  long <- long |>
    dplyr::mutate(facet_label = paste0(variable, "\n(", units, ")"))
  
  # ---- Bar plot for total; line/point for monthly/annual ----
  if (period == "total") {
    long |>
      ggplot2::ggplot(ggplot2::aes(
        x    = inflow_id,
        y    = load,
        fill = if (group_vars) variable else inflow_id
      )) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ facet_label, scales = "free_y") +
      ggplot2::labs(
        title = title,
        x     = "Inflow",
        y     = NULL,
        fill  = if (group_vars) "Variable" else "Inflow"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "grey90"),
        axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position  = "bottom"
      )
    
  } else {
    long |>
      ggplot2::ggplot(ggplot2::aes(
        x      = Period,
        y      = load,
        colour = if (group_vars) variable else inflow_id,
        group  = if (group_vars) interaction(inflow_id, variable) else inflow_id
      )) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::facet_wrap(~ facet_label, scales = "free_y") +
      ggplot2::scale_x_date(
        date_labels = if (period == "monthly") "%b %Y" else "%Y",
        date_breaks = if (period == "monthly") "3 months" else "1 year"
      ) +
      ggplot2::labs(
        title  = title,
        x      = NULL,
        y      = NULL,
        colour = if (group_vars) "Variable" else "Inflow"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "grey90"),
        axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position  = "bottom"
      )
  }
}


# -----------------------------------------------------------------------------
# 4. plot_loads_by_group()
#    Convenience wrapper: one panel per variable-group (NIT, PHS, CAR, etc.)
#    rather than one panel per individual variable.
# -----------------------------------------------------------------------------
plot_loads_by_group <- function(df,
                                period = c("monthly", "annual", "total"),
                                groups = NULL) {
  
  period <- match.arg(period)
  
  long <- df |>
    calculate_loads() |>
    aggregate_loads(period = period) |>
    dplyr::mutate(group = sub("_.*", "", variable))  # e.g. "NIT_amm" -> "NIT"
  
  if (!is.null(groups)) {
    long <- long |> dplyr::filter(group %in% groups)
  }
  
  y_label <- switch(period,
                    monthly = "Load (tonnes / month)",
                    annual  = "Load (tonnes / year)",
                    total   = "Total Load (tonnes)"
  )
  
  if (period == "total") {
    long |>
      ggplot2::ggplot(ggplot2::aes(x = variable, y = load, fill = inflow_id)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::facet_wrap(~ group, scales = "free") +
      ggplot2::labs(
        title = paste("Total Loads \u2013", paste(unique(long$group), collapse = ", ")),
        x     = NULL,
        y     = y_label,
        fill  = "Inflow"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle = 35, hjust = 1),
        legend.position = "bottom"
      )
    
  } else {
    long |>
      ggplot2::ggplot(ggplot2::aes(
        x        = Period,
        y        = load,
        colour   = variable,
        group    = interaction(inflow_id, variable),
        linetype = inflow_id
      )) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::facet_wrap(~ group, scales = "free_y") +
      ggplot2::scale_x_date(
        date_labels = if (period == "monthly") "%b %Y" else "%Y"
      ) +
      ggplot2::labs(
        title    = paste(tools::toTitleCase(period), "Loads by Group"),
        x        = NULL,
        y        = y_label,
        colour   = "Variable",
        linetype = "Inflow"
      ) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
        legend.position = "bottom"
      )
  }
}

# -----------------------------------------------------------------------------
# 5. plot_annual_loads_stacked()
#    Stacked bar chart of annual loads by year, coloured by stream (inflow_id),
#    with one facet panel per variable - lets you see each stream's
#    contribution to the total load, and how that contribution shifts
#    from year to year.
#    vars: character vector of variable names to include, e.g. c("NIT_amm","NIT_nit")
#          NULL = all variables
# -----------------------------------------------------------------------------
plot_annual_loads_stacked <- function(aeme, vars = c("HYD_flow", "NIT_amm",
                                                     "NIT_nit", "NIT_don",
                                                     "NIT_pon", "PHS_frp",
                                                     "PHS_pop", "PHS_pip"),
                                      date_filter = TRUE) {
  
  df <- AEME::get_inflows(aeme, return_df = TRUE)
  
  var_label <- AEME::key_naming |> 
    dplyr::filter(var_aeme %in% vars) |> 
    dplyr::select(var_aeme, name_text, name_parse)
  
  if (date_filter) {
    df <- .filter_date(df, aeme)
  }
  long <- df |>
    calculate_loads() |>
    aggregate_loads(period = "annual")
  
  # Order inflow_id factor by descending total HYD_flow, so stacking order
  # (and legend order) is consistent across all panels
  flow_order <- long |>
    dplyr::filter(variable == "HYD_flow") |>
    dplyr::group_by(inflow_id) |>
    dplyr::summarise(total_flow = sum(load, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_flow))
  
  long <- long |>
    dplyr::mutate(inflow_id = factor(inflow_id, levels = flow_order$inflow_id))
  
  if (!is.null(vars)) {
    long <- long |> dplyr::filter(variable %in% vars)
  }
  

  # Build facet label: "NIT_amm (tonnes / year)"
  long <- long |>
    dplyr::left_join(var_label, by = c("variable" = "var_aeme")) |>
    dplyr::mutate(facet_label = paste0(name_text, "\n(", units, ")"))
  
  # alphabetical order of variable (= var_aeme) becomes the factor level order
  level_order <- long |>
    dplyr::distinct(variable, facet_label) |>
    dplyr::arrange(variable) |>
    dplyr::pull(facet_label)
  
  long <- long |>
    dplyr::mutate(facet_label = factor(facet_label, levels = level_order))
  
  stream_names <- unique(long$inflow_id)
  stream_cols <- get_stream_colours(stream_names)
  
  long |>
    ggplot2::ggplot(ggplot2::aes(
      x    = Year,
      y    = load,
      fill = inflow_id
    )) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::facet_wrap(~ facet_label, scales = "free_y") +
    # ggplot2::scale_fill_viridis_d() +
    ggplot2::scale_fill_manual(values = stream_cols) +
    ggplot2::labs(
      title = "Annual Loads by Stream",
      x     = "Year",
      y     = NULL,
      fill  = "Stream"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "grey90"),
      axis.text.x       = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position   = "bottom"
    )
}


# =============================================================================
# Example usage
# =============================================================================
if (FALSE) {
  df <- structure(list(
    Date      = structure(c(-3837,-3836,-3835,-3834,-3833,-3832), class = "Date"),
    HYD_flow  = c(46139.7, 99345.6, 122622.3, 95109.3, 81509.7, 129715.7),
    HYD_temp  = c(12.12, 12.27, 12.73, 12.71, 12.81, 12.45),
    CHM_salt  = c(0,0,0,0,0,0),
    CHM_oxy   = c(10.91, 10.87, 10.76, 10.77, 10.74, 10.83),
    CHM_ph    = c(6.6,6.6,6.6,6.6,6.6,6.6),
    NIT_amm   = c(0.009039,0.009040,0.009040,0.009040,0.009041,0.009041),
    NIT_nit   = c(0.5748,0.5748,0.5749,0.5749,0.5749,0.5749),
    NIT_don   = c(0.09695,0.09695,0.09696,0.09696,0.09697,0.09697),
    NIT_pon   = c(0.14543,0.14543,0.14544,0.14544,0.14545,0.14545),
    PHS_frp   = c(0.06464,0.06464,0.06464,0.06464,0.06464,0.06464),
    PHS_dop   = c(0,0,0,0,0,0),
    PHS_pop   = c(0.000304,0.000304,0.000304,0.000304,0.000304,0.000304),
    PHS_pip   = c(0.001218,0.001218,0.001218,0.001217,0.001217,0.001217),
    CAR_doc   = c(4.963,4.963,4.963,4.964,4.964,4.964),
    CAR_poc   = c(1.060,1.060,1.060,1.060,1.060,1.060),
    CAR_dic   = c(10,10,10,10,10,10),
    NCS_ss1   = c(0.3,0.3,0.3,0.3,0.3,0.3),
    NCS_ss2   = c(0.7,0.7,0.7,0.7,0.7,0.7),
    SIL_rsi   = c(1,1,1,1,1,1),
    inflow_id = rep("Awahou", 6)
  ), class = c("tbl_df", "tbl", "data.frame"))
  
  df <- AEME::get_inflows(aeme, return_df = TRUE)
  
  
  # Step 1 – calculate loads
  loads <- calculate_loads(df)
  
  # Step 2 – aggregate to monthly/annual/total
  monthly_loads <- aggregate_loads(loads, period = "monthly")
  annual_loads  <- aggregate_loads(loads, period = "annual")
  total_loads   <- aggregate_loads(loads, period = "total")
  
  # Step 3 – plot all variables
  plot_loads(df, period = "monthly")
  plot_loads(df, period = "annual")
  plot_loads(df, period = "total", 
             vars = c("HYD_flow", "NIT_amm", "NIT_nit", "NIT_don", "NIT_pon",
                      "PHS_frp", "PHS_pop", "PHS_pip"))
  
  # Plot only nitrogen variables
  plot_loads(df, period = "monthly", vars = c("NIT_amm", "NIT_nit", "NIT_don", "NIT_pon"))
  
  # Group-level plots
  plot_loads_by_group(df, period = "monthly", groups = c("NIT", "PHS", "CAR"))
  plot_loads_by_group(df, period = "total")
}
