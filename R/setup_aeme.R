setup_aeme <- function(aeme, model, path, gcm_point_data_df, gcm, scenario,
                       lake_level = NULL, sim_periods) {
  
  if (scenario != "historical") {
    sel_scenarios <- c("historical", scenario) 
    sim_period <- sim_periods$ssp
  } else if (scenario == "historical") {
    sel_scenarios <- c("historical")
    sim_period <- sim_periods$historical
  }
  
  met_df <- gcm_point_data_df |>
    dplyr::filter(gcm == !!gcm,
                  scenario %in% sel_scenarios) |>
    dplyr::select(date, variable, value) |>
    tidyr::pivot_wider(names_from = variable, values_from = value)
  
  # aeme <- AEME::remove_met(aeme)
  inp <- AEME::input(aeme)
  inp$meteo <- met_df
  AEME::input(aeme) <- inp
  
  # Set start/stop times
  aeme <- AEME::set_time(aeme = aeme, start = sim_period$start,
                         stop = sim_period$stop, spin_up = sim_period$spin_up) 
  
  # Remove inflows
  inf <- AEME::inflows(aeme)
  inf$data <- NULL
  AEME::inflows(aeme) <- inf
  aeme <- AEME::set_precip(aeme, type = "precip_as_inflow")
  
  if (!is.null(lake_level)) {
    lke <- AEME::lake(aeme)
    inp <- AEME::input(aeme)
    lake_elev <- lke$elevation
    obs_lvl <- lake_level |> 
      dplyr::select(Date, value)
    
    aeme <- AEME::add_obs(aeme = aeme, level = obs_lvl)
    
  }
  
  aeme <- AEME::build_aeme(aeme = aeme, model = model, ext_elev = 3, 
                           use_bgc = TRUE, wb_method = 3, path = path)
  
  
}