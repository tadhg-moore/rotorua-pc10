setup_aeme <- function(aeme, model, path, gcm_point_data_std_df, gcm, scenario,
                       lake_level = NULL, sim_periods) {

  if (scenario != "historical") {
    sel_scenarios <- c("historical", scenario)
    sim_period <- sim_periods$ssp
  } else if (scenario == "historical") {
    sel_scenarios <- c("historical")
    sim_period <- sim_periods$historical
  }

  # gcm_point_data_std_df is already wide (pivoted inside
  # standardise_to_gregorian()) and calendar-complete (no missing calendar
  # days from 360_day/365_day GCMs) — just filter and rename to AEME's
  # Date/MET_* convention.
  met_df <- gcm_point_data_std_df |>
    dplyr::filter(gcm == !!gcm,
                  scenario %in% sel_scenarios) |>
    dplyr::rename(
      Date = date,
      MET_humrel = hurs,
      MET_tmpair = tas,
      MET_pprain = pr,
      MET_radswd = rsds,
      MET_wndspd = sfcWind
    ) |>
    dplyr::select(Date, MET_humrel, MET_tmpair, MET_pprain, MET_radswd, MET_wndspd)
  
  
  
  # aeme <- AEME::remove_met(aeme)
  aeme <- AEME::add_met(aeme, met_df)
  
  # Set start/stop times
  aeme <- AEME::set_time(aeme = aeme, start = sim_period$start,
                         stop = sim_period$stop, spin_up = sim_period$spin_up) 
  
  # Remove inflows
  inf <- AEME::inflows(aeme)
  inf$data <- NULL
  AEME::inflows(aeme) <- inf
  AEME::precip_status(aeme)
  aeme <- AEME::set_precip(aeme, type = "met")
  
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