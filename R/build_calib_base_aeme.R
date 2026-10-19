#' Build the baseline GLM-AED Aeme object used as the starting point of the
#' staged PEST++ calibration (calibration/), and write it to disk.
#'
#' Mirrors the construction in setup_base_GLM_sim.R so the calibration starts
#' from a reproducible object rather than a hand-saved aeme_base.rds.
#'
#' @param aeme_download_file path to the downloaded Aeme object (rds).
#' @param inflows named list of inflow data frames (one realisation; Rainfall
#'   is dropped).
#' @param alum_nml_file optional aed.nml holding the &aed_alum block.
#' @param out_file where to write the built Aeme object.
#' @param build_path directory the baseline model is built in.
#' @returns `out_file` (for a `format = "file"` target).
build_calib_base_aeme <- function(aeme_download_file, hyps_no_hole,
                                  era5_corrected_hourly, inflows,
                                  zone_heights, weir_params, aed_models_alum,
                                  model, ext_elev,
                                  alum_nml_file = NULL,
                                  out_file = "aeme_base.rds",
                                  build_path = "calibration/base_build",
                                  old_buoy_wq,
                                  daily_met = FALSE, sim_start = NULL, sim_stop = NULL,
                                  spin_up_days = 730) {
  aeme <- readRDS(aeme_download_file)
  inflows$Rainfall <- NULL

  # era5_corrected_hourly carries MET_pprain in metres accumulated per hour (read_era5_hourly_met()
  # divides the raw mm/hour by 1000; annual sums are 1.0-2.2). AEME's add_met() wants
  # millimetres per step (mm/hr for hourly data): it multiplies by 24 to mm/day and writes
  # /1000 to the GLM file in m/day. So convert m/hr -> mm/hr = * 1000.
  # CHECKED against the model output: with the earlier * 1000 / 24 the GLM `rain` volume was
  # ~13,700 m3/day (about 62 mm/yr over the lake), 24x too small; * 1000 gives ~1.5 m/yr.
  era5_corrected_hourly <- era5_corrected_hourly |>
    dplyr::mutate(MET_pprain = MET_pprain * 1000)
  # daily_met = TRUE: the series is a daily met data frame already in m/day (corrected
  # daily series), so * 1000 gives mm/day, which is what add_met() expects for daily data.
  yrs <- as.numeric(diff(range(as.Date(era5_corrected_hourly$Date)))) / 365.25 + 1 / 365.25
  rain_mm_yr <- sum(era5_corrected_hourly$MET_pprain, na.rm = TRUE) / yrs
  message("MET_pprain after conversion: ", round(rain_mm_yr), " mm/yr")
  if (!dplyr::between(rain_mm_yr, 800, 2500))
    warning("Annual rainfall of ", round(rain_mm_yr),
            " mm/yr is outside the plausible range for Rotorua.")

  max_depth <- AEME::get_hypsograph(aeme) |>
    dplyr::pull(depth) |>
    min()
  adj_depth <- max_depth - min(hyps_no_hole$depth)
  adj_zone_heights <- zone_heights[-1] + adj_depth
  aed_models <- aed_models_alum[aed_models_alum != "aed_oasim"]

  model_controls <- AEME::get_model_controls(aeme, use_bgc = TRUE) |>
    AEME::set_vars_sim(vars_sim = c("ALU_ala", "ALU_alap")) |>
    dplyr::mutate(
      inf_default = dplyr::case_when(
        var_aeme %in% c("ALU_ala", "ALU_alap") ~ 0,
        TRUE ~ inf_default
      ),
      initial_wc = dplyr::case_when(
        var_aeme %in% c("ALU_ala", "ALU_alap") ~ 0,
        TRUE ~ initial_wc
      )
    )

  sed_params <- AEME::glm_sed_params(n_zones = length(adj_zone_heights),
                                     zone_heights = adj_zone_heights) |>
    dplyr::mutate(file = "glm4.nml")

  aeme_base <- aeme |>
    AEME::remove_inflow(all = TRUE) |>
    AEME::remove_outflow(all = TRUE) |>
    AEME::remove_param() |>
    AEME::reset_wbal_param() |>
    AEME::add_hypsograph(hyps_no_hole) |>
    AEME::set_initial_conditions(depth = 21) |>
    AEME::add_met(era5_corrected_hourly) |>
    AEME::add_inflows(inflows) |>
    AEME::add_param(sed_params) |>
    AEME::add_param(weir_params) |>
    AEME::add_obs(lake = old_buoy_wq) |> 
    AEME::set_output_time_step(frequency = "daily")

  if (is.null(sim_start)) {
    aeme_base <- AEME::set_sim_period(aeme_base, AEME::suggest_sim_period(aeme_base))
  } else {
    aeme_base <- AEME::set_time(aeme_base, start = sim_start, stop = sim_stop,
                                spin_up = spin_up_days)
  }

  dir.create(build_path, recursive = TRUE, showWarnings = FALSE)
  aeme_base <- aeme_base |>
    AEME::build_aeme(model = model, path = build_path, use_bgc = FALSE,
                     ext_elev = ext_elev, model_controls = model_controls) |>
    AEME::set_glm_aed_models(aed_models = aed_models)

  if (!is.null(alum_nml_file) && file.exists(alum_nml_file)) {
    alum_cfg <- AEME::read_nml(alum_nml_file)[["aed_alum"]]
    aed_file <- AEME::get_model_config_files(aeme_base)$glm_aed["aed"]
    nml <- AEME::read_nml(aed_file)
    nml[["aed_alum"]] <- alum_cfg
    AEME::write_nml(nml, aed_file)
  }

  saveRDS(aeme_base, out_file)
  out_file
}
