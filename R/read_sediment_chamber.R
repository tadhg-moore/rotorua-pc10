# Benthic chamber deployment, Lake Rotorua, 17-20 March 2026.
# Source: data/raw/sediment_chamber/Rotorua Nutrient Flux.xlsx
#
# Three sites (1 = 10 m, 2 = 15 m, 3 = 20 m), light and dark chambers (colour
# = chamber ID). Rates are linear-regression slopes of chamber concentration.
#   nutrient_flux: NH4, NOx, PO4 flux, mg/m2/day, positive = release to water.
#   sod:           sediment oxygen demand, mg O2/m2/day, negative = uptake;
#                  slope fitted from deployment start to first DO <= 1 mg/L.
#                  (The sheet's "SODMeans" header says mg/m2/day but those
#                  means are g/m2/day; here everything is kept in mg.)
#   lake_conditions: mid-day buoy casts at 2 m and 20 m on each deployment day.
#   summary: mean/SD/n by nutrient, site and light, and pooled over light,
#            recomputed from the per-chamber values (matches the workbook's
#            "Flux Means", "Flux Pooled Means" and "SODMeans" sheets).

read_sediment_chamber <- function(file) {
  site_depth <- c(`1` = 10, `2` = 15, `3` = 20)

  flux <- readxl::read_excel(
    file, sheet = "Raw Nutrient Flux", skip = 2, col_names = c(
      "site", "light", "chamber", "variable", "flux_mg_m2_d", "se",
      "r2", "adj_r2", "p", "n", "intercept"
    ), col_types = c("numeric", "text", "text", "text", rep("numeric", 7))
  ) |>
    dplyr::filter(!is.na(site)) |>
    dplyr::mutate(site = as.integer(site),
                  depth_m = unname(site_depth[as.character(site)]),
                  .after = site)

  sod <- readxl::read_excel(
    file, sheet = "SOD", skip = 2, col_names = c(
      "chamber", "depth_lab", "light", "slope_mg_l_d", "sod_mg_m2_d",
      "r2", "n", "se"
    ), col_types = c("text", "text", "text", rep("numeric", 5))
  ) |>
    dplyr::filter(!is.na(chamber), !is.na(sod_mg_m2_d)) |>
    dplyr::mutate(depth_m = as.numeric(sub("\\s*m$", "", depth_lab)),
                  site = as.integer(match(depth_m, site_depth)),
                  variable = "SOD") |>
    dplyr::select(site, depth_m, light, chamber, variable,
                  slope_mg_l_d, sod_mg_m2_d, se, r2, n)

  lake <- readxl::read_excel(
    file, sheet = "Lake Conditions",
    range = readxl::cell_limits(c(3, 1), c(NA, 4)),
    col_names = c("date", "depth_m", "do_mg_l", "temp_c"),
    col_types = c("date", rep("numeric", 3))
  ) |>
    dplyr::filter(!is.na(date)) |>
    dplyr::mutate(date = as.Date(date))

  rates <- dplyr::bind_rows(
    flux |> dplyr::select(site, depth_m, light, chamber, variable,
                          rate_mg_m2_d = flux_mg_m2_d),
    sod |> dplyr::select(site, depth_m, light, chamber, variable,
                         rate_mg_m2_d = sod_mg_m2_d)
  )
  summ <- function(d) dplyr::summarise(
    d, mean_mg_m2_d = mean(rate_mg_m2_d), sd_mg_m2_d = stats::sd(rate_mg_m2_d),
    n = dplyr::n(), .groups = "drop"
  )
  summary <- dplyr::bind_rows(
    rates |> dplyr::group_by(variable, site, depth_m, light) |> summ(),
    rates |> dplyr::group_by(variable, site, depth_m) |> summ() |>
      dplyr::mutate(light = "Pooled")
  ) |>
    dplyr::arrange(variable, depth_m, light)

  list(
    nutrient_flux   = flux,
    sod             = sod,
    lake_conditions = lake,
    summary         = summary,
    dates           = range(lake$date)
  )
}
