# Helpers to compare the 2026 PC10 loads with the project's existing inflow file.
# Pure functions: they read the raw CSVs and write nothing.

#' Hydrological year (July-June), named by the year in which it ends
pc10_hy <- function(date) {
  as.integer(format(date, "%Y")) + (as.integer(format(date, "%m")) >= 7)
}

#' Annual loads (t/y) per inflow from a project-format inflow list
#'
#' @param inflows named list of data frames with Date, HYD_flow (m3/day) and
#'   concentrations in mg/L, as from `read_pc10_loads()` or `format_dyresm_inflow()`.
#' @return data frame: inflow, hy, flow (Mm3), tn, tp, frp, pp (particulate P),
#'   dop, tss, din (all t/y).
inflow_annual_loads <- function(inflows) {
  col <- function(d, nm) if (nm %in% names(d)) d[[nm]] else 0
  dplyr::bind_rows(lapply(names(inflows), function(nm) {
    d <- inflows[[nm]]
    v <- d$HYD_flow                       # m3/day; mg/L x m3 = g
    t <- 1e-6                             # g -> t
    data.frame(
      inflow = nm,
      hy     = pc10_hy(d$Date),
      flow   = v / 1e6,
      tn     = (col(d, "NIT_amm") + col(d, "NIT_nit") + col(d, "NIT_don") + col(d, "NIT_pon")) * v * t,
      tp     = (col(d, "PHS_frp") + col(d, "PHS_dop") + col(d, "PHS_pop") + col(d, "PHS_pip")) * v * t,
      frp    = col(d, "PHS_frp") * v * t,
      pp     = (col(d, "PHS_pop") + col(d, "PHS_pip")) * v * t,
      dop    = col(d, "PHS_dop") * v * t,
      din    = (col(d, "NIT_amm") + col(d, "NIT_nit")) * v * t,
      tss    = (col(d, "NCS_ss1") + col(d, "NCS_ss2")) * v * t
    )
  })) |>
    dplyr::group_by(inflow, hy) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum), .groups = "drop")
}

#' Read the existing project inflow file in the same list format
#'
#' Uses the project's own reader so the comparison is against exactly what the
#' current targets use. `format_dyresm_inflow()` writes CSVs to
#' data/processed, so the file is parsed here without that side effect.
read_current_inflows <- function(file = here::here("data", "raw", "flows",
                                                    "rotorua_inflows_populated_noalum.csv")) {
  inf <- readr::read_csv(file, show_col_types = FALSE) |>
    dplyr::mutate(
      Date = lubridate::as_date(Date, format = "%d/%m/%Y"),
      dplyr::across(where(is.numeric), \(x) ifelse(is.na(x), 0, x))
    )
  nm <- c(Awahou = 1, Hamurana = 2, Ngongotaha = 3, Puarenga = 4, Utuhina = 5,
          Waingaehe = 6, Waiohewa = 7, Waiowhiro = 8, Waiteti = 9, Geothermal = 10,
          `Ungauged-Slow` = 11, `Ungauged-Quick` = 12, Rainfall = 13, WWTP = 14)
  lapply(nm, function(i) {
    d <- inf[inf$InfNum == i, ]
    data.frame(
      Date = d$Date, HYD_flow = pmax(d$VOL, 0),
      CHM_oxy = d$DO, NIT_amm = d$NH4, NIT_nit = d$NO3,
      NIT_don = d$DONL, NIT_pon = d$PONL, PHS_frp = d$DRP,
      PHS_dop = d$DOPL, PHS_pop = d$POPL, PHS_pip = d$PIP,
      CAR_doc = d$DOCL, CAR_poc = d$POCL, NCS_ss1 = d$SSOL1, NCS_ss2 = d$SSOL2
    )
  })
}
