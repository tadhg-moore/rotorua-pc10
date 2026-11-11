#' Read a 2026 PC10 loads set into the project's inflow-list format
#'
#' Reads one folder of `data/raw/pc10_loads_2026` (McBride, Limnotrack / BoPRC,
#' 10 Nov 2026) and returns a named list of data frames in the same layout and
#' units as `format_dyresm_inflow()` produces from
#' `rotorua_inflows_populated_noalum.csv`, so it can feed
#' `rotorua_inflow_list_matrix` without other changes. Nothing is written to
#' disk and the existing inflow targets are not touched.
#'
#' The new files are in AED units (mmol m-3, `FLOW` in m3 s-1). The project list
#' is in mg L-1 (AEME converts to mmol itself), so concentrations are converted
#' back with the molar masses used in the report (N 14.007, P 30.974, C 12.011,
#' SiO2 60.08, O2 31.998 g mol-1). Silica is expressed as mg Si L-1 (28.086 g/mol)
#' to match `rotorua_inflow_key.csv`.
#'
#' Column mapping (new -> project):
#' FLOW -> HYD_flow (m3 day-1); TEMP -> HYD_temp; SALT -> CHM_salt;
#' OXY_oxy -> CHM_oxy (mg O2 L-1); CAR_pH -> CHM_ph; NIT_amm/NIT_nit -> same (mg N L-1);
#' OGM_don/OGM_pon -> NIT_don/NIT_pon; PHS_frp -> PHS_frp;
#' OGM_dop/OGM_pop -> PHS_dop/PHS_pop; PHS_frp_ads -> PHS_pip;
#' OGM_doc/OGM_poc -> CAR_doc/CAR_poc; CAR_dic -> CAR_dic (mg C L-1);
#' NCS_ss1/NCS_ss2 -> same (g m-3 = mg L-1); SIL_rsi -> SIL_rsi (mg Si L-1).
#'
#' No alum columns are added here: as in the existing targets, `ALU_ala` and
#' `ALU_alp` are added afterwards from the dosing records. For the project's
#' convention (alum module on, in-stream binding handled in the model) use a
#' `noalum_*` set.
#'
#' @param set Folder name, `<alum|noalum>_<storm|nostorm>_<central|lower|upper>`.
#' @param root Directory holding the set folders.
#' @param include_rain If TRUE add `Rainfall` (rain on the lake) as an inflow.
#'   Leave FALSE when rain is supplied through the met file (it would be counted twice).
#' @param from,to Optional date limits (inclusive).
#' @return Named list of inflow data frames, ordered and named as the existing
#'   project inflows (Awahou, Hamurana, ..., Ungauged-Quick, [Rainfall], WWTP).
read_pc10_loads <- function(set = "noalum_storm_central",
                            root = here::here("data", "raw", "pc10_loads_2026"),
                            include_rain = FALSE, from = NULL, to = NULL) {
  dir <- file.path(root, set)
  if (!dir.exists(dir)) stop("Loads set not found: ", dir)

  key <- pc10_inflow_key()
  if (!include_rain) key <- key[key$name != "Rainfall", ]

  out <- lapply(seq_len(nrow(key)), function(i) {
    df <- readr::read_csv(file.path(dir, key$file[i]), show_col_types = FALSE)
    pc10_to_project(df, from = from, to = to)
  })
  names(out) <- key$name
  out
}

#' Add ALU_ala / ALU_alp columns to an inflow list (PC10-loads branch)
#'
#' Same logic as the `rotorua_inflow_list_matrix` target (alum dose on the dosed
#' streams, zero elsewhere), kept as a function so the old target stays as it is.
#' Days with no dosing record get zero alum.
#'
#' @param inflows list from [read_pc10_loads()].
#' @param alum_dosing data frame from `read_alum_dosing()` (Date, one column per stream).
#' @param dosed names of the dosed inflows.
#' @param ratio_al_p Al:P ratio applied to the dose, as in the existing target.
add_alum_to_inflows <- function(inflows, alum_dosing,
                                dosed = c("Puarenga", "Utuhina"),
                                ratio_al_p = 1) {
  out <- lapply(names(inflows), function(nm) {
    df <- inflows[[nm]]
    if (nm %in% dosed) {
      sel <- alum_dosing |>
        dplyr::select(dplyr::contains(c("Date", nm))) |>
        dplyr::rename(alum = dplyr::contains(nm))
      df <- df |>
        dplyr::left_join(sel, by = "Date") |>
        dplyr::mutate(
          alum    = dplyr::if_else(is.na(alum) | alum < 0, 0, alum),
          ALU_ala = calc_alum_inflow(alum_vol_L_day = alum,
                                     flow_m3s = HYD_flow / 86400) * ratio_al_p,
          ALU_alp = 0
        ) |>
        dplyr::select(-alum)
    } else {
      df <- dplyr::mutate(df, ALU_ala = 0, ALU_alp = 0)
    }
    dplyr::mutate(df, Date = as.Date(Date))
  })
  names(out) <- names(inflows)
  out
}

#' Inflow file / project-name key (project InfNum order, from Rotorua_infID.csv)
pc10_inflow_key <- function() {
  data.frame(
    num = c(1:10, 11, 12, 13, 14),
    name = c("Awahou", "Hamurana", "Ngongotaha", "Puarenga", "Utuhina",
             "Waingaehe", "Waiohewa", "Waiowhiro", "Waiteti", "Geothermal",
             "Ungauged-Slow", "Ungauged-Quick", "Rainfall", "WWTP"),
    file = c("inflow_05_Awahou.csv", "inflow_04_Hamurana.csv",
             "inflow_03_Ngongotaha.csv", "inflow_02_Puarenga.csv",
             "inflow_01_Utuhina.csv", "inflow_08_Waingaehe.csv",
             "inflow_09_Waiohewa.csv", "inflow_07_Waiowhiro.csv",
             "inflow_06_Waiteti.csv", "inflow_10_Geothermal.csv",
             "inflow_11_Ungauged_Slow.csv", "inflow_12_Ungauged_Quick.csv",
             "rain_on_lake_OPTIONAL.csv", "inflow_14_WWTP.csv"),
    stringsAsFactors = FALSE
  )
}

#' Convert one new-format inflow table to the project's units and names
pc10_to_project <- function(df, from = NULL, to = NULL) {
  mw <- c(N = 14.007, P = 30.974, C = 12.011, Si = 28.086, O2 = 31.998)
  mg <- function(x, m) x * m / 1000   # mmol m-3 -> mg L-1
  zero <- function(x) ifelse(is.na(x), 0, x)

  res <- data.frame(
    Date     = as.Date(df$time),
    HYD_flow = pmax(df$FLOW, 0) * 86400,
    HYD_temp = df$TEMP,
    CHM_salt = df$SALT,
    CHM_oxy  = mg(df$OXY_oxy, mw[["O2"]]),
    CHM_ph   = df$CAR_pH,
    NIT_amm  = mg(df$NIT_amm, mw[["N"]]),
    NIT_nit  = mg(df$NIT_nit, mw[["N"]]),
    NIT_don  = mg(df$OGM_don, mw[["N"]]),
    NIT_pon  = mg(df$OGM_pon, mw[["N"]]),
    PHS_frp  = mg(df$PHS_frp, mw[["P"]]),
    PHS_dop  = mg(df$OGM_dop, mw[["P"]]),
    PHS_pop  = mg(df$OGM_pop, mw[["P"]]),
    PHS_pip  = mg(df$PHS_frp_ads, mw[["P"]]),
    CAR_doc  = mg(df$OGM_doc, mw[["C"]]),
    CAR_poc  = mg(df$OGM_poc, mw[["C"]]),
    CAR_dic  = mg(df$CAR_dic, mw[["C"]]),
    NCS_ss1  = df$NCS_ss1,
    NCS_ss2  = df$NCS_ss2,
    SIL_rsi  = mg(df$SIL_rsi, mw[["Si"]])
  )
  # The rain file has no water temperature; the project list has no NAs
  res[] <- lapply(res, function(x) if (is.numeric(x)) zero(x) else x)
  if (!is.null(from)) res <- res[res$Date >= as.Date(from), ]
  if (!is.null(to))   res <- res[res$Date <= as.Date(to), ]
  tibble::as_tibble(res)
}
