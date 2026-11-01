lspc_to_aeme <- function(inf, flow_var = "ro") {

  c_n_ratio <- 5.7 # Typical mass C:N (Redfield-based mass ratio) ≈ 5.7 (range ~5–8)

  inflow <- inf |>
    dplyr::mutate(Date = lubridate::as_date(datetime)) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      # HYD_flow = sum(!!rlang::sym(flow_var)), # Convert from m3/day to m3/s
      HYD_flow = sum(ro), # Convert from m3/day to m3/s
      HYD_temp = mean(temp),

      # Chemistry
      CHM_salt = 0,
      CHM_oxy = ifelse(HYD_flow > 0,
                       sum(dox * ro, na.rm = TRUE) / HYD_flow,
                       0),
      # Nitrogen concentrations in mg/L
      NIT_tn = ifelse(HYD_flow > 0,
                      sum(concout1 * ro, na.rm = TRUE) / HYD_flow,
                      0), #tn
      NIT_nit = ifelse(HYD_flow > 0,
                       sum(no3 * ro, na.rm = TRUE) / HYD_flow,
                       0),
      NIT_amm = ifelse(HYD_flow > 0,
                       sum(tam * ro, na.rm = TRUE) / HYD_flow,
                       0),
      NIT_din = ifelse(HYD_flow > 0,
                       sum(din * ro, na.rm = TRUE) / HYD_flow,
                       0),

      # Phosphorus concentrations in mg/L
      PHS_tp = ifelse(HYD_flow > 0,
                      sum(concout2 * ro, na.rm = TRUE) / HYD_flow,
                      0),
      PHS_frp = ifelse(HYD_flow > 0,
                       sum(po4 * ro, na.rm = TRUE) / HYD_flow,
                       0),
      # PHS_pop = mean(po4),


      # Total suspended solids in mg/L
      NCS_tss = ifelse(HYD_flow > 0,
                       sum(ssedout * ro, na.rm = TRUE) / HYD_flow,
                       0),
      NCS_ss1 = ifelse(HYD_flow > 0,
                       sum(ssedout * ro, na.rm = TRUE) / HYD_flow,
                       0),
      # Derived variables
      NIT_don = ifelse(HYD_flow > 0,
                       sum(torn * 0.3 * ro, na.rm = TRUE) / HYD_flow,
                       0), # Assume 30% of DON is bioavailable

      PART_N_mgL = ifelse(HYD_flow > 0,
                          sum(concsnh4 * ro, na.rm = TRUE) / HYD_flow,
                          0),   # particulate NH4-N (mg/L)
      PART_P_mgL = ifelse(HYD_flow > 0,
                          sum(concspo4 * ro, na.rm = TRUE) / HYD_flow,
                          0),   # particulate PO4-P (mg/L)
      NIT_pon = ifelse(HYD_flow > 0,
                       # prefer to use measured particulate N if available; else fallback to % of TSS
                       ifelse(!all(is.na(concsnh4)),
                              PART_N_mgL,
                              NCS_tss_mgL * 0.01),
                       0), # Assume 1% of TSS is particulate organic nitrogen

      PHS_dop = ifelse(HYD_flow > 0,
                       sum(torp * 0.2 * ro, na.rm = TRUE) / HYD_flow,
                       0), # Assume 20% of DOP is bioavailable
      PHS_pop = ifelse(HYD_flow > 0,
                       ifelse(!all(is.na(concspo4)),
                              PART_P_mgL * 0.4,
                              concspo4 * 0.4),
                       0),
      PHS_pip = mean(concspo4) * 0.6,
      # Carbon derivations in mg/L
      CAR_doc = NIT_don * c_n_ratio, # Typical mass C:N (Redfield-based mass ratio) ≈ 5.7 (range ~5–8)
      CAR_poc = NIT_pon * c_n_ratio,


      watershed_area_ha = watershed_area_ha[1],
      drainage_area_ha = drainage_area_ha[1]
    ) |>
    dplyr::select(-PART_N_mgL, -PART_P_mgL)
  inflow
  return(inflow)
}

# vars <- c("CHM_salt", "CHM_oxy", "PHS_frp", "PHS_dop", "PHS_pip", "NIT_don", "NIT_pon", "CAR_doc", "CAR_poc", "SIL_rsi", "PHY_cyano",
#          "PHY_diatom", "NCS_ss1", "PHS_pop")
# AEME::lookup_aeme_vars(var_aeme = vars)
# AEME::lookup_aeme_vars(name = "nit")
