#' Calculate Alum Inflow Concentration for AED Model
#'
#' @param alum_vol_L_day Liquid alum dosage rate in Liters per day (L/day).
#' @param flow_m3s Stream inflow rate in cubic meters per second (m^3/s).
#' @param percent_Al Percentage of active Aluminum by weight (default: 4.263%).
#' @param density_kg_L Density/specific gravity of liquid alum in kg/L or g/cm^3 (default: 1.323).
#' @param ratio_Al_P Molar binding efficiency ratio of Al:P (default: 1.0, 1 mmol Al binds 1 mmol P).
#'
#' @return Concentration of ALU_ala in mmol P / m^3.
#'
calc_alum_inflow <- function(alum_vol_L_day, 
                             flow_m3s, 
                             percent_Al = 4.263, 
                             density_kg_L = 1.323, 
                             ratio_Al_P = 1.0) {
  
  # if (flow_m3s == 0) {
  #   return(0)
  # } else if (alum_vol_L_day == 0) {
  #   return(0)
  # }
  # Atomic mass of Aluminum (g/mol or mg/mmol)
  mw_Al <- 26.981539
  
  # 1. Mass of liquid alum dosing solution (kg/day)
  mass_alum_kg_day <- alum_vol_L_day * density_kg_L
  
  # 2. Mass of pure Aluminum (Al) added (kg Al/day)
  mass_Al_kg_day <- mass_alum_kg_day * (percent_Al / 100)
  
  # 3. Convert kg Al/day to mmol Al/day
  mmol_Al_day <- (mass_Al_kg_day * 1e6) / mw_Al
  
  # 4. Total phosphorus binding capacity added (mmol P capacity/day)
  mmol_P_capacity_day <- mmol_Al_day * ratio_Al_P
  
  # 5. Convert stream flow rate from m^3/s to m^3/day
  flow_m3_day <- flow_m3s * 86400
  
  # 6. Calculate stream concentration (mmol P / m^3)
  conc_ALU_ala <- mmol_P_capacity_day / flow_m3_day
  
  return(conc_ALU_ala)
}
