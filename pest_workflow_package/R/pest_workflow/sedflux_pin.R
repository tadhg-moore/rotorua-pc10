# Pin measured sediment flux parameters (chamber experiments) in the stage-4
# adjustable parameter table, instead of freezing them.
#
# Why not freeze: the earlier freeze_sediment_flux_data() was abandoned because
# calib_aeme_pest()'s localizer needs EVERY parameter (adjustable or frozen) to
# have a row in the stage's own Morris screen, and fsed_*/Ksed_* first appear in
# stage 4, so a frozen one has no screen row and the stage crashes.
#
# Each measured parameter is removed from the adjustable table and returned in
# attr(ptab, "pinned") as FIXED rows (min = max = measurement), which v6 passes to
# run_stage() as frozen parameters. (First version: bounds of +/-0.1% "adjustable in
# name only" - WRONG: a Morris elementary effect divides the response change by the
# parameter step, so a ~1e-4-wide range turned noise into mu* ~ 4e5 and swamped the
# whole screen. Fixed parameters are not varied by the screen and ignored by the
# localizer.)
#
# Active only for lakes listed in env PEST_PIN_SEDFLUX_LAKES (comma separated
# LIDs, e.g. "LID22999,LID23000").
#
# Source of the measurements: the `flux_params` TARGET (R/est_sed_fluxes.R, the
# same table the dashboard's sediment_flux_obs shows) - NOT
# data/processed/sediment_flux_params.csv, which is a stale Oct-2025 export with
# the KSOD/Sg cells swapped and unconverted (e.g. Fsed_frp = 7.08 instead of
# 0.051 mmol/m2/d) and is what the abandoned freeze_sediment_flux_data() read.
#
# Pinned by default: the sediment RATES (Fsed_oxy, Fsed_frp, Fsed_amm) and the
# oxygen terms (Ksed_oxy, theta_sed_oxy). NOT pinned: Ksed_frp / Ksed_amm - the
# workbook conversion multiplies the cell by 31.998 (the molar mass of O2) to get
# these, which is only valid for oxygen, and gives 220-270 mmol/m3, far outside
# the model's 10-100 range; PEST keeps calibrating them. Override with env
# PEST_PIN_SEDFLUX_PARS (comma separated, e.g. "Fsed_oxy,Fsed_frp").

PIN_REL <- 0.001
PIN_PARS_DEFAULT <- c("Fsed_oxy", "Ksed_oxy", "theta_sed_oxy", "Fsed_frp", "Fsed_amm")
SEDFLUX_PAR_MAP <- c(
  Fsed_oxy = "aed_sed_const2d/fsed_oxy", Ksed_oxy = "aed_oxygen/ksed_oxy",
  theta_sed_oxy = "aed_oxygen/theta_sed_oxy",
  Fsed_frp = "aed_sed_const2d/fsed_frp", Ksed_frp = "aed_phosphorus/ksed_frp",
  Fsed_amm = "aed_sed_const2d/fsed_amm", Ksed_amm = "aed_nitrogen/ksed_amm",
  Fsed_nit = "aed_sed_const2d/fsed_nit", Ksed_nit = "aed_nitrogen/ksed_nit"
)

sedflux_pin_lakes <- function() {
  x <- Sys.getenv("PEST_PIN_SEDFLUX_LAKES")
  if (!nzchar(x)) character() else trimws(strsplit(x, ",")[[1]])
}

#' @param ptab stage-4 adjustable table (columns name, value, min, max, ...)
#' @param lid  e.g. "LID22999"
pin_sediment_flux <- function(ptab, lid, flux_params = targets::tar_read(flux_params)) {
  if (!lid %in% sedflux_pin_lakes()) return(ptab)
  pars <- Sys.getenv("PEST_PIN_SEDFLUX_PARS")
  pars <- if (nzchar(pars)) trimws(strsplit(pars, ",")[[1]]) else PIN_PARS_DEFAULT
  sfp <- as.data.frame(flux_params)
  sfp <- sfp[paste0("LID", sfp$LID) == lid & !is.na(sfp$value) & sfp$par %in% pars, ]
  sfp$aed_name <- SEDFLUX_PAR_MAP[sfp$par]
  sfp <- sfp[!is.na(sfp$aed_name), ]
  if (!nrow(sfp)) { message("  pin_sediment_flux: no measurements for ", lid); return(ptab) }
  pinned <- list()
  for (i in seq_len(nrow(sfp))) {
    hit <- which(ptab$name == sfp$aed_name[i])
    if (!length(hit)) { message("  pin_sediment_flux: ", sfp$aed_name[i], " not in the stage-4 table - skipped"); next }
    v <- sfp$value[i]
    fx <- ptab[hit, , drop = FALSE]
    fx$value <- v; fx$min <- v; fx$max <- v          # FIXED: min = max
    pinned[[length(pinned) + 1]] <- fx
    ptab <- ptab[-hit, , drop = FALSE]                # no longer adjustable -> not in the Morris screen
    message(sprintf("  pin_sediment_flux: %s = %.5g (measured; held FIXED) for %s", sfp$aed_name[i], v, lid))
  }
  attr(ptab, "pinned") <- if (length(pinned)) do.call(rbind, pinned) else NULL
  ptab
}
