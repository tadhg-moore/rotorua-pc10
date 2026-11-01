# Parameter set for the wide Morris sensitivity analysis (SA). Ranges are physically
# plausible bounds (AED source: libaed-water/src/*.F90; literature ranges for phytoplankton
# traits), deliberately wider than the calibration bounds so the SA shows what the model
# can do, not only what the calibration was allowed to do.
#
# Each row: module/name, file, group (phyto group), index (sediment zone / phyto column),
# min, max, scale ("log" = sampled uniformly in log space, needs same-sign bounds).
# The start value is taken from the calibrated set (baseline) when the parameter is in it,
# otherwise from the built AED/GLM configuration.

sa_spec <- function() {
  S <- list()
  add <- function(name, min, max, scale = "log", file = "aed.nml", model = "glm_aed",
                  group = NA_character_, index = NA_real_, theme = NA_character_)
    S[[length(S) + 1]] <<- data.frame(model = model, file = file, name = name, group = group,
                                      index = index, min = min, max = max, scale = scale,
                                      theme = theme, stringsAsFactors = FALSE)
  glm3 <- function(...) add(..., file = "glm3.nml", theme = "physics")
  glm4 <- function(...) add(..., file = "glm4.nml", theme = "physics")
  met  <- function(name, min, max) add(name, min, max, "lin", file = "met", theme = "physics")

  # ---- physics: forcing scalings, heat-flux coefficients, mixing ------------------------------
  met("MET_wndspd", 0.8, 1.35); met("MET_radswd", 0.85, 1.15); met("MET_radlwd", 0.9, 1.2)
  met("MET_tmpair", 0.9, 1.1)
  glm4("meteorology/rh_factor", 0.9, 1.1, "lin")
  glm4("meteorology/ce", 0.0008, 0.002, "lin"); glm4("meteorology/ch", 0.0008, 0.002, "lin")
  glm4("meteorology/cd", 0.0008, 0.002, "lin")
  glm3("light/Kw", 0.05, 1.0)
  glm4("mixing/coef_mix_conv", 0.05, 0.4, "lin"); glm4("mixing/coef_wind_stir", 0.1, 0.6, "lin")
  glm4("mixing/coef_mix_shear", 0.05, 0.8, "lin"); glm4("mixing/coef_mix_turb", 0.1, 1.5, "lin")
  glm4("mixing/coef_mix_KH", 0.1, 0.8, "lin"); glm4("mixing/coef_mix_hyp", 0.1, 2.0)

  # ---- sediment fluxes by zone (zone 1 = deepest). Ranges are multiples of the base value. ----
  for (z in 1:4) {
    add("aed_sed_const2d/fsed_oxy", NA, NA, "log", index = z, theme = "sediment")
    add("aed_sed_const2d/fsed_amm", NA, NA, "log", index = z, theme = "sediment")
    add("aed_sed_const2d/fsed_frp", NA, NA, "log", index = z, theme = "sediment")
  }
  add("aed_oxygen/ksed_oxy", 5, 100, theme = "sediment")
  add("aed_oxygen/theta_sed_oxy", 1.02, 1.12, "lin", theme = "sediment")
  add("aed_nitrogen/ksed_amm", 5, 100, theme = "sediment")
  add("aed_nitrogen/theta_sed_amm", 1.02, 1.12, "lin", theme = "sediment")
  add("aed_nitrogen/ksed_nit", 10, 200, theme = "sediment")
  add("aed_phosphorus/ksed_frp", 5, 100, theme = "sediment")
  add("aed_phosphorus/theta_sed_frp", 1.02, 1.12, "lin", theme = "sediment")
  add("aed_organic_matter/fsed_doc", 0.01, 1.0, theme = "sediment")
  add("aed_organic_matter/ksed_dom", 20, 300, theme = "sediment")

  # ---- organic matter ------------------------------------------------------------------------
  om <- function(...) add(..., theme = "organic matter")
  om("aed_organic_matter/rdom_minerl", 0.001, 0.05); om("aed_organic_matter/rpoc_hydrol", 0.001, 0.1)
  om("aed_organic_matter/rpon_hydrol", 1e-4, 0.01); om("aed_organic_matter/rpop_hydrol", 1e-5, 1e-3)
  om("aed_organic_matter/rdomr_minerl", 1e-5, 1e-3)
  om("aed_organic_matter/theta_hydrol", 1.02, 1.12, "lin")
  om("aed_organic_matter/theta_minerl", 1.02, 1.12, "lin")
  om("aed_organic_matter/kpom_hydrol", 3, 100); om("aed_organic_matter/kdom_minerl", 3, 100)
  om("aed_organic_matter/f_an", 0.05, 1, "lin"); om("aed_organic_matter/k_nit", 1, 30)
  om("aed_organic_matter/w_pom", -0.5, -0.001)
  om("aed_organic_matter/rcpom_bdown", 0.01, 0.2)

  # ---- nitrogen ------------------------------------------------------------------------------
  nn <- function(...) add(..., theme = "nitrogen")
  nn("aed_nitrogen/rnitrif", 0.01, 1); nn("aed_nitrogen/knitrif", 20, 400)
  nn("aed_nitrogen/theta_nitrif", 1.02, 1.12, "lin")
  nn("aed_nitrogen/rdenit", 1, 50); nn("aed_nitrogen/kdenit", 5, 100)
  nn("aed_nitrogen/theta_denit", 1.02, 1.12, "lin")
  nn("aed_nitrogen/rdnra", 0.001, 0.1); nn("aed_nitrogen/kdnra_oxy", 0.1, 2)

  # ---- phosphorus ----------------------------------------------------------------------------
  add("aed_phosphorus/kpo4p", 0.1, 20, theme = "phosphorus")
  add("aed_phosphorus/w_po4ads", -0.5, -0.001, theme = "phosphorus")

  # ---- phytoplankton traits, per group (index = column order in aed_phyto_pars.csv) ----------
  gi <- c(cyano = 1, green = 2, diatom = 3)
  ph <- function(name, ...) for (g in names(gi))
    add(name, ..., file = "aed_phyto_pars.csv", group = g, index = gi[[g]], theme = "phytoplankton")
  ph("R_growth", NA, NA)            # x0.4 .. x2 of base
  ph("I_K", 5, 100); ph("KePHY", 5e-4, 0.02)
  ph("R_resp", NA, NA)              # x0.2 .. x2.5 of base
  ph("theta_growth", 1.06, 1.12, "lin"); ph("k_fres", 0.1, 0.9, "lin"); ph("k_fdom", 0.01, 0.5)
  ph("K_N", 0.5, 15); ph("K_P", 0.02, 0.5)
  ph("X_nmin", 0.005, 0.03, "lin"); ph("X_nmax", 0.04, 0.12, "lin")
  ph("X_pmin", 0.0001, 0.0015, "lin"); ph("X_pmax", 0.003, 0.01, "lin")
  ph("R_nuptake", 0.005, 0.1); ph("R_puptake", 3e-4, 0.005)
  ph("Xcc", 20, 100, "lin")
  # sinking / buoyancy (positive = rising): cyanobacteria can rise, the others sink
  pw <- function(g, lo, hi) add("w_p", lo, hi, "lin", file = "aed_phyto_pars.csv", group = g,
                                index = gi[[g]], theme = "phytoplankton")
  pw("cyano", -0.2, 0.3); pw("green", -0.3, 0); pw("diatom", -0.5, -0.01)
  # cyanobacteria temperature response (fT_Method 1 for this group only)
  pt <- function(nm, lo, hi) add(nm, lo, hi, "lin", file = "aed_phyto_pars.csv", group = "cyano",
                                 index = 1, theme = "phytoplankton")
  # The AED temperature-function solver (aed_bio_temp_function in aed_util.F90) fails ("Cannot solve for
  # fT") for low theta_growth and for wide T_std/T_opt/T_max spreads; these ranges and theta_growth
  # >= 1.06 were checked by replicating the solver (about 100% success).
  pt("T_std", 15, 20); pt("T_opt", 25, 28); pt("T_max", 34, 38)

  # ---- alum (thesis-informed ranges, see alum_calib_table()) ----------------------------------
  al <- function(...) add(..., theme = "alum")
  al("aed_alum/w_ala", 0.2, 3); al("aed_alum/k_w", 0.1, 10); al("aed_alum/K_P", 0.02, 2)
  al("aed_alum/k_s", 0.02, 0.5); al("aed_alum/k_age_w", 0.02, 0.4); al("aed_alum/k_age_s", 0.02, 0.4)
  al("aed_alum/k_bur", 0.002, 0.1); al("aed_alum/k_res", 0.0005, 0.2)
  do.call(rbind, S)
}

# Default value of a spec row from the built configuration / phytoplankton database.
sa_default_value <- function(r, cfg_glm, phyto_csv) {
  if (r$file == "aed_phyto_pars.csv") {
    row <- phyto_csv[phyto_csv$p_name == r$name, , drop = FALSE]
    if (!nrow(row)) return(NA_real_)
    return(as.numeric(row[[r$group]]))
  }
  parts <- strsplit(r$name, "/")[[1]]
  sec <- if (r$file == "aed.nml") cfg_glm$bgc$aed[[parts[1]]] else cfg_glm$hydrodynamic[[parts[1]]]
  v <- sec[[parts[2]]]
  if (is.null(v)) v <- sec[[tolower(parts[2])]]
  if (is.null(v)) return(NA_real_)
  v <- suppressWarnings(as.numeric(v))
  if (!length(v)) return(NA_real_)
  if (!is.na(r$index) && length(v) >= r$index) v[r$index] else v[1]
}

# Ranges that are multiples of the base value
SA_REL <- list(
  "aed_sed_const2d/fsed_oxy" = c(3, 0.25),    # base is negative: min = 3x (more negative), max = 0.25x
  "aed_sed_const2d/fsed_amm" = c(0.1, 100),
  "aed_sed_const2d/fsed_frp" = c(0.2, 6),
  "R_growth" = c(0.4, 2), "R_resp" = c(0.2, 2.5))

# Assemble the full table: baseline (calibrated) rows fixed, SA rows with ranges.
sa_param_table <- function(baseline, cfg_glm, phyto_csv) {
  spec <- sa_spec()
  key <- function(d) paste(d$name, ifelse(is.na(d$group), "", d$group), ifelse(is.na(d$index), "", d$index))
  bk <- key(baseline)
  spec$value <- NA_real_
  for (i in seq_len(nrow(spec))) {
    j <- match(key(spec[i, ]), bk)
    # baseline phyto rows may carry a different index convention, so match on group
    if (is.na(j) && !is.na(spec$group[i]))
      j <- which(baseline$name == spec$name[i] & baseline$group %in% spec$group[i])[1]
    spec$value[i] <- if (!is.na(j)) baseline$value[j] else sa_default_value(spec[i, ], cfg_glm, phyto_csv)
  }
  rel <- spec$name %in% names(SA_REL) & is.na(spec$min)
  for (i in which(rel)) {
    f <- SA_REL[[spec$name[i]]]; v <- spec$value[i]
    spec$min[i] <- min(v * f); spec$max[i] <- max(v * f)
  }
  # the start value must lie inside the range: widen to include a calibrated value outside it
  spec$min <- pmin(spec$min, spec$value); spec$max <- pmax(spec$max, spec$value)
  miss <- is.na(spec$value) | is.na(spec$min)
  if (any(miss)) {
    message("SA parameters with no default (dropped): ", paste(key(spec)[miss], collapse = "; "))
    spec <- spec[!miss, ]
  }
  # other baseline rows stay fixed at their calibrated value
  fixed <- baseline[!bk %in% key(spec), ]
  fixed <- fixed[!(fixed$name %in% spec$name & fixed$group %in% spec$group[!is.na(spec$group)] &
                     fixed$file == "aed_phyto_pars.csv"), ]
  fixed$min <- fixed$value; fixed$max <- fixed$value
  fixed$scale <- NA_character_; fixed$theme <- "fixed"
  cols <- c("model", "file", "name", "value", "min", "max", "group", "index", "scale", "theme")
  rbind(spec[, cols], fixed[, cols])
}
