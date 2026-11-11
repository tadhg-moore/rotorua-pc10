# Nitrogen and phosphorus budgets for a finished GLM-AED run.
#
#   b  <- nutrient_budget(aeme, path = path, from = "1991-01-01", to = "2009-12-31")
#   budget_table(list(`2b` = b2b, `2c` = b2c), "N")        # one column per run, t/yr
#   plot_nutrient_budget(list(`2b` = b2b, `2c` = b2c))      # boundary and source / sink terms
#
# Needs the run to have written the GLM mass balance (AEME::set_glm_balance_groups() before the run; GLM v4) and
# output.nc. Boundary terms (inflow, outflow, change in lake stock) come from the mass balance, so the internal net
# loss is what is left once those are taken off. Process terms are volume-integrated rates (mmol/m3/d) or lake-area
# fluxes (mmol/m2/d) from output.nc.
# Caveats: the inflow / outflow terms leave out particulate inorganic P (carried on suspended solids) and the
# refractory DON / DOP pools, and the sediment-surface (swi) flux terms have not been reconciled with production.

#' N and P budgets (t/yr) of a finished GLM-AED run
#'
#' @param aeme Aeme object that was run (its hypsograph gives the layer volumes).
#' @param path Model build directory (the folder holding `LID.../glm_aed`); used to find `output.nc`
#'   and the mass-balance file. Ignored when `nc` is given.
#' @param nc Path to `output.nc`, if not under `path`.
#' @param mass_balance Daily by-variable mass balance (`AEME::diag_glm_mass_balance(..., by = "var")$daily`);
#'   read from the run when `NULL`.
#' @param from,to Averaging window (dates). Leave the spin-up out.
#' @param model Model name for the lake directory lookup.
#' @return A list: `N` and `P` (named numeric vectors, t/yr, negative = loss from the lake),
#'   `years`, and mean sediment `zone_amm` / `zone_frp` fluxes (mmol/m2/d).
#' @export
nutrient_budget <- function(aeme, path = NULL, nc = NULL, mass_balance = NULL,
                            from = NULL, to = NULL, model = "glm_aed") {
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("ncdf4 is needed")
  MWN <- 14.0067; MWP <- 30.974
  if (is.null(nc)) nc <- file.path(AEME::get_lake_dir(aeme, path), model, "output", "output.nc")
  x <- ncdf4::nc_open(nc); on.exit(ncdf4::nc_close(x))
  t0 <- ncdf4::ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncdf4::ncvar_get(x, "time")
  dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  from <- if (is.null(from)) min(dts) else as.Date(from); to <- if (is.null(to)) max(dts) else as.Date(to)
  keep <- which(dts >= from & dts <= to); if (length(keep) < 30) stop("fewer than 30 output days in the window")
  ny <- length(keep) / 365.25
  M <- function(v) tryCatch(ncdf4::ncvar_get(x, v), error = function(e) NULL)
  NS <- M("NS"); Z <- M("z"); sa <- M("surface_area")
  hyps <- AEME::get_hypsograph(aeme)
  vol <- matrix(0, nrow(Z), ncol(Z))
  for (i in keep) { n <- NS[i]; h <- Z[seq_len(n), i]; th <- diff(c(0, h)); dep <- (h - th / 2) - h[n]
    vol[seq_len(n), i] <- th * stats::approx(hyps$depth, hyps$area, xout = dep, rule = 2)$y }
  tpy <- function(mmol_d, mw) mmol_d * mw * 365.25 * 1e-9                          # mmol/d -> t/yr
  vint <- function(v, mw) { m <- M(v); if (is.null(m)) return(NA_real_)             # volume-integrated rate (mmol/m3/d)
    tpy(mean(colSums(m[, keep, drop = FALSE] * vol[, keep, drop = FALSE], na.rm = TRUE)), mw) }
  vint_sum <- function(vs, mw) sum(vapply(vs, vint, 1, mw = mw), na.rm = TRUE)
  aflux <- function(v, mw) { m <- M(v); if (is.null(m)) return(NA_real_); tpy(mean(m[keep] * sa[keep]), mw) }   # mmol/m2/d x lake area
  pool <- function(v, mw) { m <- M(v); if (is.null(m)) return(NA_real_); k <- range(keep)                         # mmol/m2 pool change x area, per year
    (m[k[2]] - m[k[1]]) * mean(sa[keep]) * mw * 1e-9 / ny }
  grp <- c("green", "cyano", "diatom")

  if (is.null(mass_balance)) mass_balance <- AEME::diag_glm_mass_balance(aeme = aeme, path = path, by = "var")$daily
  mb <- mass_balance; mb$date <- as.Date(mb$datetime); mb <- mb[mb$date >= from & mb$date <= to, ]
  mbs <- function(nm, mw) {
    z <- stats::aggregate(cbind(inflow, outflow, lake) ~ date, mb[mb$name %in% nm, ], sum); z <- z[order(z$date), ]
    c(inflow = tpy(mean(z$inflow), mw), outflow = tpy(mean(z$outflow), mw), d_stock = (utils::tail(z$lake, 1) - z$lake[1]) * mw * 1e-9 / ny) }
  bn <- mbs(c("NIT_amm", "NIT_nit", "OGM_don", "OGM_pon", paste0("PHY_", grp, "_IN")), MWN)
  bp <- mbs(c("PHS_frp", "OGM_dop", "OGM_pop", paste0("PHY_", grp, "_IP")), MWP)

  N <- c(`Inflow` = bn[["inflow"]], `Outflow` = -bn[["outflow"]], `Change in lake stock` = -bn[["d_stock"]], `= Internal net loss` = NA,
         `Denitrification (nitrogen module, water column)` = -vint("NIT_denit", MWN),
         `Denitrification (organic-matter module, simDenitrification)` = -vint("OGM_doc_denit", MWN), `Anammox` = -vint("NIT_anammox", MWN), `DNRA (NO3 -> NH4)` = vint("NIT_dnra", MWN),
         `Sediment NH4 flux (+ = release)` = aflux("SDF_Fsed_amm", MWN), `Sediment NO3 flux (+ = release)` = aflux("SDF_Fsed_nit", MWN),
         `N fixation (N2 uptake)` = vint("PHY_upt_n2", MWN),
         `[internal] Nitrification (NH4 -> NO3)` = vint("NIT_nitrif", MWN), `[internal] Phytoplankton uptake NO3` = vint("PHY_upt_no3", MWN),
         `[internal] Phytoplankton uptake NH4` = vint("PHY_upt_nh4", MWN),
         `[internal] Phytoplankton gross N production (gpp_n)` = vint_sum(paste0("PHY_", grp, "_gpp_n"), MWN), `[internal] DON mineralisation` = vint("OGM_don_min", MWN),
         `[internal] PON hydrolysis` = vint("OGM_pon_hyd", MWN), `[internal] Phytoplankton mortality` = vint_sum(paste0("PHY_", grp, "_mor_n"), MWN),
         `[swi, unreconciled] Net PON flux at the sediment surface` = aflux("OGM_pon_swi", MWN), `[swi, unreconciled] Net DON flux` = aflux("OGM_don_swi", MWN),
         `[swi, unreconciled] Net phytoplankton N flux` = aflux("PHY_phy_swi_n", MWN), `[swi] PON resuspension` = aflux("OGM_pon_res", MWN),
         `[swi] Change in sediment organic N pool` = pool("OGM_ton_sed", MWN))
  N["= Internal net loss"] <- -(N[["Inflow"]] + N[["Outflow"]] + N[["Change in lake stock"]])
  P <- c(`Inflow` = bp[["inflow"]], `Outflow` = -bp[["outflow"]], `Change in lake stock` = -bp[["d_stock"]], `= Internal net loss` = NA,
         `Sediment FRP flux (+ = release)` = aflux("SDF_Fsed_frp", MWP), `Adsorbed-P net flux at the sediment (+ = up)` = aflux("PHS_frp_ads_swi", MWP),
         `[internal] FRP adsorption to particles` = vint("PHS_frp_srp", MWP), `[internal] Phytoplankton uptake FRP` = vint("PHY_upt_po4", MWP),
         `[internal] DOP mineralisation` = vint("OGM_dop_min", MWP), `[internal] POP hydrolysis` = vint("OGM_pop_hyd", MWP),
         `[internal] Phytoplankton mortality` = vint_sum(paste0("PHY_", grp, "_mor_p"), MWP),
         `[swi, unreconciled] Net POP flux at the sediment surface` = aflux("OGM_pop_swi", MWP), `[swi, unreconciled] Net DOP flux` = aflux("OGM_dop_swi", MWP),
         `[swi, unreconciled] Net phytoplankton P flux` = aflux("PHY_phy_swi_p", MWP), `[swi] POP resuspension` = aflux("OGM_pop_res", MWP),
         `[swi] Change in sediment organic P pool` = pool("OGM_top_sed", MWP))
  P["= Internal net loss"] <- -(P[["Inflow"]] + P[["Outflow"]] + P[["Change in lake stock"]])
  zf <- function(v) { m <- M(v); if (is.null(m)) return(NULL); round(rowMeans(m[, keep, drop = FALSE]), 4) }
  # concentrations: lake-mean of the surface layer, and the share of FRP held as adsorbed P (is sorption doing anything?)
  adsorbed_share <- { f <- M("PHS_frp"); a <- M("PHS_frp_ads"); if (is.null(f) || is.null(a)) NA_real_ else mean(a[, keep], na.rm = TRUE) / mean(f[, keep], na.rm = TRUE) }
  structure(list(N = N, P = P, years = ny, from = from, to = to, zone_amm = zf("SDF_Fsed_amm_Z"), zone_frp = zf("SDF_Fsed_frp_Z"),
                 adsorbed_share = adsorbed_share), class = "nutrient_budget")
}

#' Side-by-side budget table (t/yr), one column per run
#' @param budgets Named list of [nutrient_budget()] results.
#' @param element `"N"` or `"P"`.
#' @export
budget_table <- function(budgets, element = c("N", "P")) {
  element <- match.arg(element)
  if (inherits(budgets, "nutrient_budget")) budgets <- list(run = budgets)
  out <- as.data.frame(do.call(cbind, lapply(budgets, function(b) round(b[[element]], 1))))
  out <- cbind(term = rownames(out), out); rownames(out) <- NULL; out
}

#' Print method: both budgets and the sediment fluxes by zone
#' @export
print.nutrient_budget <- function(x, ...) {
  cat(sprintf("Budget %s to %s (%.1f years); t/yr, negative = loss from the lake\n", format(x$from), format(x$to), x$years))
  cat("\nNitrogen\n"); print(budget_table(x, "N"), row.names = FALSE)
  cat("\nPhosphorus (excludes particulate inorganic P)\n"); print(budget_table(x, "P"), row.names = FALSE)
  cat("\nMean sediment flux by zone (mmol/m2/d)\n  NH4:", x$zone_amm, "\n  FRP:", x$zone_frp, "\n")
  cat(sprintf("Adsorbed P / FRP concentration: %.3f\n", x$adsorbed_share)); invisible(x)
}

#' Bar chart of the boundary and source / sink terms for one or more runs
#' (the `[internal]`, `[swi]` rows and the net loss are left out)
#' @export
plot_nutrient_budget <- function(budgets) {
  if (inherits(budgets, "nutrient_budget")) budgets <- list(run = budgets)
  long <- function(el, lab) { t <- budget_table(budgets, el); t$nutrient <- lab; t }
  d <- rbind(long("N", "Nitrogen (t N/yr)"), long("P", "Phosphorus (t P/yr)"))
  d <- d[!grepl("^\\[", d$term) & d$term != "= Internal net loss", ]
  d <- stats::reshape(d, direction = "long", varying = names(budgets), v.names = "t_yr", timevar = "run", times = names(budgets), idvar = c("term", "nutrient"))
  d <- d[is.finite(d$t_yr), ]; d$term <- factor(d$term, rev(unique(d$term)))
  ggplot2::ggplot(d, ggplot2::aes(.data$t_yr, .data$term, fill = .data$run)) + ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_vline(xintercept = 0) + ggplot2::facet_wrap(~nutrient, scales = "free", ncol = 1) +
    ggplot2::labs(x = "t per year", y = NULL, fill = NULL) + ggplot2::theme_minimal(base_size = 9)
}
