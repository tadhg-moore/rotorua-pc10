# Helpers for rotorua-alum-scenarios.qmd: reduce a GLM-AED output.nc (one per
# scenario, written by scripts/alum/01_run_scenarios.R) to daily lake-level
# phosphorus / alum quantities.
#
# Units: concentrations mmol P/m3, benthic pools mmol P/m2, masses returned in
# kg P (MW_P), loads in kg P/d.

MW_P   <- 30.973762e-3          # kg P per mol P  (=> mmol * MW_P * 1e-3 = kg)

MMOL_TO_KG <- 30.973762 * 1e-6

alum_run_dir <- function(tag, root = file.path("..", "scripts", "alum", "runs"))
  file.path(root, tag, "LID11133_rotorua", "glm_aed")

#' Read the (z, time) matrix of a water-column variable, masked to active layers.
nc_wc <- function(nc, v, NS) {
  x <- ncdf4::ncvar_get(nc, v)                     # [z, time]
  mask <- outer(seq_len(nrow(x)), NS, "<=")
  x[!mask | !is.finite(x) | abs(x) > 1e30] <- NA
  x
}

#' Daily lake-level series for one scenario.
#'
#' Volume-weighted whole-lake, surface-layer (top) and bottom-layer values of
#' the water-column variables; zone-summed benthic pools / fluxes (kg P, kg P/d);
#' water-column process rates integrated over the lake (kg P/d).
read_alum_run <- function(tag, dir = alum_run_dir(tag), cache = TRUE,
                          cache_dir = file.path("..", "scripts", "alum", "out"),
                          site_dir = ALUM_SITE_DIR) {
  # Committed, NetCDF-free summary (see export_alum_site_data()) takes priority so
  # the page renders on CI / any machine without the model output.
  sf <- file.path(site_dir, paste0("summary_", tag, ".rds"))
  cf <- file.path(cache_dir, paste0("04_summary_", tag, ".rds"))
  nc_file <- file.path(dir, "output", "output.nc")
  if (!file.exists(nc_file) && file.exists(sf)) return(readRDS(sf))
  if (cache && file.exists(cf) && file.mtime(cf) > file.mtime(nc_file))
    return(readRDS(cf))

  nc <- ncdf4::nc_open(nc_file); on.exit(ncdf4::nc_close(nc))
  t0 <- as.POSIXct("2011-07-02", tz = "UTC")
  tu <- strsplit(nc$dim$time$units, " since ")[[1]]
  time <- as.Date(as.POSIXct(tu[2], tz = "UTC") + ncdf4::ncvar_get(nc, "time") * 3600)
  NS <- as.integer(ncdf4::ncvar_get(nc, "NS"))
  V  <- nc_wc(nc, "V", NS)                          # m3 per layer
  Vt <- colSums(V, na.rm = TRUE)

  vw   <- function(x) colSums(x * V, na.rm = TRUE) / Vt     # volume-weighted mean
  vsum <- function(x) colSums(x * V, na.rm = TRUE)          # lake total (conc*V)
  top  <- function(x) vapply(seq_along(NS), \(j) x[NS[j], j], numeric(1))
  bot  <- function(x) x[1, ]

  g <- function(v) nc_wc(nc, v, NS)
  frp <- g("PHS_frp"); tp <- g("TOT_tp"); ads <- g("PHS_frp_ads")
  chl <- g("PHY_tchla"); ala <- g("ALU_ala"); alp <- g("ALU_alp")
  upw <- g("ALU_uptake_w"); fph <- g("ALU_fpH")
  setl <- g("PHS_frp_ads_set")

  # layer depths (m below the surface), oxygen and layer areas
  ztop <- ncdf4::ncvar_get(nc, "z")                          # layer top heights above bed
  zbot <- rbind(0, ztop[-nrow(ztop), , drop = FALSE])
  zsrf <- vapply(seq_along(NS), \(j) ztop[NS[j], j], numeric(1))
  Dm   <- sweep(-(ztop + zbot) / 2, 2, zsrf, "+")           # layer-centre depth
  Dm[!outer(seq_len(nrow(Dm)), NS, "<=")] <- NA
  oxy  <- g("OXY_oxy")                                       # mmol O2/m3
  O2MG <- 32 / 1000                                          # mmol O2/m3 -> mg/L
  dmean <- function(x, lo, hi) {                             # volume-weighted mean over a depth band
    w <- V * (!is.na(Dm) & Dm >= lo & Dm <= hi)
    colSums(x * w, na.rm = TRUE) / colSums(w, na.rm = TRUE)
  }
  vfrac <- function(mask) colSums(V * mask, na.rm = TRUE) / Vt
  anox_depth <- function(thr) vapply(seq_along(NS), \(j) {   # shallowest depth below which O2 < thr
    ok <- which(oxy[seq_len(NS[j]), j] * O2MG < thr)
    if (length(ok)) min(Dm[ok, j], na.rm = TRUE) else NA_real_ }, numeric(1))

  # sediment zones: GLM reports n_zones+1 entries; zones with zero area drop out
  za  <- ncdf4::ncvar_get(nc, "zarea")                      # [zone, time]
  zv  <- function(v) ncdf4::ncvar_get(nc, v)                # [zone, time]
  zsum <- function(v) colSums(zv(v) * za, na.rm = TRUE)     # per-m2 -> lake total

  lake <- tibble::tibble(
    Date = time, scenario = tag,
    volume_m3 = Vt,
    surface_area_m2 = as.numeric(ncdf4::ncvar_get(nc, "surface_area")),
    level     = ncdf4::ncvar_get(nc, "lake_level"),
    inflow_m3 = as.numeric(ncdf4::ncvar_get(nc, "tot_inflow_vol")),
    outflow_m3 = as.numeric(ncdf4::ncvar_get(nc, "tot_outflow_vol")),
    overflow_m3 = as.numeric(ncdf4::ncvar_get(nc, "overflow_vol")),
    evap_m3 = as.numeric(ncdf4::ncvar_get(nc, "evaporation")),
    temp_surf = top(g("temp")), temp_bot = bot(g("temp")),
    # concentrations (mmol P/m3, chl ug/L)
    frp_mean = vw(frp), frp_top = top(frp), frp_bot = bot(frp),
    tp_mean  = vw(tp),  tp_top  = top(tp),  tp_bot  = bot(tp),
    chl_mean = vw(chl), chl_top = top(chl),
    ads_mean = vw(ads),
    ala_mean = vw(ala), ala_top = top(ala), ala_bot = bot(ala),
    alp_mean = vw(alp), alp_top = top(alp), alp_bot = bot(alp),
    fpH_mean = vw(fph),
    # water-column pools (kg P)
    frp_kg = vsum(frp) * MMOL_TO_KG, tp_kg = vsum(tp) * MMOL_TO_KG,
    ads_kg = vsum(ads) * MMOL_TO_KG,
    ala_kg = vsum(ala) * MMOL_TO_KG, alp_kg = vsum(alp) * MMOL_TO_KG,
    # lake-integrated water-column rates (kg P/d)
    uptake_w_kgd = vsum(upw) * MMOL_TO_KG,
    ads_settle_kgd = vsum(setl) * MMOL_TO_KG,
    # benthic pools (kg P) and fluxes (kg P/d), summed over sediment zones
    ben_ala_kg = zsum("ALU_ben_ala_Z") * MMOL_TO_KG,
    ben_alp_kg = zsum("ALU_ben_alp_Z") * MMOL_TO_KG,
    uptake_s_kgd = zsum("ALU_uptake_s_Z") * MMOL_TO_KG,
    fsed_frp_kgd = zsum("SDF_Fsed_frp_Z") * MMOL_TO_KG,
    dsf_frp_kgd  = zsum("PHS_frp_dsf_Z") * MMOL_TO_KG,
    ads_swi_kgd  = zsum("PHS_frp_ads_swi_Z") * MMOL_TO_KG,
    sed_area_m2 = colSums(za, na.rm = TRUE),
    # epilimnetic (0-5 m) and hypolimnetic (>=15 m) means, for comparison with observations
    frp_epi = dmean(frp, 0, 5), frp_hyp = dmean(frp, 15, 99),
    tp_epi  = dmean(tp, 0, 5),  tp_hyp  = dmean(tp, 15, 99),
    chl_epi = dmean(chl, 0, 5),
    oxy_epi = dmean(oxy, 0, 5) * O2MG, oxy_hyp = dmean(oxy, 15, 99) * O2MG,
    oxy_bot = bot(oxy) * O2MG, oxy_top = top(oxy) * O2MG,
    # oxygen state of the water column
    anox_vol = vfrac(oxy * O2MG < 1), hypox_vol = vfrac(oxy * O2MG < 2),
    anox_depth = anox_depth(1),     # depth of the 1 mg/L isopleth (NA = none)
    hypox_depth = anox_depth(2)
  )

  # per-zone benthic detail (long)
  nz <- sum(is.finite(za[, ncol(za)]) & za[, ncol(za)] > 0)
  zone <- dplyr::bind_rows(lapply(seq_len(nz), \(k) tibble::tibble(
    Date = time, scenario = tag, zone = k, area_m2 = za[k, ],
    ben_ala = zv("ALU_ben_ala_Z")[k, ], ben_alp = zv("ALU_ben_alp_Z")[k, ],
    uptake_s = zv("ALU_uptake_s_Z")[k, ], fsed_frp = zv("SDF_Fsed_frp_Z")[k, ],
    dsf_frp = zv("PHS_frp_dsf_Z")[k, ])))

  # depth-time fields for the heat-map figures: keep daily surface/bottom only
  out <- list(lake = lake, zone = zone)
  if (cache) saveRDS(out, cf)
  out
}

#' Daily inflow loads (kg P/d) per stream from the inflow CSVs a scenario used.
#' flow is m3/s; concentrations mmol/m3. TP load = FRP + DOP + POP + adsorbed P.
read_alum_inflow_loads <- function(tag, dir = alum_run_dir(tag),
                                   start = NULL, stop = NULL,
                                   site_dir = ALUM_SITE_DIR) {
  sf <- file.path(site_dir, paste0("loads_", tag, ".rds"))
  fs <- list.files(file.path(dir, "bcs"), "^inflow_.*\\.csv$", full.names = TRUE)
  if (!length(fs) && file.exists(sf)) {
    out <- readRDS(sf)
    if (!is.null(start)) out <- dplyr::filter(out, Date >= as.Date(start))
    if (!is.null(stop))  out <- dplyr::filter(out, Date <= as.Date(stop))
    return(out)
  }
  dplyr::bind_rows(lapply(fs, \(f) {
    d <- utils::read.csv(f, check.names = FALSE)
    nm <- sub("^inflow_(.*)\\.csv$", "\\1", basename(f))
    col <- \(x) if (x %in% names(d)) d[[x]] else 0
    q <- d$flow * 86400                                          # m3/d
    tibble::tibble(
      Date = as.Date(d$time), stream = nm, flow_m3d = q,
      frp_kgd = q * col("PHS_frp") * MMOL_TO_KG,
      tp_kgd  = q * (col("PHS_frp") + col("OGM_dop") + col("OGM_pop") +
                       col("PHS_frp_ads")) * MMOL_TO_KG,
      ala_kgd = q * col("ALU_ala") * MMOL_TO_KG,   # P-binding capacity dosed
      al_kgd  = q * col("ALU_ala") * 26.981539e-6  # kg Al (1 Al : 1 P capacity)
    )
  })) -> out
  if (!is.null(start)) out <- dplyr::filter(out, Date >= as.Date(start))
  if (!is.null(stop))  out <- dplyr::filter(out, Date <= as.Date(stop))
  out
}


#' Observations (daily epilimnion / hypolimnion means) from the Aeme object a
#' scenario was run with. FRP and TP are mg/L; converted to ug P/L (x1000);
#' oxygen mg/L; chl-a ug/L. Epilimnion = 0-5 m, hypolimnion = 15-21 m (the model
#' lake is 21 m deep; the deeper pit in the survey data is not represented).
read_alum_obs <- function(aeme_rds = file.path("..", "scripts", "alum", "runs", "S1_aeme.rds"),
                          site_dir = ALUM_SITE_DIR) {
  sf <- file.path(site_dir, "obs_banded.rds")
  if (!file.exists(aeme_rds) && file.exists(sf)) return(readRDS(sf))
  o <- AEME::observations(readRDS(aeme_rds))$lake
  o <- o[o$var_aeme %in% c("PHS_frp", "PHS_tp", "PHY_tchla", "CHM_oxy"), ]
  o$Date <- as.Date(o$Date)
  band <- ifelse(o$depth <= 5, "epi", ifelse(o$depth >= 15 & o$depth <= 21, "hyp", NA))
  o$band <- band
  scale <- c(PHS_frp = 1000, PHS_tp = 1000, PHY_tchla = 1, CHM_oxy = 1)
  o$value <- o$value * scale[o$var_aeme]
  o <- o[!is.na(o$band), ]
  o |> dplyr::group_by(Date, var_aeme, band) |>
    dplyr::summarise(obs = mean(value, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
}

# ---- NetCDF-free site data -------------------------------------------------
# website/data/alum/ holds small summaries that are committed so the page renders
# on CI / other machines without scripts/alum/runs (output.nc, 50+ MB each).
ALUM_SITE_DIR <- file.path("data", "alum")

#' Write the committed summaries from the model output. Run locally after
#' (re)running scenarios: Rscript scripts/alum/02_export_site_data.R
export_alum_site_data <- function(tags, site_dir = ALUM_SITE_DIR,
                                  start = "2011-07-01") {
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
  for (tg in tags) {
    r <- read_alum_run(tg, site_dir = tempfile())
    if (tg != "S1") r$zone <- r$zone[0, ]          # per-zone detail is only plotted for S1
    saveRDS(r, file.path(site_dir, paste0("summary_", tg, ".rds")), compress = "xz")
    il <- read_alum_inflow_loads(tg, start = start, site_dir = tempfile())
    if (tg != "S1") il <- il[il$stream %in% c("Puarenga", "Utuhina"), ]   # only these carry alum
    saveRDS(il, file.path(site_dir, paste0("loads_", tg, ".rds")), compress = "xz")
  }
  aeme_rds <- file.path("..", "scripts", "alum", "runs", "S1_aeme.rds")
  saveRDS(read_alum_obs(aeme_rds, site_dir = tempfile()), file.path(site_dir, "obs_banded.rds"), compress = "xz")
  saveRDS(read_alum_obs_raw(aeme_rds), file.path(site_dir, "obs_oxy_deep.rds"), compress = "xz")
  invisible(list.files(site_dir))
}

#' Raw observed oxygen at 15-21 m (mg/L), for the hypoxia comparison.
read_alum_obs_raw <- function(aeme_rds = file.path("..", "scripts", "alum", "runs", "S1_aeme.rds"),
                              site_dir = ALUM_SITE_DIR) {
  sf <- file.path(site_dir, "obs_oxy_deep.rds")
  if (!file.exists(aeme_rds) && file.exists(sf)) return(readRDS(sf))
  o <- AEME::observations(readRDS(aeme_rds))$lake
  o <- o[o$var_aeme == "CHM_oxy" & o$depth >= 15 & o$depth <= 21, c("Date", "value", "depth")]
  o$Date <- as.Date(o$Date)
  o
}
