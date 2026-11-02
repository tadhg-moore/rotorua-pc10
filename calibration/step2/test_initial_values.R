# How much do the ~1000x-too-low initial water-quality values (init_profiles/wq_init_vals) matter after the 540-day spin-up?
# Runs the 2b best member over a short window with the initial values as built and with them reset to the aed.nml *_initial values
# (mmol/m3), then compares the surface / bottom series year by year.
#   Rscript calibration/step2/test_initial_values.R      (IV_START / IV_STOP, default 1990-01-01 / 1996-12-31)
Sys.setenv(S2_STAGE = "b", S2_DIR = file.path("calibration", "step2", "initval_test"))
source("calibration/step2/step2_common.R"); suppressMessages(library(ncdf4))
SA_START <- Sys.getenv("IV_START", "1990-01-01"); SA_STOP <- Sys.getenv("IV_STOP", "1996-12-31")
b <- step2_build("iv"); best <- readRDS("calibration/step2b/runs/step2_best.rds")$best
nml <- readLines(list.files(b$path, "^aed.nml$", recursive = TRUE, full.names = TRUE)[1])
nmlval <- function(key) as.numeric(sub(".*= *", "", grep(sprintf("^ *%s *=", key), nml, ignore.case = TRUE, value = TRUE)[1]))
fixed <- c(OGM_doc = nmlval("doc_initial"), OGM_poc = nmlval("poc_initial"), OXY_oxy = nmlval("oxy_initial"), NIT_amm = nmlval("amm_initial"),
           OGM_don = nmlval("don_initial"), NIT_nit = nmlval("nit_initial"), OGM_pon = nmlval("pon_initial"), OGM_dop = nmlval("dop_initial"),
           PHS_frp = nmlval("frp_initial"), OGM_pop = nmlval("pop_initial"))

series <- function(aeme) {
  x <- nc_open(file.path(AEME::get_lake_dir(aeme, b$path), "glm_aed", "output", "output.nc")); on.exit(nc_close(x))
  t0 <- ncatt_get(x, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); tm <- ncvar_get(x, "time")
  dts <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm; NS <- ncvar_get(x, "NS")
  top <- function(v) { m <- ncvar_get(x, v); sapply(seq_along(NS), function(i) m[NS[i], i]) }
  bot <- function(v) { m <- ncvar_get(x, v); m[1, ] }
  data.frame(Date = dts, frp = top("PHS_frp"), nit = top("NIT_nit"), amm = top("NIT_amm"), chl = top("PHY_tchla"), tp = top("TOT_tp"), tn = top("TOT_tn"),
             oxy_bot = bot("OXY_oxy"), doc = top("OGM_doc"))
}
run_case <- function(fix) {
  a <- b$aeme; cfg <- AEME::configuration(a); ip <- cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]]
  v <- matrix(ip$wq_init_vals, nrow = ip$num_depths); colnames(v) <- ip$wq_names
  if (fix) for (n in names(fixed)) v[, n] <- fixed[[n]]
  cfg[["glm_aed"]][["hydrodynamic"]][["init_profiles"]][["wq_init_vals"]] <- as.vector(v)
  AEME::configuration(a) <- cfg
  AEME::write_configuration(aeme = a, model = MODEL, path = b$path, include_boundary = FALSE)
  res <- aemetools::run_aeme_param(a, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
  stopifnot(inherits(res, "Aeme")); series(res)
}
A <- run_case(FALSE); B <- run_case(TRUE)
cat("\ninitial values as built (first depth):", paste(names(fixed), signif(sapply(names(fixed), function(n) 0), 1)), "\n")
ym <- function(d) { d$yr <- as.integer(format(d$Date, "%Y")); aggregate(. ~ yr, d[, setdiff(names(d), "Date")], mean) }
cat("\nAnnual means, as built (A) vs reset initial values (B):\n"); print(cbind(ym(A), B = round(ym(B)[, -1], 4)), digits = 3)
d <- merge(A, B, by = "Date", suffixes = c("_A", "_B")); d$days <- as.numeric(d$Date - min(d$Date))
for (v in c("frp", "nit", "amm", "chl", "tp", "tn", "oxy_bot", "doc")) d[[paste0("rel_", v)]] <- abs(d[[paste0(v, "_A")]] - d[[paste0(v, "_B")]]) / pmax(abs(d[[paste0(v, "_B")]]), 1e-6)
d$yr <- as.integer(format(d$Date, "%Y"))
cat("\nMean absolute relative difference A vs B, by year:\n"); print(round(aggregate(cbind(rel_frp, rel_nit, rel_amm, rel_chl, rel_tp, rel_tn, rel_oxy_bot, rel_doc) ~ yr, d, mean), 3), row.names = FALSE)
saveRDS(list(A = A, B = B), file.path("calibration", "step2", "initval_test", "series.rds"))
