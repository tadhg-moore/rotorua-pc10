# Historical ensemble (2005-01-01 to 2022-06-29): the top members of a calibration stage ensemble run on the stage's base, alum on.
#   Rscript calibration/historical/run_hist.R <worker> <n_workers>
# Env: HIST_SRC (default calibration/step2s/runs), HIST_BASE (default calibration/alum/stream/base_Q50.rds), HIST_N (20 members), HIST_TAG (default s)
# Member parameters: last-iteration ensemble of HIST_SRC/step2_ies.rds, lowest phi first (the table values are used for every parameter that is not varied).
# Writes calibration/historical/out_<tag>/member_<i>.rds: obs/sim pairs (csps_pair) and daily surface (0-5 m) and bottom (lowest 3 m) means of the main variables, and deletes the run folder.
.libPaths(c(.libPaths(), "C:/Users/mooret/AppData/Local/R/win-library/4.5"))   # arrow (for AEME::write_aeme_parquet) is in the user library, not the renv one
if (nzchar(Sys.getenv("HIST_LIB"))) .libPaths(c(Sys.getenv("HIST_LIB"), .libPaths()))   # optional: a library placed first (e.g. an older AEME for a comparison)
args <- commandArgs(TRUE); wk <- as.integer(args[1]); nw <- as.integer(args[2])
src <- Sys.getenv("HIST_SRC", "calibration/step2s/runs"); basef <- Sys.getenv("HIST_BASE", "calibration/alum/stream/base_Q50.rds"); nmem <- as.integer(Sys.getenv("HIST_N", "20")); tg <- Sys.getenv("HIST_TAG", "s")
outdir <- file.path("calibration", "historical", paste0("out_", tg)); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
suppressPackageStartupMessages({library(AEME); library(dplyr)})
if (nzchar(Sys.getenv("HIST_GLM_EXEC"))) options(AEME.glm_exec = Sys.getenv("HIST_GLM_EXEC"))   # optional: run with a specific GLM executable
cat("GLM exe:", AEME::glm_exe_path(), "\n")
x <- readRDS(file.path(src, "step2_ies.rds")); h <- x$hist[[length(x$hist)]]; tab <- x$tab; vary <- x$vary
val <- function(u, r) if (identical(r$scale, "log") && r$min * r$max > 0) { lo <- log(min(abs(r$min), abs(r$max))); hi <- log(max(abs(r$min), abs(r$max))); sign(r$min) * exp(lo + u * (hi - lo)) } else r$min + u * (r$max - r$min)   # as in step2_ies.R
ord <- order(h$phi); ord <- ord[is.finite(h$phi[ord])]; ord <- head(ord, nmem)
mine <- ord[seq_along(ord) %% nw == wk %% nw]
cat("worker", wk, "of", nw, "members", mine, "\n")
Sys.setenv(S2_STAGE = "p", S2_ALUM = "1", S2_ALP = "1", S2_BASE = basef, S2_START = Sys.getenv("HIST_START", "2005-01-01"), S2_STOP = Sys.getenv("HIST_STOP", "2022-06-29"), S2_DIR = file.path("calibration", "historical", paste0("work_", tg, "_", wk)),
           S2_ALUM_FROM = file.path(src, "step2_best.rds"), S2_OVERLAY = file.path(src, "step2_best.rds"))
source("calibration/step2/step2_common.R"); source("calibration/step2/alum_resid.R")
b <- step2_build(paste0("hist", wk))
nml <- list.files(b$path, "glm4?[.]nml$", recursive = TRUE, full.names = TRUE)[1]; xx <- readLines(nml); i <- grep("^\\s*inflow_vars", xx)
if (!any(grepl("ALU_alp", xx[i]))) { xx[i] <- paste0(xx[i], ",'ALU_alp'"); writeLines(xx, nml) }
inf <- AEME::inflows(readRDS(basef))$data
for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE); nm <- sub("^inflow_(.*)[.]csv$", "\\1", basename(f)); a <- inf[[nm]]
  d$ALU_alp <- if (!is.null(a) && "ALU_alp" %in% names(a)) { v <- a$ALU_alp[match(as.Date(d$time), as.Date(a$Date))]; v[is.na(v)] <- 0; v } else 0
  write.csv(d, f, row.names = FALSE, quote = FALSE) }
# daily() summarises every layer-resolved variable of the GLM output as top-5 m mean (_surf), bottom-3 m mean (_bot) and whole-lake volume-weighted mean (_lake),
# and keeps every time-only variable (lake level, ice, inflow and outflow volumes, ...). Every output variable and depth is also written with AEME::write_aeme_parquet (parquet_<tag>/member_<m>).
daily <- function(res) { f <- AEME::get_model_outfile(res)$glm_aed[["output"]]; nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  z <- ncdf4::ncvar_get(nc, "z"); ns <- ncdf4::ncvar_get(nc, "NS"); tm <- ncdf4::ncvar_get(nc, "time"); t0 <- ncdf4::ncatt_get(nc, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); dd <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  nt <- length(ns); nl <- nrow(z); valid <- row(z) <= matrix(ns, nl, nt, byrow = TRUE); zv <- ifelse(valid, z, NA)
  zmax <- apply(zv, 2, max, na.rm = TRUE); zmin <- apply(zv, 2, min, na.rm = TRUE); ms <- valid & z >= matrix(zmax - 5, nl, nt, byrow = TRUE); mb <- valid & z <= matrix(zmin + 3, nl, nt, byrow = TRUE)
  V <- if ("V" %in% names(nc$var)) ncdf4::ncvar_get(nc, "V") else NULL; wl <- NULL
  if (!is.null(V)) { dV <- rbind(V[1, ], apply(V, 2, diff)); wl <- ifelse(valid, if (all(apply(ifelse(valid, V, NA), 2, \(x) all(diff(x[is.finite(x)]) >= 0)))) dV else V, 0) }
  out <- data.frame(Date = dd); if (!is.null(wl)) out$Vol <- colSums(wl)
  for (v in names(nc$var)) { if (v %in% c("z", "V", "NS", "time")) next; a <- tryCatch(ncdf4::ncvar_get(nc, v), error = function(e) NULL); if (is.null(a) || !is.numeric(a)) next
    if (is.matrix(a) && nrow(a) == nl && ncol(a) == nt) { a[!is.finite(a) | abs(a) > 1e20] <- NA; ok <- function(m) m & !is.na(a)
      out[[paste0(v, "_surf")]] <- colSums(ifelse(ok(ms), a, 0)) / pmax(colSums(ok(ms)), 1) ; out[[paste0(v, "_bot")]] <- colSums(ifelse(ok(mb), a, 0)) / pmax(colSums(ok(mb)), 1)
      if (!is.null(wl)) out[[paste0(v, "_lake")]] <- colSums(ifelse(!is.na(a), a, 0) * wl) / colSums(wl * !is.na(a))
    } else if (is.null(dim(a)) && length(a) == nt) out[[v]] <- as.numeric(a) }
  out }
for (m in mine) { of <- file.path(outdir, paste0("member_", m, ".rds")); if (file.exists(of)) next
  p <- tab; for (j in seq_along(vary)) p$value[vary[j]] <- val(h$U[m, j], tab[vary[j], ])
  res <- tryCatch(aemetools::run_aeme_param(b$aeme, param = p[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE), error = function(e) { cat("run failed", m, conditionMessage(e), "\n"); NULL })
  if (!inherits(res, "Aeme")) next
  if (Sys.getenv("HIST_KEEP") == "1") saveRDS(res, file.path(outdir, paste0("aeme_", m, ".rds")))   # optional: keep the Aeme object (and the run folder, see the end of the script)
  P <- csps_pair(res, vars = c("CHM_oxy", "PHS_tp", "PHS_frp", "PHY_tchla", "NIT_tn", "HYD_temp"))
  D <- tryCatch(daily(res), error = function(e) { cat("daily failed", conditionMessage(e), "\n"); NULL })
  if (Sys.getenv("HIST_PARQUET", "1") == "1") tryCatch(AEME::write_aeme_parquet(res, out_root = file.path("calibration", "historical", paste0("parquet_", tg), paste0("member_", m)), model = MODEL), error = function(e) cat("parquet write failed", m, conditionMessage(e), "\n"))   # every output variable and depth, one store per member (read with AEME::read_aeme_parquet)
  saveRDS(list(member = m, phi = h$phi[m], P = P, daily = D, params = p[vary, c("name", "group", "index", "value")]), of); cat("done member", m, "phi", round(h$phi[m], 3), "\n") }
if (Sys.getenv("HIST_KEEP") != "1") unlink(S2_DIR_WORK <- file.path("calibration", "historical", paste0("work_", tg, "_", wk)), recursive = TRUE); cat("worker done\n")
