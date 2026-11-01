# In-stream alum reaction variants for the 2005-2022 alum-ON run (2005-2022 window, O member).
#   Rscript calibration/alum/stream_variant.R <name> <mU> <mP>          (env: STR_KSCALE = 0.85, STR_START, STR_STOP)
# Inflows: the no-alum populated file for the phosphorus of every stream (the old base had the with-alum series for the Utuhina and the Ungauged-Quick inflow, and the no-alum series for the Puarenga).
# In-stream reaction on the dosed streams (Utuhina ~700 m, Puarenga ~300 m from the lake): a fraction f of the inflow FRP reacts with the dosed alum before the lake
#   Puarenga: f = mP * 0.9 * min(1, R / 23)   (McIntosh paired up/downstream data, Hamilton et al. 2015 Fig 20: ramp to a 90% cap at Al:DRP ~23; R = ALU_ala / FRP, mol/mol)
#   Utuhina:  f = mU * 0.45                    (measured downstream vs reconstructed untreated DRP, 2007+ dosed days; Hamilton et al. 2015 p36)
# The bound P is delivered as ALU_alp (suspended alum-bound P) and ALU_ala is reduced by the same amount (mass conserving).  m = 0: no reaction; m = 1: as above.
# k_w and k_s are scaled by STR_KSCALE (the mean pH efficiency fpH ~0.85 from the buoy pH record).
args <- commandArgs(TRUE); vname <- args[1]; mU <- as.numeric(args[2]); mP <- as.numeric(args[3]); kscale <- as.numeric(Sys.getenv("STR_KSCALE", "0.85"))
suppressPackageStartupMessages({library(AEME); library(dplyr); library(readr)})
outdir <- file.path("calibration", "alum", "stream"); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
MMOL <- 1000 / 30.974
base <- readRDS("calibration/step3/aeme_base_2005_alum.rds")
n <- read_csv("data/raw/flows/rotorua_inflows_populated_noalum.csv", show_col_types = FALSE)
inf <- AEME::inflows(base)$data
for (s in c("Utuhina", "Ungauged-Quick")) {                       # swap in the untreated P series
  f <- n[n$Inflow == s, ]; f$Date <- as.Date(f$Date); d <- inf[[s]]; d$Date <- as.Date(d$Date); i <- match(d$Date, f$Date); k <- !is.na(i)
  cm <- c(PHS_frp = "DRP", PHS_dop = "DOPL", PHS_pop = "POPL", PHS_pip = "PIP"); for (v in names(cm)) d[[v]][k] <- f[[cm[[v]]]][i[k]]
  inf[[s]] <- d }
fs <- as.numeric(Sys.getenv("STR_FLOODSCALE", "1"))                      # sensitivity: scale the particulate P (POP, PIP) delivered on flood days (isFlood flag of the file) in every flagged inflow
if (fs != 1) for (s in names(inf)) { f <- n[n$Inflow == s, ]; if (!nrow(f) || all(is.na(f$isFlood))) next; f$Date <- as.Date(f$Date); d <- inf[[s]]; d$Date <- as.Date(d$Date)
  fl <- f$isFlood[match(d$Date, f$Date)] %in% 1; for (v in c("PHS_pop", "PHS_pip")) d[[v]][fl] <- d[[v]][fl] * fs; inf[[s]] <- d
  cat("flood scaling", fs, "applied to", s, ":", sum(fl), "days\n") }
rule <- list(Utuhina = function(R) rep(0.45, length(R)), Puarenga = function(R) 0.9 * pmin(1, R / 23)); mult <- c(Utuhina = mU, Puarenga = mP)
for (s in names(mult)) { d <- inf[[s]]; frp <- d$PHS_frp * MMOL; dose <- d$ALU_ala; R <- ifelse(frp > 0, dose / frp, 0)
  f <- mult[[s]] * rule[[s]](R); f[dose <= 0 | frp <= 0] <- 0
  B <- pmin(f * frp, dose); f <- ifelse(frp > 0, B / frp, 0)             # B = bound P, mmol P/m3 of inflow
  d$PHS_frp <- d$PHS_frp * (1 - f); d$ALU_ala <- dose - B; d$ALU_alp <- B; inf[[s]] <- d
  z <- d$Date >= "2011-01-01" & d$Date <= "2017-12-31"; cat(s, ": mean f on 2011-17 dosed days", round(mean(f[z & dose > 0]), 2), "| bound P load 2011-17 (kg P/yr)", round(sum(B[z] * d$HYD_flow[z]) * 30.974 / 1e6 / 7), "\n") }
base <- base |> AEME::remove_inflow(all = TRUE) |> AEME::add_inflows(inf)
bf <- file.path(outdir, paste0("base_", vname, ".rds")); saveRDS(base, bf)
Sys.setenv(S2_STAGE = "o", S2_ALUM = "1", S2_BASE = bf, S2_START = Sys.getenv("STR_START", "2005-01-01"), S2_STOP = Sys.getenv("STR_STOP", "2022-06-29"), S2_DIR = file.path(outdir, paste0("run_", vname)),
           S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds")
source("calibration/step2/step2_common.R"); source("calibration/step2/alum_resid.R")
b <- step2_build(vname)
# AEME names the pool ALU_alap and the calibration build leaves it out of the inflow variables, so the stream-bound P (ALU_alp) is added by hand:
# an ALU_alp column in every built inflow file and 'ALU_alp' in inflow_vars of the GLM namelist.
nml <- list.files(b$path, "glm4?[.]nml$", recursive = TRUE, full.names = TRUE)[1]; x <- readLines(nml); i <- grep("^\\s*inflow_vars", x)
if (!any(grepl("ALU_alp", x[i]))) { x[i] <- paste0(x[i], ",'ALU_alp'"); writeLines(x, nml) }
for (f in list.files(b$path, "^inflow_.*[.]csv$", recursive = TRUE, full.names = TRUE)) { d <- read.csv(f, check.names = FALSE); nm <- sub("^inflow_(.*)[.]csv$", "\\1", basename(f)); a <- inf[[nm]]
  d$ALU_alp <- if (!is.null(a) && "ALU_alp" %in% names(a)) { v <- a$ALU_alp[match(as.Date(d$time), as.Date(a$Date))]; v[is.na(v)] <- 0; v } else 0
  write.csv(d, f, row.names = FALSE, quote = FALSE) }
dU <- read.csv(list.files(b$path, "^inflow_Utuhina[.]csv$", recursive = TRUE, full.names = TRUE)[1], check.names = FALSE)
cat("ALU_alp in built Utuhina inflow:", "ALU_alp" %in% names(dU), "| max", signif(max(dU$ALU_alp), 3), "| inflow_vars:", grepl("ALU_alp", readLines(nml)[grep("^\\s*inflow_vars", readLines(nml))]), "\n")
pars <- readRDS(Sys.getenv("STR_PARS", "calibration/step2o/runs/step2_best.rds"))$best                         # STR_PARS: e.g. the stage q best member (its table already holds the scaled k_w, k_s: use STR_KSCALE=1)
pars$value[pars$name %in% c("aed_alum/k_w", "aed_alum/k_s")] <- pars$value[pars$name %in% c("aed_alum/k_w", "aed_alum/k_s")] * kscale
res <- aemetools::run_aeme_param(b$aeme, param = pars[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE); stopifnot(inherits(res, "Aeme"))
nml2 <- readLines(nml); cat("inflow_vars still has ALU_alp after the run:", any(grepl("ALU_alp", nml2[grep("^\\s*inflow_vars", nml2)])), "\n")
P <- csps_pair(res, vars = c("CHM_oxy", "PHS_tp", "PHS_frp", "PHY_tchla", "NIT_tn", "HYD_temp"))
pool <- tryCatch({ f <- AEME::get_model_outfile(res)$glm_aed[["output"]]; nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  z <- ncdf4::ncvar_get(nc, "z"); ns <- ncdf4::ncvar_get(nc, "NS"); tm <- ncdf4::ncvar_get(nc, "time"); t0 <- ncdf4::ncatt_get(nc, "time", "units")$value; orig <- as.Date(strsplit(t0, " ")[[1]][3]); dd <- if (grepl("hour", t0)) orig + floor(tm / 24) else orig + tm
  g <- function(v) { a <- ncdf4::ncvar_get(nc, v); vapply(seq_along(ns), function(i) { h <- z[seq_len(ns[i]), i]; mean(a[seq_len(ns[i]), i][h >= max(h) - 5], na.rm = TRUE) }, numeric(1)) }
  vn <- names(nc$var); P_VARS <- unique(c("ALU_ala", "ALU_alp", grep("^PHS_frp$|^PHS_frp_ads$|^OGM_dop$|^OGM_pop$|^OGM_dopr$|_IP$|^PHS_tp$", vn, value = TRUE)))
  out <- data.frame(Date = dd); for (v in intersect(P_VARS, vn)) out[[v]] <- g(v); out$ala <- out$ALU_ala; out$alp <- out$ALU_alp; out }, error = function(e) { cat("pool read failed:", conditionMessage(e), "\n"); NULL })
if (!is.null(pool)) cat("lake ALU_alp (top 5 m, mmol P/m3): mean", signif(mean(pool$alp, na.rm = TRUE), 3), " max", signif(max(pool$alp, na.rm = TRUE), 3), "\n")
saveRDS(list(name = vname, mU = mU, mP = mP, kscale = kscale, P = P, pool = pool, cal = step3_resid(P, al_sim_from_nc(res), DOSE_START, as.Date("2018-06-30")), val = step3_resid(P, al_sim_from_nc(res), as.Date("2018-07-01"), as.Date("2022-06-29"))), file.path(outdir, paste0("result_", vname, ".rds")))
unlink(file.path(outdir, paste0("run_", vname)), recursive = TRUE); cat("done", vname, "\n")
