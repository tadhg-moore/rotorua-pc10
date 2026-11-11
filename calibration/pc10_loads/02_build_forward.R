# Build a stand-alone forward run of the reference member (calibration/all_stage/out_main/best.rds, i.e. calibration/all_stage/best_glm_aed)
# on the old or the 2026 PC10 inflows, to compare lake TP / FRP / chl / TN / oxygen / level.
#   Rscript calibration/pc10_loads/02_build_forward.R <old|new|newsil> [sil=old|new] [set=noalum_storm_central]
#
# old : unchanged copy of best_glm_aed (inflows of the calibrated base, 1976-01-01 to 2022-06-29).
# new : same folder with the 13 inflow files replaced by the 2026 loads (GLM-native units: mmol/m3, m3/s, no conversion), 1976-01-01 to 2025-12-31
#       (the end of the ERA5 forcing, bcs/meteo_glm.csv).
#   - Alum (module on): ALU_ala is the dose from the dosing records divided by the NEW flow (calc_alum_inflow, as the old base: it reproduces the old
#     ALU_ala + ALU_alp to 1e-5), then the in-stream reaction of calibration/alum/stream_variant.R is applied to the new FRP:
#     Utuhina f = 0.45, Puarenga f = 0.9 min(1, R/23), R = ALU_ala / PHS_frp (mol/mol); bound P goes to ALU_alp and ALU_ala is reduced by the same amount.
#     Use a noalum_* set with this (the model applies alum itself).
#   - Silica: sil = old keeps the old constant (SIL_rsi 200 mmol/m3) so that the comparison is about N, P, C, solids and flow only; sil = new uses the
#     stream-specific silica of the new files (1000+ mmol/m3), which can change diatom growth and so confounds the comparison.
#   - PHY_cyano / PHY_diatom / PHY_green are 0.1 as in the old files.
#   - Outflow: bcs/outflow_wbal.csv is a constant 6.0588 m3/s after the spin-up (the rest of the outflow is the calibrated weir overflow), so it is
#     extended at that value to 2025-12-31. The new Ohau series is not used; lake level is the check on the water balance.
# Writes calibration/pc10_loads/fwd_<tag>/ (a copy of best_glm_aed with output/ empty). Run it with 03_run_forward.ps1.
args <- commandArgs(TRUE); tag <- args[1]; sil <- if (length(args) >= 2) args[2] else "old"; set <- if (length(args) >= 3) args[3] else "noalum_storm_central"
if (tag == "newsil") sil <- "new"                                                   # tag newsil = new inflows with the new stream silica
stopifnot(tag %in% c("old", "new", "newsil"), sil %in% c("old", "new"))
suppressPackageStartupMessages(library(dplyr))
source("R/read_alum_dosing.R"); source("R/calc_alum_inflow.R"); source("R/read_pc10_loads.R")
src <- "calibration/all_stage/best_glm_aed"; dst <- file.path("calibration", "pc10_loads", paste0("fwd_", tag))
if (dir.exists(dst)) unlink(dst, recursive = TRUE)
dir.create(dst, recursive = TRUE)
for (f in setdiff(list.files(src, all.files = FALSE), "output")) file.copy(file.path(src, f), dst, recursive = TRUE)
dir.create(file.path(dst, "output"), showWarnings = FALSE)

if (tag %in% c("new", "newsil")) {
  STOP <- as.Date("2025-12-31")
  root <- file.path("data", "raw", "pc10_loads_2026", set)
  key <- pc10_inflow_key(); key <- key[key$name != "Rainfall", ]
  dos <- suppressMessages(read_alum_dosing("data/raw/alum_dosing/Rotorua alum dose data February 2026.xlsx")); dos$time <- as.Date(dos$Date)
  dosecol <- c(Utuhina = "utuhina_alum_l_per_day", Puarenga = "puarenga_alum_l_per_day")
  rule <- list(Utuhina = function(R) rep(0.45, length(R)), Puarenga = function(R) 0.9 * pmin(1, R / 23))
  for (i in seq_len(nrow(key))) {
    nm <- key$name[i]; d <- read.csv(file.path(root, key$file[i])); d$time <- as.Date(d$time); d <- d[d$time <= STOP, ]
    o <- read.csv(file.path(src, "bcs", paste0("inflow_", nm, ".csv")), nrows = 2)        # old file: column set and silica constant
    out <- data.frame(time = format(d$time), flow = d$FLOW, ALU_ala = 0, OGM_doc = d$OGM_doc, OGM_poc = d$OGM_poc, OXY_oxy = d$OXY_oxy, salt = d$SALT,
                      temp = d$TEMP, NCS_ss1 = d$NCS_ss1, NCS_ss2 = d$NCS_ss2, NIT_amm = d$NIT_amm, OGM_don = d$OGM_don, NIT_nit = d$NIT_nit,
                      OGM_pon = d$OGM_pon, OGM_dop = d$OGM_dop, PHS_frp = d$PHS_frp, PHS_frp_ads = d$PHS_frp_ads, OGM_pop = d$OGM_pop,
                      PHY_cyano = 0.1, PHY_diatom = 0.1, PHY_green = 0.1, SIL_rsi = if (sil == "old") o$SIL_rsi[1] else d$SIL_rsi, ALU_alp = 0)
    if (nm %in% names(dosecol)) {
      L <- dos[[dosecol[[nm]]]][match(d$time, dos$time)]; L[is.na(L) | L < 0] <- 0
      dose <- ifelse(out$flow > 0, calc_alum_inflow(L, out$flow), 0)                        # mmol P capacity per m3 of inflow
      R <- ifelse(out$PHS_frp > 0, dose / out$PHS_frp, 0); f <- rule[[nm]](R); f[dose <= 0 | out$PHS_frp <= 0] <- 0
      B <- pmin(f * out$PHS_frp, dose); f <- ifelse(out$PHS_frp > 0, B / out$PHS_frp, 0)
      out$PHS_frp <- out$PHS_frp * (1 - f); out$ALU_ala <- dose - B; out$ALU_alp <- B
      z <- d$time >= "2011-01-01" & d$time <= "2017-12-31"
      cat(nm, ": mean f on 2011-17 dosed days", round(mean(f[z & dose > 0]), 2), "| bound P load 2011-17 (kg P/yr)", round(sum(B[z] * out$flow[z] * 86400) * 30.974 / 1e6 / 7), "\n")
    }
    stopifnot(!anyNA(out), all(is.finite(as.matrix(out[, -1]))))
    write.csv(out, file.path(dst, "bcs", paste0("inflow_", nm, ".csv")), row.names = FALSE, quote = FALSE)
  }
  ow <- read.csv(file.path(dst, "bcs", "outflow_wbal.csv")); ow$time <- as.Date(ow$time); last <- tail(ow, 1)
  ext <- data.frame(time = seq(last$time + 1, STOP, by = "day"), flow = last$flow); ow <- rbind(ow, ext); ow$time <- format(ow$time)
  write.csv(ow, file.path(dst, "bcs", "outflow_wbal.csv"), row.names = FALSE, quote = FALSE)
  nml <- file.path(dst, "glm4.nml"); x <- readLines(nml); i <- grep("^\\s*stop\\s*=", x)[1]; x[i] <- "   stop = '2025-12-31 00:00:00'"; writeLines(x, nml)
  cat("new run: stop", format(STOP), "| silica", sil, "| set", set, "| outflow rows", nrow(ow), "\n")
}
writeLines(c("@echo off", "set EXE=%~dp0..\\..\\..\\exe\\alum-diag-aed-v3.0.1", "set PATH=%EXE%;%PATH%", "cd /d %~dp0", "\"%EXE%\\glm.exe\" glm4.nml"), file.path(dst, "run.bat"))
cat("built", dst, "\n")
