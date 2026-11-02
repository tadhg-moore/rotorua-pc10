# Diagnose the total-N shortfall: inflow N concentrations/loads vs lake pools in the step 2 baseline run.
Sys.setenv(SA_VARIANT = "noalum", SA_BASE = "calibration/step2/aeme_base_1990_v2.rds", SA_START = "1990-01-01",
           SA_STOP = "2006-05-31", SA_SPINUP = "540", SA_DIR_OVERRIDE = file.path("calibration", "step2", "runs"))
source("calibration/sa/sa_common.R")
suppressPackageStartupMessages(library(dplyr))
# ---- inflows (flow-weighted concentrations, all streams, 1990-2005) ----
inf <- targets::tar_read(rotorua_inflow_list_matrix)[[1]]; inf$Rainfall <- NULL
I <- bind_rows(inf, .id = "stream") |> filter(Date >= as.Date("1990-01-01"), Date <= as.Date("2005-12-31"))
cols <- c("NIT_amm", "NIT_nit", "NIT_don", "NIT_pon", "PHS_frp", "PHS_dop", "PHS_pop", "PHS_pip", "CAR_doc", "CAR_poc")
fw <- sapply(cols, function(v) sum(I[[v]] * I$HYD_flow, na.rm = TRUE) / sum(I$HYD_flow))
cat("\nflow-weighted inflow concentrations (as stored):\n"); print(signif(fw, 3))
cat("total inflow volume m3/yr:", signif(sum(I$HYD_flow) / 16, 3), "\n")
cat("stream flow-weighted TN-like sum (amm+nit+don+pon):\n")
print(I |> group_by(stream) |> summarise(vol_m3_yr = sum(HYD_flow) / 16, N = sum((NIT_amm + NIT_nit + NIT_don + NIT_pon) * HYD_flow) / sum(HYD_flow), P = sum((PHS_frp + PHS_dop + PHS_pop) * HYD_flow) / sum(HYD_flow)) |> arrange(desc(vol_m3_yr)) |> as.data.frame(), digits = 3)
# ---- lake pools in the baseline run ----
b <- sa_build("sa_base"); best <- readRDS("calibration/checkpoints/step1_final_best.rds")$best
res <- aemetools::run_aeme_param(b$aeme, param = best[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
saveRDS(res, file.path(SA_DIR, "step2_baseline_aeme_v2.rds"))
vs <- c("NIT_amm", "NIT_nit", "NIT_don", "NIT_pon", "NIT_tn", "PHS_frp", "PHS_dop", "PHS_pop", "PHS_tp", "PHY_tchla")
cat("\nmodelled lake pools, surface mean by year (g/m3 or ug/L):\n")
out <- lapply(vs, function(v) { d <- tryCatch(AEME::get_var(res, model = MMODEL <- "glm_aed", var_sim = v, use_obs = FALSE, remove_spin_up = TRUE), error = function(e) NULL)
  if (is.null(d)) return(NULL); d$yr <- as.integer(format(as.Date(d$Date), "%Y")); d <- d[d$depth <= 5, ]
  r <- aggregate(sim ~ yr, d, mean); names(r)[2] <- v; r })
out <- Filter(Negate(is.null), out); A <- Reduce(function(x, y) merge(x, y, by = "yr", all = TRUE), out)
print(round(A, 4), row.names = FALSE)
cat("\navailable sim vars matching N/OGM:\n"); print(grep("^(NIT|OGM|PHY).*", AEME::get_var_names %||% character(), value = TRUE))
