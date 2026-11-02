# Does P sorption do anything? Runs the 2b best member with ads_use_external_tss on (as built) and off (sorbs onto NCS_ss1),
# at three kpo4p values, over a short window, and compares the surface FRP / TP fit, the adsorbed-P share and the P budget.
#   Rscript calibration/step2/test_sorption.R       (SORP_START / SORP_STOP, default 1998-01-01 / 2003-12-31)
Sys.setenv(S2_STAGE = "b", S2_DIR = file.path("calibration", "step2", "sorption_test"))
source("calibration/step2/step2_common.R"); source("R/nutrient_budget_helpers.R")
SA_START <- Sys.getenv("SORP_START", "1998-01-01"); SA_STOP <- Sys.getenv("SORP_STOP", "2003-12-31")
out <- file.path("calibration", "step2", "sorption_test"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
b <- step2_build("sorp")
best <- readRDS("calibration/step2b/runs/step2_best.rds")$best
a0 <- tryCatch(AEME::set_glm_balance_groups(b$aeme, groups = c("TN", "TP", "TSS"), path = b$path), error = function(e) b$aeme)

run_case <- function(label, ext_tss, kpo4p) {
  a <- a0; cfg <- AEME::configuration(a)
  cfg[["glm_aed"]][["bgc"]][["aed"]][["aed_phosphorus"]][["ads_use_external_tss"]] <- ext_tss
  AEME::configuration(a) <- cfg
  AEME::write_configuration(aeme = a, model = MODEL, path = b$path, include_boundary = FALSE)   # run_aeme_param() only edits parameters in the nml on disk
  nml <- readLines(list.files(b$path, "^aed.nml$", recursive = TRUE, full.names = TRUE)[1]); cat(" ", grep("ads_use_external_tss", nml, value = TRUE), "
")
  p <- best; p$value[p$name == "aed_phosphorus/kpo4p"] <- kpo4p; p$min[p$name == "aed_phosphorus/kpo4p"] <- min(p$min[p$name == "aed_phosphorus/kpo4p"], kpo4p); p$max[p$name == "aed_phosphorus/kpo4p"] <- max(p$max[p$name == "aed_phosphorus/kpo4p"], kpo4p)
  res <- aemetools::run_aeme_param(a, param = p[, PCOLS], model = MODEL, path = b$path, return_aeme = TRUE)
  stopifnot(inherits(res, "Aeme"))
  P <- csps_pair(res, vars = c("PHS_tp", "PHS_frp", "PHY_tchla", "NIT_amm", "NIT_nit", "NIT_tn"))
  P$PHS_tp$sim <- P$PHS_tp$sim + DOPR_GM3; P$NIT_tn$sim <- P$NIT_tn$sim + DONR_GM3
  fit <- do.call(rbind, lapply(names(P), function(v) { d <- P[[v]]; d <- d[d$depth <= 5, ]
    data.frame(case = label, var = v, obs = mean(d$obs), sim = mean(d$sim), logbias = mean(log(d$sim + .003) - log(d$obs + .003)), r = suppressWarnings(cor(d$obs, d$sim))) }))
  bud <- nutrient_budget(res, path = b$path, from = as.Date(SA_START) + 550, to = SA_STOP)
  list(fit = fit, P = bud$P, adsorbed_share = bud$adsorbed_share)
}
cases <- list(list("A external TSS (as built)", TRUE, 11.4), list("B NCS_ss1, kpo4p 11.4", FALSE, 11.4), list("C NCS_ss1, kpo4p 0.5", FALSE, 0.5), list("D NCS_ss1, kpo4p 0.1", FALSE, 0.1))
R <- lapply(cases, function(k) { cat("running", k[[1]], "\n"); flush.console(); do.call(run_case, k) })
fit <- do.call(rbind, lapply(R, `[[`, "fit")); fit[, 3:6] <- lapply(fit[, 3:6], signif, 3)
cat("\nSurface (0-5 m) fit by case (logbias = mean log(sim/obs)):\n"); print(fit[fit$var %in% c("PHS_frp", "PHS_tp", "PHY_tchla"), ], row.names = FALSE)
cat("\nAdsorbed P as a share of FRP:", paste(sprintf("%s: %.3f", vapply(cases, `[[`, "", 1), vapply(R, `[[`, 1, "adsorbed_share")), collapse = " | "), "\n")
pb <- as.data.frame(do.call(cbind, lapply(R, function(r) round(r$P, 1)))); names(pb) <- c("A", "B", "C", "D"); pb$term <- names(R[[1]]$P)
cat("\nP budget, t P/yr:\n"); print(pb[, c("term", "A", "B", "C", "D")], row.names = FALSE)
write.csv(fit, file.path(out, "fit.csv"), row.names = FALSE); write.csv(pb, file.path(out, "p_budget.csv"), row.names = FALSE)
