suppressMessages(library(AEME)); aeme <- readRDS("calibration/historical/out_budget_m46/aeme_46.rds")
mb <- tryCatch(AEME::read_glm_mass_balance(aeme = aeme), error = function(e) conditionMessage(e)); if (is.character(mb)) cat("mass balance:", mb, "\n") else { cat("mass balance rows", nrow(mb), "vars:", paste(unique(mb$var), collapse = ", "), "\n"); print(range(mb$datetime)) }
x <- tryCatch(AEME::aeme_to_inflow(aeme), error = function(e) conditionMessage(e)); if (is.character(x)) cat("aeme_to_inflow:", x, "\n") else { str(x); print(summary(x[, c("HYD_flow", "NIT_tn", "PHS_tp", "PHS_frp", "NIT_nit", "NIT_amm", "HYD_temp")])) }
saveRDS(x, "calibration/historical/out_budget_m46/outflow_inflow_df.rds")
