# Smoke test: build, show the free parameters, evaluate the start values once and print the residuals.
source("calibration/step2/step2_common.R")
b <- step2_build("sa_base"); saveRDS(b$aeme, file.path(SA_DIR, "sa_base_aeme.rds"))
tab <- step2_table(b); saveRDS(tab, file.path(SA_DIR, "step2_table.rds"))
vary <- which(tab$min < tab$max); cat(length(vary), "free parameters\n")
print(tab[vary, c("name", "group", "index", "value", "min", "max", "scale")], row.names = FALSE)
cat("missing from table:", paste(.key2(STEP2_PARAMS)[!.key2(STEP2_PARAMS) %in% .key2(tab)], collapse = "; "), "\n")
t0 <- Sys.time(); r <- step2_eval(b$aeme, b$path, tab); cat("run time", round(difftime(Sys.time(), t0, units = "secs")), "s\n")
if (is.null(r)) stop("evaluation failed")
cat(sprintf("phi cal %.2f  val %.2f\n", mean(r$cal^2), mean(r$val^2))); print(round(cbind(cal = r$cal, val = r$val), 2))

