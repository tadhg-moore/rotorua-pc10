# Choose between recipe A (raw chl-a targets) and B (log1p chl-a targets) from the fixA-vs-fixB
# comparison written by compare_ab_calibrations.R. Prints "A" or "B" (+ reason on stderr).
#   B wins when it lowers the log1p chl-a/cyano RMSE (mean over depth classes) AND at most 2
#   non-phytoplankton variable x depth groups have RMSE >10% worse than A. Otherwise A.
d <- Sys.getenv("CHOOSE_DIR", "pest_workflow_compare/fixA_vs_fixB")
fail <- function(why) { message("choose_recipe: ", why, " -> A (default)"); cat("A\n"); quit(status = 0) }
if (!file.exists(file.path(d, "ab_summary.csv"))) fail("no comparison output")
s <- read.csv(file.path(d, "ab_summary.csv")); l <- read.csv(file.path(d, "ab_logchl.csv"))
lc <- aggregate(log1p_rmse ~ recipe, l, mean)
lA <- lc$log1p_rmse[lc$recipe == "A"]; lB <- lc$log1p_rmse[lc$recipe == "B"]
worse <- sum(s$verdict == "B WORSE" & !s$variable %in% c("PHY_tchla", "PHY_cyano"), na.rm = TRUE)
message(sprintf("choose_recipe: log1p chl RMSE A=%.3f B=%.3f; non-phyto groups where B is worse: %d", lA, lB, worse))
cat(if (length(lA) && length(lB) && lB < lA && worse <= 2) "B\n" else "A\n")
