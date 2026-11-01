b <- readRDS("calibration/step2j/runs/step2_best.rds")
cat(sprintf("cal phi start %.3f best %.3f | val phi start %.3f best %.3f\n", mean(b$baseline_Ycal^2), mean(b$best_Ycal^2), mean(b$baseline_Yval^2), mean(b$best_Yval^2)))
x <- readRDS("calibration/step2j/runs/step2_ies.rds"); for (i in seq_along(x$hist)) { p <- x$hist[[i]]$phi; cat(i - 1, round(min(p, na.rm = TRUE), 3), round(median(p, na.rm = TRUE), 3), "\n") }
m <- readRDS("website/data/calibration/joint_shrinkage.csv" |> (\(f) NULL)())
