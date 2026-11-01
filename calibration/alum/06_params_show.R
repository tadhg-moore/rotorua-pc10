p <- readRDS("calibration/step2o/runs/step2_best.rds")$best
print(p[grepl("^aed_alum/", p$name), c("name", "value", "min", "max")])
m <- readRDS("calibration/posterior/alum_members.rds")
a <- do.call(rbind, lapply(seq_along(m), function(i) { x <- m[[i]]; x <- x[grepl("^aed_alum/", x$name), ]; setNames(x$value, sub("aed_alum/", "", x$name)) }))
print(round(apply(a, 2, quantile, c(0, .5, 1)), 4))
