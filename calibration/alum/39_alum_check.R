b <- readRDS("calibration/step2j/runs/step2_best.rds")$best
a <- b[grepl("^aed_alum/", b$name), c("name", "group", "index", "value")]; print(a)
o <- readRDS("calibration/step2o3/runs/step2_best.rds")$best
cat("alum values identical to o3 best:", isTRUE(all.equal(a$value, o$value[match(paste(a$name, a$group, a$index), paste(o$name, o$group, o$index))])), "\n")
