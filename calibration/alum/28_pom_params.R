p <- readRDS("calibration/step2o/runs/step2_best.rds")$best
i <- grepl("aed_organic_matter|aed_phosphorus", p$name) & grepl("settl|hydrol|w_pom|w_poc|rpop|rpoc|minerl|Kpo4|ads|ksed|theta", p$name, ignore.case = TRUE)
print(p[i, intersect(c("name", "value", "min", "max", "scale", "theme"), names(p))], row.names = FALSE)
