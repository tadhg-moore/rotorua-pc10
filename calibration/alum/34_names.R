t <- readRDS("calibration/step2o3/runs/step2_table.rds")
v <- t[grepl("nit|amm|denit|_N$|K_N|rpon|rdon|don", t$name, ignore.case = TRUE) | t$name %in% "K_N", c("name", "group", "index", "value")]
print(v, row.names = FALSE)
