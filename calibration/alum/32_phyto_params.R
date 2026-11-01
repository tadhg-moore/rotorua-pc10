t <- readRDS("calibration/step2p4/runs/step2_table.rds")
print(t[, intersect(c("name", "group", "index", "value", "min", "max", "theme"), names(t))][!grepl("^aed_alum|^MET|^light|^Kw", t$name) & (t$name %in% c("R_growth", "R_resp", "I_K", "K_N", "K_P", "w_p", "theta_growth", "X_pmin", "X_pmax", "k_p", "Xpmin", "Xpmax", "KePHY") | grepl("phyto|dop|DOP|po4|pom|sed_frp|fsed", t$name)), ], row.names = FALSE)
cat("\nall names:\n"); print(unique(t$name))
