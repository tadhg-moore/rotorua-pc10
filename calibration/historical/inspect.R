f <- list.files("calibration/historical/out_o3", "^member_.*[.]rds$", full.names = TRUE); cat(length(f), "members\n")
m <- readRDS(f[1]); str(m, max.level = 1); print(names(m$P)); print(head(m$P$PHS_tp, 3)); print(names(m$daily)); print(range(m$daily$Date))
