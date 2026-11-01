t <- readRDS(commandArgs(TRUE)[1]); v <- t[t$max > t$min, c("name", "group", "index", "value", "min", "max")]; print(v, row.names = FALSE); cat(nrow(v), "free\n")
