suppressPackageStartupMessages(library(AEME))
inf <- AEME::inflows(readRDS("calibration/alum/stream/base_Q50.rds"))$data
for (s in c("Puarenga", "Utuhina")) { d <- inf[[s]]; d$Date <- as.Date(d$Date); k <- d$ALU_ala > 0 | d$ALU_alp > 0; cat(s, "first dose", format(min(d$Date[k], na.rm = TRUE)), "| dosed days by year:", paste(names(table(format(d$Date[k], "%Y"))), table(format(d$Date[k], "%Y")), collapse = " "), "\n") }
m <- readRDS(list.files("calibration/historical/out_j50", "^member_.*[.]rds$", full.names = TRUE)[1]); str(m$P$PHS_tp); print(range(m$P$PHS_tp$Date)); print(table(format(m$P$PHS_tp$Date, "%Y")))
