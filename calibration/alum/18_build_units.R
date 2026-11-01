Sys.setenv(S2_STAGE = "o", S2_DIR = file.path("calibration", "alum", "units_runs"), S2_ALUM = "1", S2_BASE = "calibration/step3/aeme_base_2005_alum.rds",
           S2_ALUM_FROM = "calibration/step2alum/runs/step2_best.rds", S2_START = "2012-01-01", S2_STOP = "2012-12-31")
source("calibration/step2/step2_common.R")
b <- step2_build("units")
f <- list.files(b$path, "^inflow_Utuhina[.]csv$", recursive = TRUE, full.names = TRUE)[1]; d <- read.csv(f, check.names = FALSE)
cat("cols:", paste(names(d), collapse = " "), "\n"); print(d[d[[1]] == "2012-03-10", ])
src <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data$Utuhina; print(src[as.Date(src$Date) == as.Date("2012-03-10"), c("PHS_frp", "PHS_dop", "PHS_pop", "PHS_pip", "NIT_nit", "CAR_doc", "ALU_ala", "ALU_alp")])
