suppressMessages({library(dplyr); library(readr)})
rd <- function(f) { x <- read_csv(file.path("data/raw/flows", f), show_col_types = FALSE); x$Date <- as.Date(x$Date); x }
w <- rd("rotorua_inflows_populated_withalum.csv"); n <- rd("rotorua_inflows_populated_noalum.csv")
for (s in c("Puarenga", "Utuhina")) {
  j <- inner_join(w |> filter(Inflow == s), n |> filter(Inflow == s), by = c("Date", "InfNum"), suffix = c("_w", "_n")) |> filter(Date >= "2011-01-01", Date <= "2017-12-31")
  cat("\n", s, "mean g/m3 2011-17 (with | no):\n")
  for (v in c("DRP", "DOPL", "POPL", "PIP", "TP", "TSS", "SSOL1", "SSOL2")) cat(sprintf("  %-6s %.4f | %.4f\n", v, mean(j[[paste0(v, "_w")]]), mean(j[[paste0(v, "_n")]])))
  cat("  max |TP - (DRP+DOPL+POPL+PIP)| with file:", signif(max(abs(j$TP_w - (j$DRP_w + j$DOPL_w + j$POPL_w + j$PIP_w))), 3), " no file:", signif(max(abs(j$TP_n - (j$DRP_n + j$DOPL_n + j$POPL_n + j$PIP_n))), 3), "\n")
  cat("  TP - sum(forms) mean: with", signif(mean(j$TP_w - (j$DRP_w + j$DOPL_w + j$POPL_w + j$PIP_w)), 3), " no", signif(mean(j$TP_n - (j$DRP_n + j$DOPL_n + j$POPL_n + j$PIP_n)), 3), "\n")
}
