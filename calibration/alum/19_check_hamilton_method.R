suppressMessages({library(dplyr); library(readr); library(AEME)})
w <- read_csv("data/raw/flows/rotorua_inflows_populated_withalum.csv", show_col_types = FALSE); n <- read_csv("data/raw/flows/rotorua_inflows_populated_noalum.csv", show_col_types = FALSE)
for (d in c("w", "n")) { x <- get(d); x$Date <- as.Date(x$Date); assign(d, x) }
# (1) Utuhina no-alum file: is DRP/TP a fixed monthly ratio from 2001-2006 (Hamilton 2015 p36)?
u <- n |> filter(Inflow == "Utuhina", Date >= "2001-01-01", Date <= "2012-12-31") |> mutate(mo = as.integer(format(Date, "%m")), yr = as.integer(format(Date, "%Y")), r = DRP / TP, pre = Date < "2006-06-01")
cat("Utuhina NO-ALUM file: DRP/TP by month, pre-alum (2001-06/2006) vs post (2006-12), medians and sd of daily ratio\n")
print(u |> group_by(mo, pre) |> summarise(med = round(median(r), 3), sd = round(sd(r), 3), .groups = "drop") |> tidyr::pivot_wider(names_from = pre, values_from = c(med, sd)) |> as.data.frame())
uw <- w |> filter(Inflow == "Utuhina", Date >= "2001-01-01", Date <= "2012-12-31") |> mutate(pre = Date < "2006-06-01", r = DRP / TP)
cat("\nUtuhina WITH-ALUM file: mean DRP/TP pre vs post:", round(mean(uw$r[uw$pre]), 3), round(mean(uw$r[!uw$pre]), 3), "\n")
# (2) Puarenga: with-alum DRP reduction vs Al dose (kg/d) and Al:DRP, compare to McIntosh: >75 kg Al/d -> >80% reduction; Al:DRP > 20 -> most DRP to particulate
inf <- AEME::inflows(readRDS("calibration/step3/aeme_base_2005_alum.rds"))$data$Puarenga; inf$Date <- as.Date(inf$Date)
p <- inner_join(w |> filter(Inflow == "Puarenga") |> select(Date, DRP_w = DRP, Q = VOL), n |> filter(Inflow == "Puarenga") |> select(Date, DRP_n = DRP), by = "Date") |> inner_join(inf[, c("Date", "ALU_ala")], by = "Date") |> filter(Date >= "2010-06-01", ALU_ala > 0, DRP_n > 0.005)
p$al_kgd <- p$ALU_ala * p$Q * 26.98 / 1e6; p$R <- p$ALU_ala / (p$DRP_n * 1000 / 30.974); p$f <- 1 - p$DRP_w / p$DRP_n
cat("\nPuarenga (with-alum file): DRP reduction by Al dose (kg Al/d)\n"); print(p |> mutate(bin = cut(al_kgd, c(0, 25, 50, 75, 100, 150, 400))) |> group_by(bin) |> summarise(n = n(), f = round(mean(f), 2), .groups = "drop") |> as.data.frame())
cat("model dose range kg Al/d, Puarenga: median", round(median(p$al_kgd)), " p90", round(quantile(p$al_kgd, .9)), "\n")
