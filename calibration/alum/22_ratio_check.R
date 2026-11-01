suppressMessages({library(dplyr); library(readr)})
rd <- function(f) { x <- read_csv(file.path("data/raw/flows", f), show_col_types = FALSE); x$Date <- as.Date(x$Date); x$mo <- as.integer(format(x$Date, "%m")); x }
w <- rd("rotorua_inflows_populated_withalum.csv") |> filter(Inflow == "Utuhina"); n <- rd("rotorua_inflows_populated_noalum.csv") |> filter(Inflow == "Utuhina")
S <- function(d, lab) d |> filter(Date >= "2001-01-01", Date <= "2012-12-31") |> mutate(period = ifelse(Date < "2006-06-01", "PRE-alum 2001-mid2006", "POST-alum mid2006-2012"), r = DRP / TP) |> group_by(period) |> summarise(file = lab, mean_ratio = round(mean(r), 3), median_ratio = round(median(r), 3), .groups = "drop")
print(as.data.frame(bind_rows(S(n, "no-alum file"), S(w, "with-alum file"))))
pre <- n |> filter(Date >= "2001-01-01", Date < "2006-06-01") |> mutate(r = DRP / TP) |> group_by(mo) |> summarise(pre_ratio = mean(r))
post <- n |> filter(Date >= "2006-06-01", Date <= "2012-12-31") |> left_join(pre, by = "mo") |> mutate(hamilton_DRP = pre_ratio * TP)
cat("\nPost-2006 untreated DRP: no-alum file mean", round(mean(post$DRP), 4), " vs Hamilton method (pre-alum monthly DRP:TP x TP)", round(mean(post$hamilton_DRP), 4), " ratio file/Hamilton", round(mean(post$DRP) / mean(post$hamilton_DRP), 3), "\n")
ww <- w |> filter(Date >= "2006-06-01", Date <= "2012-12-31"); cat("measured (with-alum) DRP mean", round(mean(ww$DRP), 4), " => removal vs file:", round(1 - mean(ww$DRP) / mean(post$DRP), 3), " vs Hamilton:", round(1 - mean(ww$DRP) / mean(post$hamilton_DRP), 3), "\n")
