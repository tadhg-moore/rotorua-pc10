# Box and arrow diagrams of the fate of alum (Al) and of the P it binds over the whole simulation (member 46, 2006 to 2022-06), in the style of the GLM nutrient budget figures.
# Reads the tables from calibration/alum/50_alum_fate.R. Arrows: mean flux per year (line width proportional); boxes: mean inventory.
#   Rscript calibration/alum/51_alum_fate_diagram.R
suppressMessages({library(ggplot2); library(readr); library(dplyr)})
d <- "website/data/alumfate"; out <- "website/images/alumfate"
ft <- read_csv(file.path(d, "fate_totals.csv"), show_col_types = FALSE); pl <- read_csv(file.path(d, "pools.csv"), show_col_types = FALSE)
g <- function(q) ft$t_per_yr[ft$quantity == q]; pm <- function(p) pl$mean_t[pl$pool == p]
fAl <- 26.98 / 30.97 # t Al per t P in the complex (capacity ratio 1)
col <- "#b0246b"; fill <- "#fbe3ef"; grey <- "#8a8a8a"; txt <- "#555555"
F <- list(applied = g("Al applied to the dosed streams"), s_cap = g("Al delivered to the lake as free capacity (ALU_ala)"), s_alp = g("Al delivered to the lake as bound P (ALU_alp)"),
  p_str = g("P bound in the stream step"), p_wc = g("P bound in the water column"), p_ben = g("P bound in settled alum (benthic uptake)"),
  age_w = g("Capacity lost to ageing, water column"), age_b = g("Capacity lost to ageing, benthic"), set_a = g("Settling flux of capacity (ala)"), set_p = g("Settling flux of bound P (alp)"),
  res_p = g("Resuspension of bound P"), res_a = g("Resuspension of capacity"), bur_p = g("Burial of bound P"), bur_al = g("Burial of capacity (Al total)"),
  out_p = g("Bound P out through the outlet"), out_a = g("Capacity out through the outlet"))
fmt <- function(x) ifelse(x < 10, sprintf("%.2f", x), sprintf("%.1f", x))
boxes <- function(b) geom_rect(data = b, aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = kind, colour = kind), linewidth = 0.5)
theme_d <- theme_void(base_size = 10) + theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 13), plot.subtitle = element_text(hjust = 0.5, colour = "#c0392b", size = 9), plot.caption = element_text(hjust = 0, colour = txt, size = 8), legend.position = "none", plot.margin = margin(8, 8, 8, 8))
pal <- scale_fill_manual(values = c(pool = fill, sink = "#eeeeee", frame = "#fff7fb")); pal2 <- scale_colour_manual(values = c(pool = col, sink = grey, frame = "#e8b9d0"))
mk <- function(boxes_df, arr, title, sub, cap, xl, yl) {
  mx <- max(arr$v); arr$lw <- 0.35 + 5.5 * arr$v / mx
  ggplot() + geom_rect(data = filter(boxes_df, kind == "frame"), aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = kind, colour = kind), linewidth = 0.4) +
    geom_rect(data = filter(boxes_df, kind != "frame"), aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = kind, colour = kind), linewidth = 0.6) +
    geom_segment(data = arr, aes(x = x, y = y, xend = xe, yend = ye, linewidth = lw), colour = col, lineend = "butt", arrow = arrow(length = unit(0.16, "inches"), type = "closed")) +
    scale_linewidth_identity() + geom_text(data = arr, aes(x = lx, y = ly, label = lab), colour = txt, size = 3, lineheight = 0.9) +
    geom_text(data = filter(boxes_df, kind != "frame"), aes(x = (x0 + x1) / 2, y = (y0 + y1) / 2, label = lab), colour = ifelse(filter(boxes_df, kind != "frame")$kind == "pool", col, "#444444"), fontface = "bold", size = 3.1, lineheight = 0.95) +
    geom_text(data = filter(boxes_df, kind == "frame"), aes(x = x0 + 0.8, y = y0 + 1.6, label = lab), hjust = 0, colour = txt, size = 3) +
    pal + pal2 + coord_cartesian(xlim = xl, ylim = yl, expand = FALSE) + labs(title = title, subtitle = sub, caption = paste(strwrap(cap, 125), collapse = "
")) + theme_d }
A <- function(x, y, xe, ye, v, lx, ly, lab) data.frame(x = x, y = y, xe = xe, ye = ye, v = v, lx = lx, ly = ly, lab = lab)
# ---- Al ----
al_ap <- F$p_str * 0 + 0 # placeholder to keep names explicit
alp_al <- function(p) p * fAl
bx <- bind_rows(
  data.frame(x0 = 0, x1 = 14, y0 = 20, y1 = 38, kind = "sink", lab = "Dosed streams\nPuarenga + Utuhina"),
  data.frame(x0 = 23, x1 = 49, y0 = 6, y1 = 52, kind = "frame", lab = "Water column"),
  data.frame(x0 = 27, x1 = 45, y0 = 33, y1 = 45, kind = "pool", lab = sprintf("Free capacity\n%s t Al", fmt(pm("Water column capacity (ala)")))),
  data.frame(x0 = 27, x1 = 45, y0 = 12, y1 = 24, kind = "pool", lab = sprintf("Al bound to P\n%s t Al", fmt(alp_al(pm("Water column bound P (alp)"))))),
  data.frame(x0 = 59, x1 = 85, y0 = 6, y1 = 52, kind = "frame", lab = "Settled alum (benthic)"),
  data.frame(x0 = 63, x1 = 81, y0 = 33, y1 = 45, kind = "pool", lab = sprintf("Settled free capacity\n%s t Al", fmt(pm("Settled capacity (ben_ala)")))),
  data.frame(x0 = 63, x1 = 81, y0 = 12, y1 = 24, kind = "pool", lab = sprintf("Settled Al bound to P\n%s t Al", fmt(alp_al(pm("Settled bound P (ben_alp)"))))),
  data.frame(x0 = 24, x1 = 82, y0 = 58, y1 = 66, kind = "sink", lab = "Aged: capacity lost (Al hydroxide no longer binds P)"),
  data.frame(x0 = 90, x1 = 104, y0 = 12, y1 = 45, kind = "sink", lab = "Buried\nin the sediment"),
  data.frame(x0 = 27, x1 = 45, y0 = -8, y1 = -1, kind = "sink", lab = "Outlet"))
ar <- bind_rows(
  A(14, 39 - 4, 27, 39, F$s_cap, 20.5, 40.5, fmt(F$s_cap)), A(14, 25, 27, 18, F$s_alp, 20.5, 19, fmt(F$s_alp)),
  A(36, 33, 36, 24, F$p_wc * fAl, 40.5, 28.5, paste0("Binding\n", fmt(F$p_wc * fAl))),
  A(45, 41, 63, 41, F$set_a, 54, 43.2, paste0("Settling\n", fmt(F$set_a))), A(45, 18, 63, 18, alp_al(F$set_p), 54, 20.2, paste0("Settling\n", fmt(alp_al(F$set_p)))),
  A(63, 36, 45, 36, F$res_a, 54, 34.2, fmt(F$res_a)), A(63, 14.5, 45, 14.5, alp_al(F$res_p), 54, 12.8, paste0("Resuspension\n", fmt(alp_al(F$res_p)))),
  A(31, 45, 31, 58, F$age_w, 26.3, 54, paste0("Ageing\n", fmt(F$age_w))), A(72, 45, 72, 58, F$age_b, 67.8, 54, paste0("Ageing\n", fmt(F$age_b))),
  A(72, 33, 72, 24, F$p_ben * fAl, 77.5, 28.5, paste0("Binding\n", fmt(F$p_ben * fAl))),
  A(81, 38, 90, 38, F$bur_al - alp_al(F$bur_p), 85.5, 40.5, fmt(F$bur_al - alp_al(F$bur_p))), A(81, 18, 90, 18, alp_al(F$bur_p), 85.5, 20.5, fmt(alp_al(F$bur_p))),
  A(36, 6, 36, -1, F$out_a + alp_al(F$out_p), 41, 2.5, fmt(F$out_a + alp_al(F$out_p))))
p1 <- mk(bx, ar, "Aluminium (t Al/yr)", "Reconstructed from the pools and module rates: water column outflows exceed inflows, the budget does not close",
  "Arrows: mean flux, t Al per year, 2006 to 2022-06 (width proportional). Boxes: mean inventory, t Al. Fluxes other than uptake are rebuilt from the saved pools and the rates in aed_alum.F90.", c(-2, 106), c(-10, 68))
ggsave(file.path(out, "fate_diagram_Al.png"), p1, width = 9.5, height = 6.8, dpi = 140, bg = "white")
# ---- bound P ----
bx2 <- bind_rows(
  data.frame(x0 = 0, x1 = 15, y0 = 40, y1 = 54, kind = "sink", lab = "Dosed streams"),
  data.frame(x0 = 0, x1 = 15, y0 = 8, y1 = 22, kind = "sink", lab = "Free alum capacity\n(water column)"),
  data.frame(x0 = 22, x1 = 46, y0 = 4, y1 = 58, kind = "frame", lab = "Water column"),
  data.frame(x0 = 27, x1 = 43, y0 = 26, y1 = 38, kind = "pool", lab = sprintf("P bound to alum\n%s t P", fmt(pm("Water column bound P (alp)")))),
  data.frame(x0 = 56, x1 = 84, y0 = 4, y1 = 58, kind = "frame", lab = "Settled alum (benthic)"),
  data.frame(x0 = 61, x1 = 79, y0 = 26, y1 = 38, kind = "pool", lab = sprintf("Settled P bound to alum\n%s t P", fmt(pm("Settled bound P (ben_alp)")))),
  data.frame(x0 = 90, x1 = 104, y0 = 22, y1 = 42, kind = "sink", lab = "Buried\nin the sediment"),
  data.frame(x0 = 27, x1 = 43, y0 = -8, y1 = -1, kind = "sink", lab = "Outlet"),
  data.frame(x0 = 61, x1 = 79, y0 = 8, y1 = 15, kind = "sink", lab = "Settled free capacity"))
ar2 <- bind_rows(
  A(15, 47, 27, 36, F$p_str, 24, 47.5, paste0("Stream step\n", fmt(F$p_str))),
  A(15, 15, 27, 28, F$p_wc, 17.5, 24.5, paste0("Binding\n", fmt(F$p_wc))),
  A(43, 34, 61, 34, F$set_p, 52, 37, paste0("Settling\n", fmt(F$set_p))),
  A(61, 29, 43, 29, F$res_p, 52, 25.5, paste0("Resuspension\n", fmt(F$res_p))),
  A(70, 15, 70, 26, F$p_ben, 75.5, 20.5, paste0("Binding\n", fmt(F$p_ben))),
  A(79, 32, 90, 32, F$bur_p, 84.5, 35, paste0("Burial\n", fmt(F$bur_p))),
  A(35, 26, 35, -1, F$out_p, 40, 6, paste0("Outlet\n", fmt(F$out_p))))
miss <- (F$set_p + F$out_p) - (F$p_str + F$p_wc + F$res_p)
p2 <- mk(bx2, ar2, "Phosphorus bound by alum (t P/yr)", sprintf("Water column outflows exceed inflows by %.1f t P/yr: the reconstruction does not close", miss),
  "Arrows: mean flux, t P per year, 2006 to 2022-06 (width proportional). Boxes: mean inventory, t P. Binding fluxes are model diagnostics; settling, resuspension and burial are rebuilt from the pools and rates.", c(-2, 106), c(-10, 62))
ggsave(file.path(out, "fate_diagram_P.png"), p2, width = 9.5, height = 6.2, dpi = 140, bg = "white")
cat("water column P: in", round(F$p_str + F$p_wc + F$res_p, 2), "out", round(F$set_p + F$out_p, 2), "\n")
cat("sediment P: in", round(F$set_p + F$p_ben, 2), "out", round(F$bur_p + F$res_p, 2), "\n")
cat("sediment Al: in", round(F$set_a + alp_al(F$set_p), 2), "out", round(F$age_b + F$bur_al + F$res_a + alp_al(F$res_p), 2), "\n")
