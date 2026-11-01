# Correct the inflow suspended-solids units bug in an Aeme object.
#
# R/lspc_to_aeme.R writes NCS_tss / NCS_ss1 as the flow-weighted mean of LSPC's
# `ssedout` column and labels it mg/L. But the processed `ssedout` is ALREADY
# converted to t/m3 (data/processed/lspc_output_key.csv: SSEDOUT mg/L,
# conversion 1e-6 -> t/m3), so the inflow solids are 1e6 too small (e.g.
# 5.5e-6 instead of 5.5 mg/L for Rototoa's inflow).
#
# The cached targets (aeme_baseline_upstream / _downstream) carry the bad values
# and set_lake_inflows() recreates them live for scenarios, so the correction is
# applied in ONE guarded place on any Aeme object: an inflow is multiplied by
# 1e6 only if its flow-weighted solids are positive but < `threshold` mg/L
# (impossible for a real stream), so already-correct objects are left alone and
# applying it twice is harmless. Set env PEST_NO_SS_FIX=1 to disable (used only
# for control experiments).
#
# TODO: fix R/lspc_to_aeme.R at the source and rebuild the cached targets; this
# guard then becomes a no-op.

fix_inflow_ss_units <- function(aeme, threshold = 1e-3, verbose = TRUE) {
  if (nzchar(Sys.getenv("PEST_NO_SS_FIX"))) return(aeme)
  inf <- AEME::get_inflows(aeme)
  changed <- character()
  for (nm in names(inf)) {
    x <- inf[[nm]]
    cols <- intersect(c("NCS_tss", "NCS_ss1"), names(x))
    if (!length(cols) || !"HYD_flow" %in% names(x)) next
    v <- x[[cols[1]]]; ok <- !is.na(v) & !is.na(x$HYD_flow)
    if (!any(ok) || sum(x$HYD_flow[ok]) <= 0) next
    fw <- sum(v[ok] * x$HYD_flow[ok]) / sum(x$HYD_flow[ok])
    if (fw > 0 && fw < threshold) {
      for (cc in cols) x[[cc]] <- x[[cc]] * 1e6
      inf[[nm]] <- x
      changed <- c(changed, sprintf("%s (%.2g -> %.2f mg/L)", nm, fw, fw * 1e6))
    }
  }
  if (length(changed)) {
    if (verbose) message("  fix_inflow_ss_units: x1e6 applied to ", paste(changed, collapse = "; "))
    aeme <- AEME::set_inflows(aeme, inflows = inf)
  }
  aeme
}
