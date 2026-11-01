# Note: sensitivity of P export downstream of the lake (not yet done)

Written 2026-11-01. Status: not started, waiting for a decision.

## Question
P exported downstream of the lake (outflow volume x outlet TP) is one of the main target variables. Can the Morris screen (`calibration/sa/sa_latest.R`, 30 trajectories, 142 parameters, results in `calibration/sa_latest/results`) say what controls it?

## Answer so far: only through proxies
- The runs did not save outflow volume or outflow TP, so export was never computed. `sa_latest_eval.R` saves surface TP summaries (`x_tp_mean`, `x_tp_summer`, `x_tp_annual_sd`, `x_tp_wet_minus_dry`, `x_tp_trend`, early and late means) and the calibration residuals, nothing on the outflow.
- Surface TP (top 5 m) should track outlet TP because the outflow is a surface weir. Drivers of mean TP:
  - cyanobacteria `K_P`, `w_p`, `R_resp`, `KePHY`
  - `rpop_hydrol`
  - wind speed (`MET_wndspd`), then `cd`
- Drivers of year-to-year TP: cyanobacteria `w_p`, `X_pmin`, `K_P`, `cd`, `rpop_hydrol`; zone 3 `fsed_frp` is weaker. Wet minus dry TP: `rpop_hydrol`.
- Sediment P release (`fsed_frp`, `ksed_frp`) barely moves the mean TP.
- The weir parameters `crest_elev` and `crest_factor` have almost no effect on surface TP (scaled mu* 0.006 and 0.0003), but they change outflow volume, so they may matter for the load.

## Why the proxies are not enough
- Export is concentration times flow. Flow depends mainly on the inflows, and inflow and flood scaling were not in the design.
- The bootstrap intervals for mean TP reach zero for every parameter (collapse runs), so even the proxy ranking is unreliable.
- Surface TP is not outlet TP, and the screen has no seasonal timing of the load.

## Proposed rerun (needs go-ahead)
- Add to `sa_latest_eval.R`: the outflow from `AEME::aeme_to_inflow(res)` (columns `HYD_flow`, `PHS_tp`, `PHS_frp`; add `DOPR_GM3` to TP as in the calibration objective). The method is already used in `calibration/historical/budget_outflow_pair.R`. Then compute annual export (t P/yr = flow x TP summed over the year), mean and wet/dry-year export, and flow-weighted outlet TP.
- Parameters: the roughly 50 that carry most of the sensitivity (the 16-parameter free set, the 8 physics parameters, alum, `rpop_hydrol`, `w_pom` and so on) plus `crest_elev` and `crest_factor`. Consider adding inflow and flood scaling (`isFlood` factor, stream-fractionation multiplier), which the first screen did not cover.
- Size: about 1,000 runs for 20 trajectories, roughly 8 h at 6 workers (about 3 min per run). Leave the machine alone meanwhile (11.7 GB RAM; 10 workers ran out of memory).
- Reuse: results are resumable (`SA_DIR`, `SA_R`); use a new `S2_DIR` so the first screen's results are not mixed in.
- Compare against the Ohau Channel outlet record, with the caveat about a possible lab or method change in that record.

## Related
- Write-up of the first screen: `website/rotorua-alum-next-steps.qmd`, section "Sensitivity analysis on the latest setup".
- Outputs: `website/data/sensitivity/`, `website/images/sensitivity/`.

## Reference values already in the write-up (best member of stage j, 2011 to 2021)
From the section "Nutrient budgets and the outflow, alum on and off" of `website/rotorua-alum-next-steps.qmd`: modelled export 10.6 t P/yr with alum (24% below the observed estimate of 13.9 t P/yr, which uses one lab sample a month times same-day flow) and 29.9 t P/yr without alum. Outflow volume is good (r 0.97, bias -0.6%, NSE 0.93 against the Ohau discharge from October 2010). The budget closure is only good to about +-10 t P/yr. Use these as the baseline the screen's export outputs should reproduce at the base point.
