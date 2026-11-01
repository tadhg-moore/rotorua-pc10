# Note: flux diagnostics for the alum module (proposed, not done)

Written 2026-11-01. Status: not started, waiting for a go-ahead.

## Why
The fate of alum and alum-bound P (`calibration/alum/50_alum_fate.R`, `51_alum_fate_diagram.R`, section "Alum applied per P removed, and the fate of alum and P" in `website/rotorua-alum-next-steps.qmd`) is rebuilt from saved pools and the module rates. Only `ALU_uptake_w` and `ALU_uptake_s` are written. The rebuilt budget does not close: settling and burial (about 23 to 25 t P/yr) are about twice the diagnosed uptake (12.4 t P/yr), the pelagic capacity balance misses by 22% and the bound-P balance by 85%.

## Proposed change
In `aed_alum.F90` (`C:\Users\mooret\Git\AED_Tools_Private\libaed-dev\src`), add diagnostic variables, in the units of the pools per day:
- water column: ageing of capacity (`ala` loss), deposition of `ala` and `alp` (`w_ala` times bottom-cell concentration)
- benthic zones: ageing, burial and resuspension of `ben_ala` and `ben_alp`, and benthic uptake
- keep the existing `ALU_uptake_w` and `ALU_uptake_s`

Then:
1. Rebuild GLM-AED with the changed library (the binary only changes results if the model equations change; diagnostics alone should leave output bit-identical, which is the first check).
2. Rerun member 46 of stage j (alum on, 2005-01 to 2022-06) with the new variables in the output.
3. Replace the reconstructed fluxes in `50_alum_fate.R` with the diagnostics, rerun `51_alum_fate_diagram.R`, and update the section.

## Notes
- Output variables to remember: `ALU_ben_*` non-`_Z` variables equal zone 1; use the `_Z` variables with `zarea` (4 zones, time-varying). `tot_outflow_vol` in the output nc is all zero: use `AEME::aeme_to_inflow()` (`HYD_flow`).
- The previous GLM binary is not kept in the repository (backed up only in a session scratchpad, 2026-10-30; see the AEME dev update memory). Keep a copy of the current `glm.exe` before rebuilding.
- If the budget still does not close with the module's own fluxes, the problem is in the mass balance (for example the GLM P budget points to 15 to 17 t P/yr), not in the reconstruction.
