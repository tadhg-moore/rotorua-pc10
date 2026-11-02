# Plan: three functional phytoplankton groups (diatoms, greens, buoyant cyanobacteria)

Draft, not started. Source: `C:\Users\mooret\Git\libaed-water\src\aed_phytoplankton.F90` (read for this plan) and `aed_util.F90` (`fTemp_function`). Current setup: `LID11133_rotorua/glm_aed/aed/aed_phyto_pars.csv`, best member values from `calibration/step2j/runs/step2_best_params.csv`.

## Target succession

| Season | Dominant group | Why |
|---|---|---|
| Late winter to spring (Aug to Nov) | Diatoms | cool water, low light, mixed column |
| Late spring to early summer, and autumn (Nov to Dec, Apr to Jun) | Greens | moderate T and light, grazed by zooplankton so they stay below the others |
| Warm, calm late summer (Jan to Mar) | Cyanobacteria | warm, stratified, P-limited surface layer; buoyant, so they collect at the surface when mixing is weak |

Total chlorophyll-a must stay where it is (mean about right). The change is a redistribution between groups, so the total-chlorophyll terms of the joint objective act as the constraint.

## What the best member does now

- Diatoms hold 95 to 99% of the biomass from July to November and about 16 to 20 mmol C/m3 all year. Cyanobacteria add the summer peak (63 to 72% of biomass in Jan to Mar). Greens sit at the `p0` floor (0.03 mmol C/m3) in every month (`website/images/phyto/`).
- Greens: `R_growth` 0.22 /d (diatom 2.4, cyano 1.2), `K_N` 10.7, `K_P` 0.22 (highest of the three), `I_K` 48 (highest), `R_resp` 0.013. Their N and P growth factors fall to 0.15 to 0.3 in April and 0.3 to 0.5 in winter. They cannot compete at any time of year.
- Cyanobacteria are not buoyant: `settling = 1,1,1` (constant velocity) and cyano `w_p` = -0.022 m/d, so they sink slowly. Nothing in the setup ties their bloom to calm water.
- Zooplankton: `aed_zoop_pars.csv` has `num_prey = 1` for all three zooplankton, so only `prey(1)` is active. That is `PHY_green` for zoop 1 and 2 and `ZOO_rotifer` for zoop 3. Greens are the only phytoplankton grazed, and diatoms and cyanobacteria are not grazed at all.
- Check before editing: `aed_phyto_pars.csv` in the repo has `fT_Method` 0 for green and diatom (so `fTemp_function` returns 1), but the output shows temperature factors of 0.3 to 1.1 for both, which means method 1 is in force in the runs. Confirm which file the run reads (the `build` copy, the calibration overlay).

## Levers in the source, by group

**1. Temperature (`fT_Method = 1`, `T_std`, `T_opt`, `T_max`, `theta_growth`).** `fTemp_function` gives `theta^(T-20)` below `T_std`, a decline to 0 at `T_max`, and 0 above. Set the optima apart: diatoms `T_std` about 15, `T_max` about 25; greens `T_std` about 22, `T_max` about 32; cyano `T_std` about 25, `T_max` 35. This is the main lever for the seasonal order. `theta_growth` sets how steeply growth falls in the cold; the current diatom theta (1.116) is already steep.

**2. Light (`I_K`, `lightModel`, `KePHY`).** Low `I_K` for diatoms (about 15 to 25) so they win in low light and deep mixing; higher for greens and cyano. A higher `KePHY` for cyano lets a surface bloom shade the others.

**3. Buoyancy (`settling`, `w_p`, `c1`, `c3`, `f1`, `f2`, `d_phy`, `min_rho`, `max_rho`).** Options in the source: 0 off, 1 constant, 2 constant with viscosity correction, 3 Stokes law with a cell density state variable (carbohydrate ballast: density rises in light by `c1*(1-exp(-par/I_K))` and falls by `c3`), 4 motile (swims up when internal N is above `f2*X_nmax` and PAR above `I_K`, down when below `f1*X_nmax`, stops at the surface).
- Stage A (minimum change): `settling = 1` for cyano with a positive `w_p` (float), for example +0.3 to +1.5 m/d. Whether this gives "calm water" blooms then depends on the model's mixing, since turbulence at wind speeds above a threshold mixes the column faster than the float velocity. This needs no new parameters.
- Stage B: option 4 (motile). Needs `f1`, `f2` (nutrient quota thresholds) and `w_p`; behaviour then follows light and nutrient status, so cells rise when replete and sink when starved. The columns exist in the parameter reader (`c1 c3 f1 f2 d_phy`), but the repo csv has no values for them, so they must be added.
- Stage C: option 3 (Stokes with density). Most realistic for scum formation, most parameters, and it adds a state variable per group. Only if A and B do not give the timing.
- Only the cyano `settling` switch changes the namelist: `settling = 1,1,1` becomes `4,1,1` or `3,1,1` (the namelist lists groups in the order of `the_phytos = 2, 3, 7`: cyano, green, diatom). Keep greens at small sinking (`w_p` about -0.05 m/d) and diatoms at faster sinking (about -0.15 m/d, as now).

**4. Nutrients (`K_P`, `K_N`, `X_pmin`, `X_pmax`, `R_puptake`, `simNFixation`).** Cyano: lowest `K_P` and highest `X_pmax` of the three (luxury P storage lets them draw on pulses and bloom after the diatoms have used the spring supply); turn on `simNFixation` with `R_nfix` and `k_nfix` if N falls below demand in summer (the current cyano N factor is 1, so N fixation is not needed yet). Diatoms: low `K_N` and `K_P` (they win in the cool, nutrient-rich season). Greens: moderate `K`, so they are limited in summer but not in winter.

**5. Growth, respiration and loss (`R_growth`, `R_resp`, `theta_resp`, `k_fres`).** Greens need `R_growth` around 0.7 to 1.2 (module default 0.7) instead of 0.22, with `R_resp` at 0.03 to 0.08 so net growth is positive at moderate temperature. Lower the diatom `R_growth` from 2.4 to about 1.5 to 2.0 if they still hold the total in summer.

**6. Grazing (`aed_zoop_pars.csv`, `num_prey`, `prey(i)%zoop_prey`, `Pzoo_prey`).** Set `num_prey` to 2 or 3 so zoop 1 and 2 graze greens and diatoms (and zoop 3 graze small cells), and keep cyano unpalatable (a low `Pzoo_prey` or no entry). Grazing is the way to keep greens from exceeding the others, and to terminate the diatom spring peak; without it the group with the best growth wins everywhere. This also changes total chlorophyll, so it comes with a refit of the grazing parameters.

## Staged approach

1. **Verify setup.** Confirm which csv the run reads, and that the zoop prey list is what the output implies. Write the group biomass for the best member as a reference (done: `calibration/historical/phyto_succession.R`).
2. **Hand tuning (single runs, about 100 s each).** Apply levers 1, 2, 5 to give greens a niche, then lever 6 for grazing, then lever 3 stage A for cyano. Check `succession_seasonal.png` after each step. Success criteria: greens above 15% of biomass in at least two months, cyano above 50% only in Jan to Mar, diatom share above 70% in Aug to Oct, total chlorophyll within about 10% of the best member, no changes to TP, FRP, DO or temperature scores.
3. **Calm-water test.** Compare cyano biomass in the top 2 m with wind speed and the Schmidt stability (or the thermocline depth) for each summer; the biomass should be highest in the calm, warm weeks and lowest after wind events. Daily output has `MET_wndspd` and `HYD_schstb` (check).
4. **Ensemble.** Free about 25 parameters in the joint ES-MDA framework (`calibration/step2/`, `S2_JOINT=1`): per group `T_std`, `T_max`, `I_K`, `K_P`, `K_N`, `R_growth`, `R_resp`; cyano `w_p` (and `f1`, `f2` for option 4); zooplankton `Pzoo_prey`. Keep alum, sediment and oxygen parameters fixed at the best member. The objective keeps its total-chlorophyll, TP, FRP and TN terms, and gets group terms (below).
5. **Re-run the historical member and alum comparison** (`run_hist.R`), because a shift from diatoms to greens changes the production of P-rich particles and the response to alum; the alum-on and alum-off pair must be rerun.

## Observations and objective terms

- No group-level data are used at present. `calibration/00_config.R` already maps `PHY_cyano` to `PHY_lcyano` and `PHY_tchla`, and `data/raw/LID_11133.rds` may hold cyanobacteria (and other) counts or biovolume; check this first, and ask for the BOPRC phytoplankton counts if not.
- Without group data, use soft targets from the literature on Lake Rotorua: a diatom spring peak, a cyanobacteria summer peak with peak share in the observed range, and monthly cyano share. The terms would be: bias in the share of cyanobacteria in January to March, bias in the diatom share in August to October, penalty on a group below its floor (`p0`) for more than 90% of days. Weights must be small so group shares cannot trade against total chlorophyll.

## Risks

- Adding a state variable (`rho`, option 3) and a vertical migration velocity can make GLM unstable or slow; test one run first and compare run time (about 97 s per model run now).
- Greens at higher growth and cyano buoyancy both raise surface chlorophyll in summer, so total chlorophyll will move; the refit of `R_growth` and grazing is needed to bring it back.
- `fT_Method` and the source version: the GLM binary was checked as identical across AEME versions, but confirm the AED library used for `glm.exe` matches the checked-out `libaed-water` (the `AED_Tools_Private/libaed-dev` copy is the one noted for the alum module).
- Group parameters are weakly identified from total chlorophyll alone. Without group observations the result is a plausible succession, not a fitted one, and the page should say so.

## Open decisions for the user

1. Order of the three groups through the year: the plan reads the request as diatoms in the cool months, greens in between and autumn, cyanobacteria in warm calm summer weather. Confirm.
2. Buoyancy: stage A (constant positive `w_p`) first, or go straight to option 4 or 3.
3. Whether group-level observations exist or can be obtained.
