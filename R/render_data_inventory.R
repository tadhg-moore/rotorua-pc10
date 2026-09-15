# ── 1. Annotation table ──────────────────────────────────────────────────────
# Things that cannot be inferred from the object itself.
# Add a row for each target. Any target not listed here gets "Unknown" defaults.
# Only `name` is required; all other columns are optional overrides.

inventory_annotations <- function() {
  tibble::tribble(
    ~name,                    ~category,       ~source,                                                        ~notes,
    
    "lake_level",             "Observations",  "Bay of Plenty RC – bulk export zip",                          "Lake Rotorua water level (m AMSL); QC-filtered (qc_code > 200)",
    "niwa_met_daily",         "Climate",       "NIWA climate station #1770",                                   "Evaporation (3 methods), radiation, rainfall, temperature, wind",
    "niwa_met_hourly_files",  "Climate",       "NIWA climate station #1770",                                   "Hourly met file paths – auto-listed from data/raw/niwa_climate/",
    "rotorua_inflow",         "Observations",  "Chris McBride – previous DYRESM load modelling",               "Catchment inflow (Rotorua_inf_final.csv); converted from DYRESM format",
    "rotorua_buoy_pro_data",  "Observations",  "LERNZMP API – lake WQ profiler (lake_id 11133)",               "Lake Rotorua water-column profiler (temperature, DO, etc.)",
    "rotorua_buoy_met_data",  "Observations",  "LERNZMP API – lake met station (lake_id 11133)",               "Meteorological observations from the on-lake buoy",
    "ctd_data",               "Observations",  "Bay of Plenty RC – Excel (bop_wq/)",                          "Lake Rotorua CTD depth profiles – full record",
    "chem_excel_file",        "Observations",  "Bay of Plenty RC – Excel (bop_wq/)",                          "Lakes water chemistry 1989-2025 (nutrients etc.)",
    "lake_shape",             "Spatial",       "LERNZMP API (lake_id = 11133)",                                "Lake polygon as sf object (WGS84)",
    "lake_meta",              "Spatial",       "LERNZMP API (lake_id = 11133)",                                "Lake metadata: name, lat, lon, elevation",
    "rotorua_catchment_bbox", "Spatial",       "Processed RDS – rotorua_lakes_area.rds",                      "Bounding box / area polygon for the Rotorua lakes catchment",
    "tutira_catchment_bbox",  "Spatial",       "Hard-coded bbox coords in _targets.R",                        "xmin=176.75, ymin=-39.29, xmax=177.00, ymax=-39.16 (EPSG:4326)",
    "vcsn_grid_points",       "Spatial",       "VCSN – NZ Virtual Climate Station Network",                   "Grid of VCSN station points for spatial matching of CMIP6 data",
    "cmip6_metadata",         "GCM / CMIP6",   "CMIP6 archive – gather_cmip6_metadata()",                     "7 GCMs x 5 scenarios; spatial resolution and calendar per model",
    "gcm_point_data_df",      "GCM / CMIP6",   "Derived – point extraction from cmip6_files at lake centroid","Daily met series: tas, hurs, pr, rsds, sfcWind",
    "gcm_point_data_std_df",  "GCM / CMIP6",   "Derived – Gregorian-standardised gcm_point_data_df",          "Gregorian-calendar standardised version of gcm_point_data_df",
    "gcm_ts_df",              "GCM / CMIP6",   "Derived – time-series summaries from cmip6_files",             "Per-variable x GCM summary stats for visualisation",
    "aeme_base_hyps",         "Model config",  "Derived – corrected hypsograph applied to lernzmp_aeme",       "Base AEME object with fixed hypsograph; foundation for all model runs",
    "glm_sed_param",          "Model config",  "Derived – estimate_sed_zones(aeme_base_hyps)",                 "GLM-AED sediment zone estimates from the corrected hypsograph",
    "glm_sed_param_meas",     "Model config",  "Derived – AEME::glm_sed_params(zone_heights)",                 "GLM sediment zone parameters for the 5 measured zone heights",
    "meas_param",             "Model config",  "Kaylee Campbell – internal loading study (unconfirmed; repeat of Bergen 2004)", "Measured sediment flux by zone (fsed_oxy/amm/nit/frp) and sediment temp/vwc params",
    "aed_alum_params",        "Model config",  "Sanila (2026) MSc thesis, University of Waikato",              "AED aed_alum module parameters from jar tests & video settling measurements",
    "alum_dosing",            "Observations",  "Bay of Plenty RC – alum dosing plant records (Excel)",         "Daily alum dose (L/day) to Utuhina & Puarenga streams",
    "rotorua_inflow_list",    "Observations",  "Chris McBride – previous DYRESM load modelling",               "Per-stream inflow list, formatted from Rotorua_inf_final.csv",
    "depth_contours",         "Spatial",       "LERNZMP API (lake_id = 11133)",                                "Lake Rotorua bathymetric depth contours",
    "chem_data",              "Observations",  "Bay of Plenty RC – Excel (bop_wq/)",                          "Lake chemistry records filtered to Rotorua sites (All_data sheet)",
    "chem_sites",             "Observations",  "Bay of Plenty RC – Excel (bop_wq/)",                          "Rotorua chemistry sampling site metadata (Sites sheet)",
    "ctd_par",                "Observations",  "Derived – format_par_ts(ctd_data)",                            "Light extinction (Kd) time series derived from CTD PAR profiles",
    "light_data",             "Observations",  "Derived – ctd_par joined with chem_data",                      "Combined light/turbidity/chlorophyll/Secchi time series",
    "corr_hyps",              "Model config",  "Derived – fix_hyps(aeme_base, lake_elev = median_lake_level_masl)", "Corrected Rotorua hypsograph used to rebuild the AEME model"
  )
}


# ── 2. Object inspector ──────────────────────────────────────────────────────

#' Inspect a single R object and return a one-row data frame of metadata.
#'
#' @param obj   Any R object.
#' @param name  Character name of the target (used as the row label).
inspect_target <- function(obj, name) {
  
  cls <- paste(class(obj), collapse = ", ")
  
  # ── Detect spatial ──────────────────────────────────────────────────────
  is_spatial <- inherits(obj, c("sf", "sfc", "SpatVector", "SpatRaster",
                                "Raster", "stars", "bbox")) ||
    (is.data.frame(obj) && any(c("geometry", "geom") %in% names(obj)))
  
  # ── Dimensions ─────────────────────────────────────────────────────────
  dims <- if (is.data.frame(obj)) {
    sprintf("%s rows x %s cols", format(nrow(obj), big.mark = ","), ncol(obj))
  } else if (is.list(obj) && !is.data.frame(obj)) {
    sprintf("list of %s", length(obj))
  } else if (is.character(obj)) {
    sprintf("%s file path%s", length(obj), if (length(obj) != 1) "s" else "")
  } else if (is.numeric(obj) || is.integer(obj)) {
    sprintf("length %s", length(obj))
  } else {
    cls
  }
  
  # ── Date range & temporal resolution ───────────────────────────────────
  period   <- "N/A"
  temporal <- "Static"
  
  if (is.data.frame(obj)) {
    date_cols <- names(obj)[vapply(obj, function(col) {
      inherits(col, c("Date", "POSIXct", "POSIXlt"))
    }, logical(1))]
    
    if (length(date_cols) > 0) {
      dcol <- obj[[date_cols[1]]]
      dcol <- dcol[!is.na(dcol)]
      if (length(dcol) > 0) {
        rng    <- range(dcol)
        period <- paste(format(rng, "%Y-%m-%d"), collapse = " to ")
        
        # Guess temporal resolution from median gap between consecutive dates
        if (length(dcol) > 1) {
          gaps <- as.numeric(diff(sort(unique(dcol))), units = "days")
          med  <- stats::median(gaps, na.rm = TRUE)
          temporal <- dplyr::case_when(
            med < 0.1  ~ "Sub-hourly",
            med < 0.9  ~ "Hourly",
            med < 1.5  ~ "Daily",
            med < 8    ~ "Weekly",
            med < 32   ~ "Monthly",
            TRUE       ~ "Irregular"
          )
        }
      }
    }
  }
  
  # ── Column names preview ────────────────────────────────────────────────
  col_preview <- if (is.data.frame(obj) && ncol(obj) > 0) {
    cols <- names(obj)
    if (length(cols) > 8) {
      paste0(paste(cols[1:8], collapse = ", "), " ... (+", length(cols) - 8, " more)")
    } else {
      paste(cols, collapse = ", ")
    }
  } else {
    ""
  }
  
  tibble::tibble(
    name        = name,
    r_class     = cls,
    dims        = dims,
    temporal    = temporal,
    period      = period,
    spatial     = is_spatial,
    col_preview = col_preview,
    # Filled later by annotations:
    category    = NA_character_,
    source      = NA_character_,
    notes       = NA_character_
  )
}


# ── 3. Build full inventory ──────────────────────────────────────────────────

#' Inspect a named list of target objects and join with manual annotations.
#'
#' @param targets_list  Named list: list(lake_level = lake_level, ...)
#' @param annotations   Data frame from inventory_annotations(), or NULL.
#' @return              A data frame suitable for render_data_inventory().

build_inventory <- function(targets_list, annotations = NULL) {
  
  stopifnot(is.list(targets_list), !is.null(names(targets_list)))
  
  rows <- mapply(
    inspect_target,
    obj  = targets_list,
    name = names(targets_list),
    SIMPLIFY = FALSE
  )
  inv <- dplyr::bind_rows(rows)
  
  if (!is.null(annotations)) {
    stopifnot(is.data.frame(annotations), "name" %in% names(annotations))
    inv <- dplyr::rows_update(inv, annotations, by = "name", unmatched = "ignore")
  }
  
  # Defaults for anything not annotated
  inv$category[is.na(inv$category)] <- "Other"
  inv$source[is.na(inv$source)]     <- "Unknown"
  inv$notes[is.na(inv$notes)]       <- ""
  
  inv
}


# ── 4. HTML renderer ─────────────────────────────────────────────────────────

#' Render a data inventory data frame to a self-contained HTML file.
#'
#' @param inventory_df  Output of build_inventory().
#' @param out_file      Path to write the HTML file.
#' @param project       Project name shown in the page title.
#' @return              out_file, for targets format = "file".

render_data_inventory <- function(inventory_df,
                                  out_file = here::here("docs", "data_inventory.html"),
                                  project  = "Rotorua Lake Modelling") {
  
  required <- c("name", "category", "source", "temporal", "period", "spatial")
  stopifnot(is.data.frame(inventory_df),
            all(required %in% names(inventory_df)))
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  esc_js <- function(x) {
    x <- gsub("\\", "\\\\", x, fixed = TRUE)
    x <- gsub("'",  "\\'",  x, fixed = TRUE)
    x <- gsub("\n", " ",    x, fixed = TRUE)
    x
  }
  
  row_to_js <- function(r) {
    sprintf(
      "{name:'%s',cat:'%s',source:'%s',temporal:'%s',period:'%s',spatial:%s,dims:'%s',cols:'%s',cls:'%s',notes:'%s'}",
      esc_js(r["name"]),
      esc_js(r["category"]),
      esc_js(r["source"]),
      esc_js(r["temporal"]),
      esc_js(r["period"]),
      tolower(as.character(isTRUE(r["spatial"]))),
      esc_js(if (!is.null(r["dims"]))        r["dims"]        else ""),
      esc_js(if (!is.null(r["col_preview"])) r["col_preview"] else ""),
      esc_js(if (!is.null(r["r_class"]))     r["r_class"]     else ""),
      esc_js(r["notes"])
    )
  }
  
  js_rows <- paste(apply(inventory_df, 1, row_to_js), collapse = ",\n")
  
  cats <- unique(inventory_df$category)
  filter_btns <- paste(
    sprintf(
      '<button class="filter-btn" onclick="setFilter(\'%s\',this)">%s</button>',
      cats, cats
    ),
    collapse = "\n    "
  )
  
  generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  n_spatial    <- sum(isTRUE(inventory_df$spatial) |
                        inventory_df$spatial == TRUE, na.rm = TRUE)
  
  # Using paste0 to avoid sprintf %% escaping issues with CSS
  html <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Data Inventory \u2014 ', project, '</title>
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
body{font-family:system-ui,sans-serif;font-size:14px;color:#1a1a1a;background:#f5f5f3;padding:2rem 1rem}
h1{font-size:20px;font-weight:500;margin-bottom:0.25rem}
.meta{font-size:12px;color:#777;margin-bottom:1.25rem}
.stats{display:flex;gap:12px;margin-bottom:1.5rem;flex-wrap:wrap}
.stat{background:#fff;border:0.5px solid #d8d6cf;border-radius:10px;padding:10px 16px;min-width:110px}
.stat-val{font-size:22px;font-weight:500;line-height:1.2}
.stat-lbl{font-size:11px;color:#888;margin-top:2px}
.card{background:#fff;border:0.5px solid #d8d6cf;border-radius:12px;padding:1.25rem}
.toolbar{display:flex;gap:8px;flex-wrap:wrap;align-items:center;margin-bottom:1rem}
input[type=search]{padding:5px 10px;font-size:13px;border-radius:8px;border:0.5px solid #bbb;flex:1;min-width:160px;max-width:260px;background:#fff;color:#1a1a1a}
.filter-btn{padding:4px 10px;border-radius:8px;border:0.5px solid #ccc;background:#fff;color:#555;cursor:pointer;font-size:12px}
.filter-btn.active,.filter-btn:hover{background:#f0eeea;color:#1a1a1a;border-color:#999}
.filter-btn.active{font-weight:500}
.count{font-size:12px;color:#777;margin-bottom:8px}
.table-wrap{overflow-x:auto}
table{width:100%;border-collapse:collapse;font-size:13px}
th{text-align:left;font-weight:500;font-size:11px;color:#888;padding:6px 10px;border-bottom:0.5px solid #e0ddd6;text-transform:uppercase;letter-spacing:0.04em;white-space:nowrap;cursor:pointer;user-select:none}
th:hover{color:#444}
th .si{margin-left:3px;opacity:0.35}
th.sorted .si{opacity:1}
td{padding:8px 10px;border-bottom:0.5px solid #eeece6;vertical-align:top;line-height:1.45}
tr:last-child td{border-bottom:none}
tr:hover td{background:#faf9f6}
.tname{font-weight:500;font-size:13px}
.note{font-size:11px;color:#777;margin-top:2px}
.cols{font-size:10px;color:#bbb;margin-top:3px;font-family:ui-monospace,monospace;line-height:1.5}
.badge{display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:500;white-space:nowrap}
.sp-yes{background:#eaf3de;color:#3b6d11}
.sp-no{background:#f1efe8;color:#5f5e5a}
.cls-pill{background:#f1efe8;color:#888;font-family:ui-monospace,monospace;font-size:10px;padding:1px 6px;border-radius:6px;display:inline-block;margin-top:3px}
@media(prefers-color-scheme:dark){
  body{background:#1c1c1a;color:#e8e6df}
  .stat,.card{background:#252523;border-color:#3a3835}
  input[type=search]{background:#1c1c1a;color:#e8e6df;border-color:#555}
  .filter-btn{background:#252523;color:#aaa;border-color:#444}
  .filter-btn.active,.filter-btn:hover{background:#333;color:#e8e6df}
  th{color:#666;border-color:#333}
  td{border-color:#2e2c28}
  tr:hover td{background:#2e2c28}
  .note{color:#888}
  .cols{color:#444}
  .stat-lbl{color:#666}
  .sp-yes{background:#27500a;color:#c0dd97}
  .sp-no{background:#3a3835;color:#888780}
  .cls-pill{background:#3a3835;color:#666}
}
</style>
</head>
<body>
<h1>Data Inventory \u2014 ', project, '</h1>
<p class="meta">Generated ', generated_at, '</p>

<div class="stats">
  <div class="stat"><div class="stat-val">', nrow(inventory_df), '</div><div class="stat-lbl">Datasets</div></div>
  <div class="stat"><div class="stat-val">', n_spatial, '</div><div class="stat-lbl">Spatial</div></div>
  <div class="stat"><div class="stat-val">', length(cats), '</div><div class="stat-lbl">Categories</div></div>
</div>

<div class="card">
  <div class="toolbar">
    <input type="search" id="search" placeholder="Search\u2026" oninput="doFilter()">
    <button class="filter-btn active" onclick="setFilter(\'all\',this)">All</button>
    ', filter_btns, '
  </div>
  <div class="count" id="count"></div>
  <div class="table-wrap">
    <table>
      <thead><tr>
        <th onclick="sortBy(\'name\')">Target <span class="si">\u21c5</span></th>
        <th onclick="sortBy(\'cat\')">Category <span class="si">\u21c5</span></th>
        <th>Source</th>
        <th onclick="sortBy(\'temporal\')">Temporal <span class="si">\u21c5</span></th>
        <th>Date range</th>
        <th onclick="sortBy(\'dims\')">Dimensions <span class="si">\u21c5</span></th>
        <th onclick="sortBy(\'spatial\')">Spatial <span class="si">\u21c5</span></th>
      </tr></thead>
      <tbody id="tbody"></tbody>
    </table>
  </div>
</div>

<script>
const DATA = [
', js_rows, '
];
const PALETTE = [
  {bg:"#e1f5ee",tx:"#0f6e56",dbg:"#085041",dtx:"#9fe1cb"},
  {bg:"#e6f1fb",tx:"#185fa5",dbg:"#0c447c",dtx:"#b5d4f4"},
  {bg:"#eeedfe",tx:"#534ab7",dbg:"#3c3489",dtx:"#cecbf6"},
  {bg:"#faeeda",tx:"#854f0b",dbg:"#633806",dtx:"#fac775"},
  {bg:"#faece7",tx:"#993c1d",dbg:"#71200d",dtx:"#f5c4b3"},
  {bg:"#fbeaf0",tx:"#993556",dbg:"#72243e",dtx:"#f4c0d1"}
];
const dark = window.matchMedia("(prefers-color-scheme: dark)").matches;
const cats = [...new Set(DATA.map(d => d.cat))];
const catColor = Object.fromEntries(cats.map((c,i) => {
  const p = PALETTE[i % PALETTE.length];
  return [c, dark ? {bg:p.dbg,tx:p.dtx} : {bg:p.bg,tx:p.tx}];
}));

let activeFilter = "all", sortCol = null, sortDir = 1;

function catBadge(cat) {
  const {bg,tx} = catColor[cat]||{bg:"#eee",tx:"#333"};
  return `<span class="badge" style="background:${bg};color:${tx}">${cat}</span>`;
}

function render(rows) {
  document.getElementById("tbody").innerHTML = rows.map(r => `<tr>
    <td>
      <div class="tname">${r.name}</div>
      ${r.notes ? `<div class="note">${r.notes}</div>` : ""}
      ${r.cols  ? `<div class="cols">${r.cols}</div>`  : ""}
    </td>
    <td>${catBadge(r.cat)}<br><span class="cls-pill">${r.cls}</span></td>
    <td style="font-size:12px">${r.source}</td>
    <td style="white-space:nowrap">${r.temporal}</td>
    <td style="font-size:12px">${r.period}</td>
    <td style="white-space:nowrap;font-size:12px">${r.dims}</td>
    <td><span class="badge ${r.spatial?"sp-yes":"sp-no"}">${r.spatial?"Yes":"No"}</span></td>
  </tr>`).join("");
  document.getElementById("count").textContent =
    rows.length + " of " + DATA.length + " datasets shown";
}

function getFiltered() {
  const q = document.getElementById("search").value.toLowerCase();
  return DATA.filter(r => {
    const matchCat = activeFilter === "all" || r.cat === activeFilter;
    const matchQ   = !q || [r.name,r.source,r.notes,r.cat,r.cols,r.cls,r.dims]
      .some(s => s && s.toLowerCase().includes(q));
    return matchCat && matchQ;
  });
}

function doFilter() { render(getSorted(getFiltered())); }

function setFilter(f, el) {
  activeFilter = f;
  document.querySelectorAll(".filter-btn").forEach(b => b.classList.remove("active"));
  el.classList.add("active");
  doFilter();
}

function getSorted(rows) {
  if (!sortCol) return rows;
  return [...rows].sort((a,b) => {
    const av = String(a[sortCol]||"").toLowerCase();
    const bv = String(b[sortCol]||"").toLowerCase();
    return av < bv ? -sortDir : av > bv ? sortDir : 0;
  });
}

function sortBy(col) {
  sortDir = sortCol === col ? -sortDir : 1;
  sortCol = col;
  document.querySelectorAll("th").forEach(th => th.classList.remove("sorted"));
  event.currentTarget.classList.add("sorted");
  doFilter();
}

render(DATA);
</script>
</body>
</html>')
  
  writeLines(html, out_file)
  out_file
}