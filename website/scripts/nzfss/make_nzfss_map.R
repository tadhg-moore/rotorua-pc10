# Interactive Lake Rotorua map for the NZFSS 2026 alum presentation.
# Inputs come from the targets store (run from anywhere; paths via here::here).
# Output: website/figures/map_rotorua.html (self-contained), shown as a
# full-screen interactive slide background in nzfss-2026-alum.qmd.

library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

store <- here::here("_targets")
rd <- function(x) targets::tar_read_raw(x, store = store)

sw   <- rd("boprc_lake_catchments")      # surface-water catchments (WGS84)
gw   <- rd("boprc_gw_subcatchments")     # groundwater sub-catchments (WGS84)
dose <- sf::st_transform(rd("alum_dosing_sites"), 4326)
ohau <- rd("ohau_weir_sf")
buoy <- rd("buoy_sf")
streams <- rd("stream_centrelines")   # Puarenga and Utuhina stream centrelines (WGS84)
sites <- sf::st_transform(rd("chem_sites_sf"), 4326)

# monitoring points: lake buoy + BOPRC sampling sites 2 and 5
mon <- rbind(
  sf::st_sf(label = "Lake buoy", col = "#f1c40f", geometry = sf::st_geometry(buoy)),
  sf::st_sf(label = sub(".*(Site [0-9]).*", "\1", sites$LocationName),
            col = "#3498db", geometry = sf::st_geometry(sites))
)

# ---- prepare layers ---------------------------------------------------------
sw <- sw[sw$Catchment == "Lake Rotorua", ]
sw$SubCatchment <- trimws(sw$SubCatchment)
is_lake   <- sw$SubCatchment == "" | grepl("^Mokoia", sw$SubCatchment)
lake      <- sw[is_lake, ]
sw        <- sw[!is_lake, ]
sw$dosed  <- sw$SubCatchment %in% c("Puarenga", "Utuhina")
sw$area_km2 <- round(sw$Shape__Area / 1e6, 1)
gw$area_km2 <- round(gw$Shape__Area / 1e6, 1)

bb <- as.list(sf::st_bbox(sf::st_union(rbind(sw[, "geometry"], lake[, "geometry"]))))

col_sw <- "#2e8b57"   # surface catchments
col_dose <- "#d95f02" # dosed catchments / dosing points
col_gw <- "#7b3f9e"   # groundwater
col_out <- "#c0392b"

sw_pop <- sprintf("<b>%s</b><br>Surface catchment, %.1f km&sup2;%s",
                  sw$SubCatchment, sw$area_km2,
                  ifelse(sw$dosed, "<br><i>Alum-dosed inflow</i>", ""))
gw_pop <- sprintf("<b>%s</b><br>Groundwater catchment, %.1f km&sup2;",
                  gw$GroundwaterSubcatchment, gw$area_km2)

m <- leaflet(options = leafletOptions(zoomControl = TRUE, zoomSnap = 0.25, keyboard = FALSE)) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
  fitBounds(bb$xmin, bb$ymin, bb$xmax, bb$ymax) |>
  # lake outline always visible
  addPolygons(data = lake, group = "lake", weight = 2, color = "#ffffff",
              fillColor = "#1b6ca8", fillOpacity = 0.35, options = pathOptions(interactive = FALSE)) |>
  # surface catchments (animated)
  addPolygons(data = sw, group = "Surface catchments", weight = 1.5,
              color = "#ffffff",
              fillColor = ifelse(sw$dosed, col_dose, col_sw),
              fillOpacity = 0, opacity = 0, popup = sw_pop, options = pathOptions(className = "g-sw"),
              label = sw$SubCatchment) |>
  # stream centrelines (animated draw-in)
  addPolylines(data = streams, group = "Streams", weight = 3.5, color = "#7fd3f7",
               opacity = 0, options = pathOptions(className = "g-str"),
               label = streams$stream) |>
  # alum dosing points (animated pulse)
  addCircleMarkers(data = dose, group = "Alum dosing points", radius = 0,
                   weight = 3, color = "#ffffff", fillColor = col_dose,
                   fillOpacity = 0, opacity = 0, options = pathOptions(className = "g-dose"),
                   label = ~paste(site, "alum dosing"),
                   labelOptions = labelOptions(permanent = FALSE)) |>
  # outflow (animated)
  addCircleMarkers(data = ohau, group = "Outflow", radius = 0, weight = 3,
                   color = "#ffffff", fillColor = col_out, fillOpacity = 0, opacity = 0, options = pathOptions(className = "g-out"),
                   label = "Ohau Channel outflow (weir)") |>
  # monitoring: buoy + sampling sites (animated)
  addCircleMarkers(data = mon, group = "Monitoring", radius = 0, weight = 3,
                   color = "#ffffff", fillColor = mon$col, fillOpacity = 0, opacity = 0,
                   options = pathOptions(className = "g-mon"), label = mon$label) |>
  addLayersControl(baseGroups = c("Satellite", "Light"),
                   overlayGroups = c("Surface catchments", "Streams", "Alum dosing points", "Outflow", "Monitoring"),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topright") |>
  addScaleBar("bottomleft")

# ---- animation --------------------------------------------------------------
js <- sprintf('
function(el, x) {
  var map = this;
  var C = {sw: "%s", dose: "%s", gw: "%s", out: "%s"};
  var cls = {"Surface catchments":"g-sw","Streams":"g-str","Alum dosing points":"g-dose","Outflow":"g-out","Monitoring":"g-mon"};
  function layers(name) { var a = []; map.eachLayer(function(l){ if (l.options && l.options.className === cls[name]) a.push(l); }); return a; }
  var timers = [];
  // keyboard: hand slide navigation keys to the parent Reveal deck (map keyboard panning is off)
  var keyMap = {ArrowRight: "next", ArrowDown: "next", PageDown: "next", " ": "next",
                ArrowLeft: "prev", ArrowUp: "prev", PageUp: "prev"};
  document.addEventListener("keydown", function(e) {
    var m = keyMap[e.key];
    if (m && window.parent !== window) {
      e.preventDefault();
      window.parent.postMessage(JSON.stringify({method: m}), "*");
    }
  });
  function tween(dur, delay, fn, done) {
    var t0 = null, id;
    var start = setTimeout(function() {
      function step(ts) {
        if (t0 === null) t0 = ts;
        var p = Math.min((ts - t0) / dur, 1);
        fn(p);
        if (p < 1) { id = requestAnimationFrame(step); } else if (done) { done(); }
      }
      id = requestAnimationFrame(step);
    }, delay);
    timers.push(start);
  }
  function fadePolys(name, delay, dur, fill, stagger) {
    layers(name).forEach(function(l, i) {
      tween(dur, delay + i * (stagger || 0), function(p) {
        var dosed = l.options.fillColor === C.dose;
        l.setStyle({fillOpacity: fill * p * (dosed ? 1.3 : 1), opacity: 1 * p});
      });
    });
  }
  function drawLines(name, delay, dur) {
    layers(name).forEach(function(l, i) {
      var p = l._path; if (!p || !p.getTotalLength) return;
      var L = p.getTotalLength();
      tween(dur, delay, function(q) {
        if (q === 0) return;
        l.setStyle({opacity: 0.95});
        p.style.strokeDasharray = L + " " + L;
        p.style.strokeDashoffset = (L * (1 - q)) + "";
      }, function() { p.style.strokeDasharray = ""; p.style.strokeDashoffset = ""; });
    });
  }
  function popMarkers(name, delay, R) {
    layers(name).forEach(function(l, i) {
      tween(700, delay + i * 250, function(p) {
        var e = 1 - Math.pow(1 - p, 3);
        l.setStyle({fillOpacity: 0.95 * e, opacity: e});
        l.setRadius(R * e);
      }, function() {
        // gentle pulse via a halo
        var halo = L.circleMarker(l.getLatLng(), {radius: R, color: l.options.fillColor,
          weight: 2, fill: false, interactive: false}).addTo(map);
        halo._pulse = true;
        var t = 0;
        var iv = setInterval(function() {
          t = (t + 0.03) %% 1;
          halo.setRadius(R + t * R * 1.6);
          halo.setStyle({opacity: 0.8 * (1 - t)});
        }, 30);
        timers.push(iv);
        map._halos = (map._halos || []).concat([[halo, iv]]);
      });
    });
  }
  function reset() {
    timers.forEach(function(t){ clearTimeout(t); clearInterval(t); });
    timers = [];
    (map._halos || []).forEach(function(h){ map.removeLayer(h[0]); });
    map._halos = [];
    layers("Surface catchments").forEach(function(l){
      l.setStyle({fillOpacity: 0, opacity: 0}); });
    layers("Streams").forEach(function(l){ l.setStyle({opacity: 0}); var p = l._path; if (p) { p.style.strokeDasharray = ""; p.style.strokeDashoffset = ""; } });
    layers("Alum dosing points").concat(layers("Outflow")).concat(layers("Monitoring")).forEach(function(l){
      l.setStyle({fillOpacity: 0, opacity: 0}); l.setRadius(0); });
    setStep("");
  }
  var stepEl = document.getElementById("map-step");
  function setStep(txt) { if (stepEl) { stepEl.innerHTML = txt; stepEl.style.opacity = txt ? 1 : 0; } }
  function play() {
    reset();
    setStep("Surface-water catchments");
    fadePolys("Surface catchments", 300, 900, 0.30, 120);
    timers.push(setTimeout(function(){ setStep("Streams: Puarenga and Utuhina"); }, 3800));
    drawLines("Streams", 4000, 2200);
    timers.push(setTimeout(function(){ setStep("Alum dosing points: Utuhina and Puarenga streams"); }, 6800));
    popMarkers("Alum dosing points", 7000, 11);
    timers.push(setTimeout(function(){ setStep("Outflow: Ohau Channel weir"); }, 9800));
    popMarkers("Outflow", 10000, 11);
    timers.push(setTimeout(function(){ setStep("Monitoring: lake buoy and sampling sites 2 and 5"); }, 12800));
    popMarkers("Monitoring", 13000, 11);
    timers.push(setTimeout(function(){ setStep(""); }, 17000));
  }
  // legend
  var legend = L.control({position: "bottomright"});
  legend.onAdd = function() {
    var d = L.DomUtil.create("div", "map-legend");
    d.innerHTML =
      "<div><i style=\\"background:"+C.sw+"\\"></i> Surface catchments</div>" +
      "<div><i style=\\"background:#7fd3f7\\"></i> Puarenga and Utuhina streams</div>" +
      "<div><i style=\\"background:"+C.dose+"\\"></i> Alum-dosed catchments / dosing points</div>" +
      "<div><i style=\\"background:"+C.out+"\\"></i> Outflow (Ohau weir)</div>" +
      "<div><i style=\\"background:#f1c40f\\"></i> Lake buoy</div>" +
      "<div><i style=\\"background:#3498db\\"></i> Sampling sites (2 and 5)</div>";
    return d;
  };
  legend.addTo(map);
  // replay button
  var btn = L.control({position: "topleft"});
  btn.onAdd = function() {
    var b = L.DomUtil.create("a", "map-replay");
    b.href = "#"; b.innerHTML = "&#9654; Replay";
    L.DomEvent.on(b, "click", function(e){ L.DomEvent.stop(e); play(); });
    L.DomEvent.disableClickPropagation(b);
    return b;
  };
  btn.addTo(map);
  // step caption
  var cap = L.control({position: "bottomleft"});
  cap.onAdd = function() {
    var d = L.DomUtil.create("div", "map-step"); d.id = "map-step"; return d; };
  cap.addTo(map);
  stepEl = document.getElementById("map-step");
  setTimeout(play, 600);
}', col_sw, col_dose, col_gw, col_out)

css <- "
html, body { margin: 0; height: 100%; }
.leaflet-container { font-family: system-ui, sans-serif; }
.map-legend { background: rgba(255,255,255,.92); padding: 10px 14px; border-radius: 6px;
  font-size: 15px; line-height: 1.7; box-shadow: 0 1px 6px rgba(0,0,0,.3); }
.map-legend i { display: inline-block; width: 14px; height: 14px; margin-right: 8px;
  vertical-align: -2px; border-radius: 2px; }
.map-replay { background: #fff; padding: 6px 12px; font-size: 15px; border-radius: 4px;
  color: #1b3a4b !important; text-decoration: none; box-shadow: 0 1px 6px rgba(0,0,0,.3); display: block; }
.map-step { background: #1b3a4bd9; color: #fff; padding: 8px 16px; border-radius: 6px;
  font-size: 20px; font-weight: 600; margin-bottom: 28px; transition: opacity .4s; opacity: 0; }
"
m <- m |> htmlwidgets::onRender(js) |>
  htmlwidgets::prependContent(tags$style(HTML(css)))

# full-viewport widget
m$sizingPolicy <- htmlwidgets::sizingPolicy(defaultWidth = "100%", defaultHeight = "100vh",
                                            padding = 0, browser.fill = TRUE)

out <- here::here("website", "figures", "map_rotorua.html")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
if (!nzchar(Sys.which("pandoc"))) Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")
pandoc <- Sys.which("pandoc")
if (!nzchar(pandoc)) {
  q <- Sys.which("quarto")
  if (nzchar(q)) {
    cand <- file.path(dirname(q), "tools", c("", "x86_64"), "pandoc.exe")
    cand <- cand[file.exists(cand)]
    if (length(cand)) Sys.setenv(RSTUDIO_PANDOC = dirname(cand[1]))
  }
}
htmlwidgets::saveWidget(m, out, selfcontained = TRUE, title = "Lake Rotorua catchments")
message("Wrote ", out)
