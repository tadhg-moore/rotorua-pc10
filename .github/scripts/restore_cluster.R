cluster <- c(
  "secretbase", "targets", "tarchetypes", "nanonext", "mirai", "crew",
  "curl", "httr", "httr2", "AzureAuth", "AzureGraph", "rstudioapi",
  "getPass", "kableExtra", "XML", "tmaptools", "maptiles", "gh",
  "ecmwfr", "plotly", "tmap", "Microsoft365R", "bathytools", "AEME",
  "aemetools", "ltapi"
)

renv::restore(packages = cluster)
renv::restore()
