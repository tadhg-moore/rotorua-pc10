#' Download an ArcGIS FeatureServer layer as a GeoJSON file
#'
#' @param service_url character, FeatureServer URL (without layer id).
#' @param dest character, path of the GeoJSON file to write.
#' @param layer_id integer, layer id within the service. Default 0.
#'
#' @return character, `dest` (for use with `tar_target(format = "file")`).
download_arcgis_layer <- function(service_url, dest, layer_id = 0) {
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  query_url <- paste0(
    sub("/+$", "", service_url), "/", layer_id,
    "/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
  )
  download.file(query_url, destfile = dest, mode = "wb", quiet = TRUE)
  dest
}
