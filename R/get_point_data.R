KELVIN_VARS <- c("tas", "tasmax", "tasmin")

## metscale::extract_climate_point() returns AEME MET_* column names; map them
## back to the raw CMIP6 short names (matching `cmip_vars`) used throughout
## this project.
.met_to_cmip_var <- c(
  MET_tmpair = "tas",
  MET_pprain = "pr",
  MET_wndspd = "sfcWind",
  MET_radswd = "rsds",
  MET_humrel = "hurs",
  MET_radlwd = "rlds",
  MET_prsttn = "ps"
)

#' Extract point data across multiple CMIP6 files
#'
#' Wraps \code{\link[metscale]{extract_climate_point}} to sample the requested
#' GCM/scenario files at a point, decode the model calendar, and convert units
#' (Kelvin to Celsius, precipitation to mm/day).
#'
#' @param lat            Latitude (WGS84)
#' @param lon            Longitude (WGS84)
#' @param lakename       Lake identifier (passed through to output, unused in extraction)
#' @param gcm            GCM name to filter on
#' @param scenario       Scenario to filter on
#' @param cmip_vars      Character vector of CMIP6 short variable names to include
#' @param cmip6_files    Character vector of all available file paths
#' @param cmip6_metadata Data frame with columns: gcm, scenario, variable, filename
#' @param method         Extraction method passed to
#'   \code{metscale::extract_climate_point()}: "bilinear" (default) or "nearest"
#'
#' @return A data.frame with columns: date, date_char, value, variable, gcm, scenario
get_point_data <- function(lat, lon, lakename, gcm, scenario, cmip_vars,
                           cmip6_files, cmip6_metadata, method = "bilinear") {

  sel_files <- cmip6_files[grepl(paste0("_", gcm, "_"), cmip6_files) &
                            grepl(paste0("_", scenario, "_"), cmip6_files) ]

  if (length(sel_files) == 0) {
    stop(paste("No files found for GCM:", gcm, "and scenario:", scenario))
  }

  missing <- sel_files[!file.exists(sel_files)]
  if (length(missing) > 0) {
    stop(paste(
      "Missing files for GCM:", gcm, "scenario:", scenario, "\n",
      paste(basename(missing), collapse = "\n ")
    ))
  }

  wide <- metscale::extract_climate_point(
    path        = sel_files,
    lon         = lon,
    lat         = lat,
    vars        = cmip_vars,
    experiments = scenario,
    method      = method,
    calendar    = "auto",
    verbose     = FALSE
  )

  tidyr::pivot_longer(
    wide,
    cols = -c(Date, experiment),
    names_to = "variable",
    values_to = "value",
    values_drop_na = TRUE
  ) |>
    dplyr::transmute(
      date      = Date,
      date_char = Date,
      value     = value,
      variable  = unname(.met_to_cmip_var[variable]),
      gcm       = gcm,
      scenario  = scenario
    )
}

#' Load a NetCDF raster file and convert temperature units if needed
#'
#' Reads a NetCDF file into a terra SpatRaster, inferring the variable name
#' and scenario from the filename. Temperature variables (as defined by
#' \code{KELVIN_VARS}) are converted from Kelvin to Celsius.
#'
#' @param file Path to a NetCDF file. The filename must follow the convention
#'   \code{<variable>_<scenario>_*.nc}, where the variable name is the first
#'   underscore-delimited token.
#'
#' @return A \code{SpatRaster} object. Temperature layers will be in Celsius;
#'   all other variables are returned in their native units.
#'
#' @seealso \code{\link{KELVIN_VARS}} for the list of variables that trigger
#'   unit conversion.
#'
#' @examples
#' \dontrun{
#' r <- load_nc("tas_ssp245_2015_2100.nc")
#' }
load_nc <- function(file) {
  if (!file.exists(file)) {
    stop(paste("File does not exist:", file))
  }
  
  file_parts <- strsplit(basename(file), "_")[[1]]
  if (length(file_parts) < 2) {
    stop(paste("Unexpected filename format:", basename(file)))
  }
  
  sel_var <- file_parts[1]
  r <- terra::rast(file)
  
  # Convert Kelvin to Celsius for temperature variables
  if (sel_var %in% KELVIN_VARS) {
    r <- r - 273.15
  }
  
  return(r)
}

#' Calculate mean annual precipitation from a daily or monthly raster
#'
#' For each year present in the raster's time dimension, sums all layers
#' within that year to produce an annual total. Then averages those annual
#' totals across all years to return a single-layer mean annual sum raster.
#'
#' @param r A \code{SpatRaster} with a time dimension set (i.e.
#'   \code{terra::time(r)} returns valid \code{Date} or \code{POSIXct}
#'   values). Layers are assumed to represent precipitation in consistent
#'   units (e.g. mm/day or kg/m²/s).
#'
#' @return A single-layer \code{SpatRaster} containing the mean annual
#'   precipitation sum, in the same units as the input layers summed over
#'   a year. The layer is unnamed; the spatial extent and CRS match the
#'   input.
#'
#' @details
#' The function subsets by matching the four-digit year string from
#' \code{terra::time(r)}, so it handles both daily and monthly inputs
#' without modification. Partial years at the start or end of the record
#' will produce lower annual sums and bias the mean downward — filter the
#' raster to complete years before calling this function if that matters
#' for your use case.
#'
#' @examples
#' \dontrun{
#' r <- terra::rast("pr_historical_1950_2014.nc")
#' mean_pr <- calc_pr_mean(r)
#' terra::plot(mean_pr, main = "Mean annual precipitation (mm)")
#' }
calc_pr_mean <- function(r) {
  time_vals <- terra::time(r)
  years <- unique(format(time_vals, "%Y"))
  
  annual_sums <- lapply(years, function(yr) {
    time_idx <- which(format(time_vals, "%Y") == yr)
    r_subset <- terra::subset(r, time_idx)
    terra::app(r_subset, fun = sum, na.rm = TRUE)
  })
  
  annual_stack <- terra::rast(annual_sums)
  names(annual_stack) <- years  # makes intermediate results inspectable
  
  mean_annual_sum <- terra::app(annual_stack, fun = mean, na.rm = TRUE)
  return(mean_annual_sum)
}

#' Read the time dimension from a NetCDF file
#'
#' Uses the \pkg{stars} package to read the time coordinate of a NetCDF
#' file and returns it as a vector of \code{Date} objects. This is a
#' workaround for cases where \code{terra::time()} does not correctly parse
#' non-standard calendars or CF-compliant time axes.
#'
#' @param ncfile Path to a NetCDF file containing a \code{time} dimension.
#'
#' @return A \code{Date} vector of length equal to the number of time steps
#'   in the file.
#'
#' @details
#' Time decoding relies on the internal \code{as_timestamp()} method of the
#' \code{CFtime} object stored in the \code{stars} dimension. This is
#' stars-internal API and may be subject to change across package versions.
#' If you encounter errors after upgrading \pkg{stars}, check whether the
#' \code{CFtime} interface has changed.
#'
#' @seealso \code{\link[stars]{read_ncdf}}, \code{\link[stars]{st_dimensions}}
#'
#' @examples
#' \dontrun{
#' dates <- get_nc_time("tas_ssp245_2015_2100.nc")
#' head(dates)
#' range(dates)
#' }
get_nc_time <- function(ncfile) {
  if (!file.exists(ncfile)) {
    stop(paste("File does not exist:", ncfile))
  }
  
  strs <- stars::read_ncdf(ncfile, var = "time")
  dims <- stars::st_dimensions(strs)
  
  if (is.null(dims$time)) {
    stop(paste("No 'time' dimension found in:", ncfile))
  }
  
  time <- dims$time$values$as_timestamp(format = "date", asPOSIX = FALSE)
  return(time)
}
