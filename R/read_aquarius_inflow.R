#' Read an Aquarius discharge export (instantaneous values).
#'
#' Reads a local BOPRC / Aquarius time-series csv and returns the instantaneous
#' record. Two layouts are recognised; the header row is found automatically:
#' * "EntireRecord" exports: `# ...` comment lines (identifier, location, units)
#'   followed by `ISO 8601 UTC, Timestamp (UTC+12:00), Value, Approval Level, Grade, Qualifiers`
#' * BOPRC "Bulk Export - Points as recorded" files: a few free-text lines (site id
#'   and name on lines 3-4) followed by `Timestamp (UTC+12:00), Value (m^3/s), Grade Code, Approval Level, ...`
#'
#' Timestamps are read on the fixed UTC offset given in the header (e.g. UTC+12:00
#' for NZST, no daylight saving), so `as.Date()` of the result gives the NZST day.
#'
#' @param file path to the csv
#' @param site_id,site_name override the labels taken from the file header
#' @return data.table with `time` (POSIXct), `q` (value, in the file's units),
#'   `approval`, `grade` and `approved` (TRUE for "Approved" / approval code 1200).
#'   Site labels and the units are stored as the attributes `site_id`,
#'   `site_name` and `unit`.
read_aquarius_inflow <- function(file, site_id = NULL, site_name = NULL) {
  head_lines <- readLines(file, n = 40, warn = FALSE)
  hdr <- grep("^(ISO 8601|Timestamp)", head_lines)[1]
  if (is.na(hdr)) stop("No 'ISO 8601' or 'Timestamp' header row found in ", file)
  meta <- head_lines[seq_len(hdr - 1)]
  hdr_cols <- strsplit(head_lines[hdr], ",")[[1]]

  # --- metadata -----------------------------------------------------------
  grab <- function(key) {
    m <- regmatches(meta, regexec(paste0("^#\\s*", key, ":\\s*(.+?)\\s*$"), meta))
    m <- Filter(length, m)
    if (length(m)) m[[1]][2] else NA_character_
  }
  if (any(grepl("^#\\s*Time-series identifier", meta))) {
    ts_id <- grab("Time-series identifier")                 # e.g. Discharge.Primary@EK378429
    id <- if (grepl("@", ts_id)) sub(".*@", "", ts_id) else ts_id
    nm <- grab("Location")
    unit <- grab("Value units")
  } else {
    cells <- vapply(strsplit(meta, ","), function(x) if (length(x) >= 2) trimws(x[2]) else "", "")
    cells <- cells[nzchar(cells)]
    id <- cells[1]; nm <- cells[2]
    unit <- sub(".*\\((.*)\\).*", "\\1", grep("^Value", hdr_cols, value = TRUE)[1])
  }
  if (is.null(site_id)) site_id <- id
  if (is.null(site_name)) site_name <- nm

  # --- data ---------------------------------------------------------------
  ts_col <- grep("^Timestamp", hdr_cols, value = TRUE)[1]
  val_col <- grep("^Value", hdr_cols, value = TRUE)[1]
  appr_col <- grep("^Approval", hdr_cols, value = TRUE)[1]
  grade_col <- grep("^Grade", hdr_cols, value = TRUE)[1]
  dat <- data.table::fread(file, skip = hdr - 1,
                           select = c(ts_col, val_col, appr_col, grade_col),
                           colClasses = list(character = c(ts_col, appr_col)))
  data.table::setnames(dat, c("time", "q", "approval", "grade"))
  dat <- dat[!is.na(q)]

  off <- as.numeric(sub(".*UTC([+-]\\d+).*", "\\1", ts_col))
  if (is.na(off)) stop("Could not read the UTC offset from the column name '", ts_col, "'")
  tz <- paste0("Etc/GMT", sprintf("%+d", -off))             # Etc/GMT-12 is UTC+12
  dat[, time := as.POSIXct(time, tz = tz)]
  dat[, approved := approval %in% c("Approved", "1200")]
  data.table::setorder(dat, time)

  attr(dat, "site_id") <- site_id
  attr(dat, "site_name") <- site_name
  attr(dat, "unit") <- unit
  dat[]
}

#' Aggregate an instantaneous inflow record to a coarser interval.
#'
#' Intervals are calendar-based in the record's own (fixed offset) time zone.
#' An interval is kept only if enough of it has data: `min_coverage` is the
#' minimum fraction of the interval's clock hours that contain at least one
#' record (the default 20/24 reproduces "at least 20 hours of data" for a day).
#' This drops the partial intervals at the ends of the record and at gaps.
#'
#' @param x output of [read_aquarius_inflow()]
#' @param interval one of "daily" (default), "hourly", "weekly" (weeks start Monday),
#'   "monthly" or "annual"
#' @param min_coverage minimum fraction of hours with data, between 0 and 1
#' @return tibble with the interval start (`time` for hourly, otherwise `date`),
#'   `q_mean`, `q_max`, `q_min`, `n_obs`, `coverage` and `approved` (TRUE if
#'   every record in the interval is approved); site labels are added if the
#'   input carries them
aggregate_inflow <- function(x, interval = c("daily", "hourly", "weekly", "monthly", "annual"),
                             min_coverage = 20 / 24) {
  interval <- match.arg(interval)
  tz <- attr(x$time, "tzone")
  unit <- c(hourly = "hour", daily = "day", weekly = "week", monthly = "month", annual = "year")[[interval]]

  d <- data.table::copy(data.table::as.data.table(x))
  d[, start := lubridate::floor_date(time, unit, week_start = 1)]
  d[, hr := floor(as.numeric(time) / 3600)]

  out <- d[, .(q_mean = mean(q), q_max = max(q), q_min = min(q), n_obs = .N,
               n_hours = data.table::uniqueN(hr), approved = all(approved)), by = start]
  out[, end := lubridate::ceiling_date(start, unit, week_start = 1, change_on_boundary = TRUE)]
  out[, coverage := n_hours / as.numeric(difftime(end, start, units = "hours"))]
  out <- out[coverage >= min_coverage][order(start)]
  out[, c("n_hours", "end") := NULL]

  if (interval == "hourly") {
    data.table::setnames(out, "start", "time")
  } else {
    out[, start := as.Date(start, tz = tz)]
    data.table::setnames(out, "start", "date")
  }
  out <- tibble::as_tibble(out)
  if (!is.null(attr(x, "site_id"))) {
    out <- dplyr::mutate(out, site_id = attr(x, "site_id"), site_name = attr(x, "site_name"), .before = 1)
  }
  attr(out, "unit") <- attr(x, "unit")
  out
}
