# Read a multi-dataset BOPRC environmental data portal bulk export (CSV) into a long table.
# The export has a title and a disclaimer, then rows of site code, site name, dataset name and a column header, then one wide row per timestamp:
# Timestamp, then (Value, Grade Code, Approval Level, Qualifiers) for each dataset. A disclaimer line follows the data.
read_bop_lab_csv_url <- function(url) {
  res <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  lns <- strsplit(res, "\r?\n")[[1]]
  hdr <- utils::read.csv(text = lns[3:6], header = FALSE, colClasses = "character", na.strings = NULL)
  ds <- which(hdr[1, ] != "")   # first column of each dataset block
  info <- data.frame(
    site_code = unlist(hdr[1, ds]), site_name = unlist(hdr[2, ds]),
    dataset = unlist(hdr[3, ds]), unit = sub("^Value \\((.*)\\)$", "\\1", unlist(hdr[4, ds])),
    start = ds
  )

  raw <- readr::read_csv(I(paste(lns[-(1:6)], collapse = "\n")), col_names = FALSE, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  raw <- raw[grepl("^\\d{4}-\\d{2}-\\d{2}", raw[[1]]), ]   # drops the trailing disclaimer
  ts <- lubridate::as_datetime(raw[[1]], tz = "Etc/GMT-12")

  out <- lapply(seq_len(nrow(info)), \(i) {
    j <- info$start[i]
    tibble::tibble(
      Timestamp = ts, site_code = info$site_code[i], site_name = info$site_name[i],
      variable = sub("\\.LabResult$", "", info$dataset[i]), unit = info$unit[i],
      value = suppressWarnings(as.numeric(raw[[j]])), grade_code = raw[[j + 1]],
      approval_level = raw[[j + 2]], qualifiers = raw[[j + 3]]
    )
  })
  dplyr::bind_rows(out) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::arrange(variable, Timestamp)
}
