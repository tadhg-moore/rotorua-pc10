extract_lake_level <- function(bop_lake_level_zip_folder) {
  # Unzip the folder to a temporary directory
  temp_dir <- tempdir()
  unzip(bop_lake_level_zip_folder, exdir = temp_dir)
  
  # List all CSV files in the unzipped directory
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Read and combine all CSV files into a single data frame
  # lake_level_data <- do.call(rbind, lapply(csv_files, read.csv))
  lake_level_data <- readr::read_csv(csv_files, skip = 3,
                                     col_names = c("DateTime", "Lake_Level_m", 
                                                   "qc_code"))
  
  lvl <- lake_level_data |> 
    dplyr::filter(!is.na(Lake_Level_m)) |>
    # dplyr::lake_level_data(-dplyr::n()) |> 
    dplyr::mutate(Date = as.Date(DateTime))
  
  med_level <- median(lvl$Lake_Level_m, na.rm = TRUE)
  
  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)
  
  return(lvl)
}
