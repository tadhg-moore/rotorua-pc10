read_electro_fishing <- function(file, tz = "Etc/GMT-12") {
  fx <- readxl::read_excel(file) |>
    suppressMessages()
  
  fish_blocks <- list(`Common bully` = 1:4, Koura = 6:9, Trout = 11:14)
  
  fish <- fish_blocks |>
    purrr::imap(function(cols, sp) {
      fx |>
        dplyr::select(dplyr::all_of(cols)) |>
        purrr::set_names(c("date", "Site 1", "Site 2", "Site 3")) |>
        dplyr::mutate(date = as.Date(date)) |>
        tidyr::pivot_longer(
          cols = -date,
          names_to = "site",
          values_to = "cpue"
        ) |>
        dplyr::mutate(species = sp)
    }) |>
    purrr::list_rbind()
  return(fish)
}
