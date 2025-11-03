get_aeme_rds <- function(lid = 11133) {
  aeme <- aemetools::get_aeme(id = lid, api_key = Sys.getenv("LERNZMP_API"))
  lke <- AEME::lake(aeme)
  out_file <- here::here("data", "aeme", paste0(lke$id, "_",
                                                    tolower(lke$name), ".rds"))
  saveRDS(aeme, out_file)
  return(out_file)
}
