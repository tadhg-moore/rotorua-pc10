suppressMessages(library(dplyr)); cd <- targets::tar_read(chem_data); print(names(cd)); print(table(cd$StationName)); 
s <- cd |> mutate(Date = as.Date(Date)) |> filter(!is.na(`Secchi (m)`)); print(table(s$StationName)); print(range(s$Date)); print(table(format(s$Date, "%Y")))
print(grep("TN|TP|Nitrogen|Phosph", names(cd), value = TRUE))
