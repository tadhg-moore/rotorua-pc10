# Convert pdf to tif
pdf_file <- here::here("data", "raw", "rotorua_bathy", "lakerotorua.pdf")
pdftools::pdf_data(pdf_file)

gdalUtilities::gdalinfo(pdf_file)

shp <- sf::st_read(pdf_file)

res <- 500
tif_file <- here::here("data", "raw", "rotorua_bathy", "lakerotorua.tif")
pdftools::pdf_convert(pdf_file, format = "tif", dpi = res, filenames = tif_file)

# gdalUtilities::gdal_translate(
#   pdf_file, tif_file,
#   of = "GTiff"
# )

# r <- terra::rast(pdf_file)
r <- terra::rast(tif_file)
r <- terra::flip(r, direction = "vertical")
plot(r)

unique(terra::values(r))


# terra::plot(r)

pts_pixel <- terra::click(r, n = 6, id = TRUE, xy = TRUE)
pts_pixel

gcps <- data.frame(
  px = pts_pixel[,1],
  py = pts_pixel[,2],
  X  = c(176.3333, 176.3333, 176.2333, 176.21667),
  Y  = c(-38.0333, -38.06667, -38.1333, -38.05)
)



