# Merge altitude fields and export single object

# TOC
# 	Load Libraries
# 	Load geo settings
# 	Reproject and crop


# ------ Load libraries ------
library(terra)
library(sf)

# ------ Load geo settings ------
crs <- readRDS("../../data/objects/models/crs.rds")
domain <- readRDS("../../data/objects/models/domain.rds")
mesh <- readRDS("../../data/objects/models/mesh.rds")

dataPath <- "../../data/elevation/met_analysis_ltc_1_0km_nordic_20240101T19Z.nc"
altitude <- rast(dataPath, lyrs=c("altitude"))


# ------ Reproject and crop ------
altitude <- terra::project(altitude, crs)

altitude <- crop(altitude, vect(domain))


# Write the generated SpatRaster to new file
writeCDF(altitude, filename = "../../data/elevation/altitude.nc", overwrite = TRUE)
print("Raster written to file")
