# This file is for downloading and treating the reanalysis data

# TOC
#   Load libraries
#   Load data
#   Download data
#   Reading and combining reanalysis
#   Write the combined SpatRaster to file

# ----- Load libraries ----- 
library(stringr)
library(sf)
library(terra)

# ----- Load data -----

crs <- readRDS("../../data/objects/models/crs.rds")
domain <- readRDS("../../data/objects/models/domain.rds")

ECA <- st_transform(read_sf("../../data/ECA/ECA.nc"), crs)

startYear <- min(ECA$year)
endYear <- max(ECA$year)

# ----- Downloading data -----

filePathBase <- "../../data/NORA3/"

baseURL <-paste0("https://thredds.met.no/",
                 "thredds/fileServer/force_derived_products/",
                 "met_nordic_analysis/long_term_consistent/version_1/",
                 "climate_indices/average/")


for (year in startYear:endYear){
  for (month in 1:12){
    print(paste("Downloading year", year,"month", month))
    
    fileURL <- paste0(baseURL, year, "/met_analysis_ltc_1_0km_nordic_tgmean_mon_", year, str_pad(string = month, width = 2, pad = 0), ".nc")
    fileDestination <- paste0(filePathBase, "raw/", year, "-", str_pad(string = month, width = 2, pad = 0),".nc")
    
    if (!file.exists(fileDestination)){
      # Download file
      download.file(
        url = fileURL,
        destfile = fileDestination,
        method = "curl"
      )
    }
  }
}



# ----- Reading and combining reanalysis -----

# Load all reanalysis files
files <- list.files(path = paste0(filePathBase, "raw"), full.names = T)

# Add each month as a seperate layer
reanalysis <- terra::rast(files[1])
names(reanalysis[[1]]) <- str_split_i(str_split_i(files[1], "/", 6), ".nc", 1)
for (i in 2:length(files)){
  print(paste("Adding file", i, "of", length(files)))
  
  # Add each new file as a new layer to same SpatRaster
  add(reanalysis) <- rast(files[i])
  
  names(reanalysis[[i]]) <- str_split_i(str_split_i(files[i], "/", 6), ".nc", 1)
}


# Reproject
reanalysis <- terra::project(reanalysis, crs)

# Crop to domain
reanalysis <- terra::crop(reanalysis, domain)

# ----- Write the combined SpatRaster to file -----
# Save terra object
resultPath <- paste0(filePathBase, "reanalysis.tif")
terra::writeRaster(reanalysis, resultPath, gdal = "COMPRESS=NONE", overwrite = TRUE) 

# Save as an sf object
reaPath <- "../../data/NORA3/rea_sf.rds"
rea_sf <- st_as_sf(as.data.frame(crds(reanalysis)), coords = c("x", "y"),  crs = crs)

# Store data on coordinate grid
for (i in 1:ncol(values(reanalysis))){
  print(paste("Adding column", i, "of", ncol(values(reanalysis))))
  rea_sf[[as.character(i)]] <- values(reanalysis)[,i]
}

saveRDS(rea_sf, file = reaPath)
