# Create realisations of model

# TOC:
#   Libraries
#   Load data
#   Make predictions
#   Save predictions


# Load libraries
library(sf)
library(terra)
library(inlabru)
library(fmesher)
library(ggplot2)
library(dplyr)
library(stringr)



modelName <- "altRW2"

# ------ Load data ------

# Mesh data
crs <- readRDS("../../../data/objects/models/crs.rds")
domain <- readRDS("../../../data/objects/models/domain.rds")
mesh <- readRDS("../../../data/objects/models/mesh.rds")

# Observation data
ECA <- st_transform(read_sf("../../../data/ECA/ECA.nc"), crs)

# Altitude field
alt <- rast("../../../data/elevation/altitude.nc")

# Reanalysis
reaPath <- "../../../data/NORA3/rea_sf.rds"

if (!file.exists(reaPath)){
  # Reanalysis
  reanalysis <- terra::rast("../../../data/NORA3/reanalysis.tif")
  
  # Create sf object with coordinates
  coords <- st_as_sf(as.data.frame(crds(reanalysis)), coords = c("x", "y"),  crs = crs)
  
  # Create sf object of reanalysis data
  rea_sf <- st_as_sf(as.data.frame(crds(reanalysis)), coords = c("x", "y"),  crs = crs)
  
  for (i in 1:ncol(values(reanalysis))){
    rea_sf[[as.character(i)]] <- values(reanalysis)[,i]
  }
} else {
  rea_sf <- readRDS(reaPath)
  coords <- st_intersection(subset(rea_sf, select = c("geometry")), domain)
}



# Load models
modelList <- readRDS(file = "../../../data/objects/models/altRW2/models.rds")


# ----- Make predictions -----

# Initialise empty dataframe
modelPredictions <- data.frame()

for (i in 1:12){
  print(paste("Month", i , "of", 12))
  # if(i == 3){next} # Having errors with the model object for march
  
  # Generate grid
  predictionGrid <- fm_cprod(coords, data.frame(year = 1:38))
  
  # Make prediction
  modelPrediction <- predict(
    object = modelList[[i]],
    newdata = predictionGrid,
    ~ Intercept + alt + time_rw + space
  )
  
  # Add month to data object
  modelPrediction <- cbind(
    modelPrediction,
    data.frame(month = i)
  )
  
  # Store predictions for this month
  print(paste("Saving modelPrediction for month", i, "of size", format(object.size(modelPrediction), units = "Mb")))
  saveRDS(modelPrediction, file = paste0("../../../data/objects/predictions/", modelName, "/predMonth", str_pad(i, 2, pad = "0"),".rds"))
  print(paste("Prediction for month", i, "stored successfully."))
  
}

# Notify that code is finished.
print("Predictions saved. Finished gracefully.")
