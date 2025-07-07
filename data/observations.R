# Extract useful information from ECA homogeneous blended data set and 
# export this to a separate data file.

# TOC
#   Load libraries
#   Get stations within bounding box
#   Store data for relevant stations
#   Save data to file

# ----- Load libraries -----
library(sf)
library(stringr)

# ------ Get stations within bounding box ------

# Read data file
stations <- readLines("../../data/ECA/ECA_homblend_tg/stations.txt")

# Remove the preface
stations <- stations[18:length(stations)]

# Store stations in dataframe
df <- read.csv(text = paste(stations, collapse = "\n"))

# Define a function to convert coordinate values
degToFloat <- function(degString){
  # Function to convert coordinates in degrees:minutes:seconds to floats
  components <- str_split_1(degString, ":")
  floatValue <- as.numeric(components[1])
  
  for (i in 2:length(components)){
    floatValue <- floatValue + as.numeric(components[i])/ (60^(i-1))
  }
  
  return(round(floatValue, digits = 4))
}

# Set coordinate values to floats
for (i in 1:(nrow(df))) {
  df$LAT[i] <- degToFloat(df$LAT[i])
  df$LON[i] <- degToFloat(df$LON[i])
}

df$LAT <- as.numeric(df$LAT)
df$LON <- as.numeric(df$LON)

# Store stations in an sfc object
stations <- st_as_sf(df, coords = c('LON', 'LAT'), crs="+proj=latlon +datum=WGS84")

crs <- readRDS("../../data/objects/models/crs.rds")
stations <- st_transform(stations, crs = crs)

# Filter stations by domain
domain <- readRDS("../../data/objects/models/domain.rds")

relevantStations <- st_filter(stations, domain[1])


# ------ Store data for relevant stations ------

dataPath <- "../../data/ECA/dataset.nc"

if (!file.exists(dataPath)){
  # Initialise an empty dataframe to store observations in
  df <- data.frame()
  
  numberStations <- nrow(relevantStations)
  notFoundStations <- 0
  
  for (i in 1:nrow(relevantStations)){
    filename <- paste0("TG_STAID", str_pad(relevantStations[i,]$STAID, 6, pad = "0"),".txt")
    filepath <- paste0("../../data/ECA/ECA_homblend_tg/", filename)
    
    # Check if file exists
    if (file.exists(filepath)){
      station <- readLines(filepath)           # Read station file
      station <- station[20:length(station)]   # Remove file preface
      station <- read.csv(text = paste(station, collapse = "\n"))  # Store observations
      
      station$geometry <- stations$geometry[stations$STAID == station$STAID[1]]
      station$altitude <- relevantStations[i, ]$HGHT
      
      station <- st_as_sf(station, crs = crs)
      
      df <- rbind(df, station)
      
    } else {
      notFoundStations <- notFoundStations + 1 
    }
  }
  
  print(paste("Reported stations:", numberStations, ", stations missing files:", notFoundStations))
  
  
  # ------ Save data to file ------
  write_sf(obj = df, dsn = dataPath)
} else {
  # Read from file if data is found
  df <- read_sf(dataPath)
}

# To read this file use: 
#dfc <- read_sf("../../data/ECA/dataset.nc")
dfc <- read_sf(dataPath)

# Check that no information is lost
st_crs(df) == st_crs(dfc)
