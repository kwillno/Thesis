# Creation and exporting of distance of sea field

# TOC
#   Load libraries
#   Load data
#   Create raster
#   Plot the distance field


# ------ Load libraries ------
library(terra)
library(sf)
library(ggplot2)

# ------ Load data ------
# Load domainfile
crs <- readRDS("../../data/objects/models/crs.rds")

# Load raster of altitude for simplicity
alt <- terra::rast("../../data/elevation/altitude.nc")


# ------ Create raster ------
# Find the sea
sea <- terra::rast(ext(alt), crs = crs, nrows = nrow(alt), ncols = ncol(alt))
sea$sea <- alt < 3    
# 3 is used here as a value "close enough" to zero. For the roughness of the domain this 
# should be sufficient. This will need to be adjusted for domains with large areas at
# very low elevations. 

# Expand border
sea <- extend(sea, ext(sea) + 50, fill = 1)

# Create polygon and remove attributes
edgeLines <- as.lines(as.polygons(sea))

# Find distance field in km
distance <- terra::distance(sea, edgeLines, unit = "km")

# Invert sea field and multiply by distance field to take only internal distance
distanceInt <- (-sea + 1) * distance

# Set correct name
names(distanceInt) <- c("distToSea")

# Save data objects
# Write the generated SpatRasters to file
writeCDF(distanceInt, filename = "../../data/objects/models/distSea.nc", overwrite = TRUE)
print("Raster written to file")

# ------ Plot the distance field ------

domain <- readRDS("../../data/objects/models/domain.rds")

dist_sf <- st_as_sf(
  cbind(
    terra::values(distanceInt, dataframe=T),
    terra::crds(distanceInt) ),
  coords = c("x","y"),
  crs = crs
)
dist_sf <- cbind(dist_sf, st_coordinates(dist_sf))


ggplot(st_intersection(dist_sf, domain)) +
  geom_sf(data = domain) +
  geom_tile(aes(x = X, y = Y, fill = distToSea)) +
  labs(x ="", y = "", fill = "Distance\nto Sea[km]") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
  )

ggsave(
  filename = "../../plots/descriptive/distToSea.png",
  plot = last_plot(),
  width = 8, height = 8
)
