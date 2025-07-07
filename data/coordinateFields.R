# Create and export coordinate fields

# TOC
#   Load libraries
#   Load data
#   Create rasters
#   Save raster objects
#   Transform and plot rasters


# ------ Load libraries ------
library(terra)
library(sf)
library(ggplot2)

# ------ Load data ------

# Load raster of altitude for simplicity
alt <- rast("../../data/elevation/altitude.nc")
crs <- readRDS("../../data/objects/models/crs.rds")
domain <- readRDS("../../data/objects/models/domain.rds")

# ------ Create rasters ------
# Coordinate value rasters
easting <- rast(ext(alt), crs = crs, nrows = nrow(alt), ncols = ncol(alt))
northing <- rast(ext(alt), crs = crs, nrows = nrow(alt), ncols = ncol(alt))

# Get coordinate values
coords <- crds(easting)

# Calculate the easting and northing fields
values(easting) <- (coords[,1] - mean(coords[,1]))/sd(coords[,1])
values(northing) <- (coords[,2] - mean(coords[,2]))/sd(coords[,2])

# Set field names
names(easting) <- c('east')
names(northing) <- c('north') 

# ------ Save raster objects ------

# Save data objects
# Write the generated SpatRasters to file
writeCDF(easting, filename = "../../data/objects/models/east.nc", overwrite = TRUE)
writeCDF(northing, filename = "../../data/objects/models/north.nc", overwrite = TRUE)
print("Raster written to file")


# ------ Transform and plot rasters ------

# Load and transform Easting
easting <- rast("../../data/objects/models/east.nc")
easting <- cbind(as.data.frame(easting), crds(easting))
easting <- st_as_sf(easting, coords = c("x","y"), crs = crs)
easting <- st_intersection(easting, domain)

xy <- st_coordinates(easting)
easting <- cbind(easting, xy)

# Plot easting
ggplot() +
  geom_sf(data = domain) + 
  geom_tile(data = easting, aes(x = X, y = Y, fill = east)) +
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "Value") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
  )

ggsave(
  filename = paste0("../../plots/descriptive/ew.png"),
  plot = last_plot(),
  width = 6, height = 6
)

# Load and transform Northing
northing <- rast("../../data/objects/models/north.nc")
northing <- cbind(as.data.frame(northing), crds(northing))
northing <- st_as_sf(northing, coords = c("x","y"), crs = crs)
northing <- st_intersection(northing, domain)

xy <- st_coordinates(northing)
northing <- cbind(northing, xy)

# Plot northing
ggplot() +
  geom_sf(data = domain) + 
  geom_tile(data = northing, aes(x = X, y = Y, fill = north)) +
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "Value") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
  )

ggsave(
  filename = paste0("../../plots/descriptive/ns.png"),
  plot = last_plot(),
  width = 6, height = 6
)
