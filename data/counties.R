# File for filtering and simplifying county objects

# Libraries
library(sf)


# ----- Read data -----

domain <- readRDS("../../data/objects/models/domain.rds")
crs <- readRDS("../../data/objects/models/crs.rds")

counties <- st_transform(read_sf("../../data/shapes/Fylker.geojson"), crs = crs)

# ----- Filter data ------

# Choose only the counties that intersect the domain
counties <- counties[c(st_intersects(counties, domain, sparse = FALSE)), ]

# ----- Simplify data -----

# Simplify
countiesSimple <- st_simplify(counties, dTolerance = 2)

# Store simplified object
saveRDS(countiesSimple, file = "../../data/shapes/counties.rds")

