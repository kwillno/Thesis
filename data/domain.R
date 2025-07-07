# File for generation of domain and mesh

# TOC
#   Load libraries
#   Set object file paths
#   Generate files
#   Plot mesh

# ------ Load libraries ------
library(sf)
library(fmesher)
library(ggplot2)

# ------ Set object file paths ------
pathCRS <- "../../data/objects/models/crs.rds"
pathDomain <- "../../data/objects/models/domain.rds"
pathMesh <- "../../data/objects/models/mesh.rds"

# Only generate if files are not found on disk
if (!file.exists(pathCRS) || !file.exists(pathDomain) || !file.exists(pathMesh)){
  ### ------ Domain settings ------
  
  # Set destination projection 
  crs <- "+proj=utm +zone=33 +datum=WGS84 +units=km +no_defs +type=crs"
  
  # Set bounding box of domain
  domain_bound <- st_sfc(
    st_polygon(
      list(
        rbind(
          c(4,57.8),
          c(13, 57.8),
          c(13,64),
          c(4, 64),
          c(4,57.8)
        )
      )
    ),
    crs = "+proj=latlon +datum=WGS84"
  )
  domain_bound <- st_transform(domain_bound, crs = crs)
  
  norwayShape <- st_transform(
    st_union(
      read_sf("../../data/shapes/Fylker.geojson")
    ),
    crs = crs
  )
  norwayShapeSimple <- st_simplify(norwayShape, dTolerance = 8)
  shape_boundry <- st_intersection(domain_bound, norwayShapeSimple)
  
  # Get intersection between shapefile and bound
  domain <- st_intersection(domain_bound, shape_boundry)
  domain <- st_transform(domain, crs = crs)
  
  # Generate triangular mesh
  mesh <- fm_mesh_2d(
    boundary = domain,
    max.edge = c(30, 50),
    offset = c(50, 150),
    crs = crs
  )
  
  ### ------ Store domain files in object storage ------
  saveRDS(crs, file = pathCRS)        # CRS
  saveRDS(domain, file = pathDomain)  # Domain Shape
  saveRDS(mesh, file = pathMesh)      # Domain Mesh

} else {
  crs <- readRDS(pathCRS)
  domain <- readRDS(pathDomain)
  mesh <- readRDS(pathMesh)
}

# ------ Plot mesh -----
ggplot() +
  geom_sf(data = domain) +
  inlabru::gg(data = mesh) +
  labs(x = "", y = "") +
  theme(
    axis.text = element_text(size = 14)
  )

ggsave("../../plots/descriptive/mesh.png", plot = last_plot(),
       height = 6, width = 5)

