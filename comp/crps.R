# Calculation of CRPS

# TOC:
#   Libraries
#   Load Data
#   Make Computations
#   Mean scores

# Set globals
modelName <- "distStat"
testing <- FALSE

maxAlt <- 1000

# Want plots of some years. 
# Choose some to represent timeframe
plotYears <- c(1990, 1995, 2000, 2010, 2018)

# Set labels in plots to monthNames
months <- readRDS("../../data/objects/months.rds")
monthLabels <- function(index){
  return(list(months))
}



# ------ Libraries ------
library(sf)
library(terra)
library(inlabru)
library(verification)
library(stringr)
library(dplyr)
library(ggplot2)

# ------ Load Data ------

# Domain and mesh
crs <- readRDS("../../data/objects/models/crs.rds")
domain <- readRDS("../../data/objects/models/domain.rds")
mesh <- readRDS("../../data/objects/models/mesh.rds")

# NORA3 Data
rea_sf <- readRDS("../../data/NORA3/rea_sf.rds")

# Counties
counties <- readRDS("../../data/shapes/counties.rds")

# ------ Remove points over max altitude ------

# Find all coordinate points within the domain
coords <- st_intersection(subset(rea_sf, select = c("geometry")), domain)

# Get altitude of these coordinates
alt <- rast("../../data/elevation/altitude.nc")
coords <- cbind(coords, extract(alt, terra::vect(coords)))

# Filter coords below given altitude
maxAlt <- coords %>% 
  filter(altitude < maxAlt) %>%
  select(altitude, geometry)

# ------ Make computations ------

numberOfMonths <- 12

timedf <- data.frame()   # Timeseries for entire domain
timedfc <- data.frame()  # Timeseries for each county
df <- data.frame()
for (k in 1:6){
  print(paste("Working on block", k, "of 6"))
  
  # Make calculations for blocks of years at a time
  # This syntax is obtuse, but works. I promise.
  years <- (1988:2018)[1:6 + 6*(k-1)]
  years <- years[!is.na(years)]
  
  print(paste("Working on years", years))
  
  # Transpose NORA3 data to get data by month
  print("Transposing NORA3 data")
  reaMonth <- data.frame()
  for (i in 1:12){
    print(paste("Working on month", i))
    for (j in years) { 
      rows <- st_as_sf(data.frame(
        year = j,
        month = i,
        geometry = rea_sf$geometry,
        rea_t2m = rea_sf[[paste0(i + (j-1 - 1980)*12)]] # -1 needed for indexing to be correct. -1980 needed for indexing to be correct
      ))
      reaMonth <- rbind(reaMonth, rows)
    }
  }
  
  scores <- data.frame()
  # Load predictions by month and make score computations
  for (i in 1:numberOfMonths){
    
    print("Loading prediction...")
    # Load prediction
    pred <- readRDS(file = paste0("../../data/objects/predictions/", modelName,"/predMonth", str_pad(i, 2, pad=0),".rds"))
    
    # Use only necessary parts of the prediction
    pred <- pred %>%
      filter(year %in% years) %>%
      select(predMean = mean, predSD = sd, year = year, month = month, geometry = geometry) %>%
      arrange(geometry)
    
    # Filter NORA3 by month
    reaMonthly <- reaMonth %>%
      filter(month == i) %>% 
      arrange(geometry)
    
    # Join predictions and NORA3 to get same order
    print("Joining...")
    comp <- st_join(pred, reaMonthly)
    comp <- comp %>%
      filter(year.x == year.y, month.x == month.y) %>%
      mutate(year = year.x, month = month.x) %>%
      mutate(year.x = NULL, year.y = NULL, month.x = NULL, month.y = NULL) %>%
      filter(geometry %in% maxAlt$geometry)  # Filter locations above maxAlt 
    
    
    # Calculate the CRPS
    CRPSobs  <- comp$rea_t2m
    CRPSpred <- data.frame(mean = comp$predMean, sd = comp$predSD)
    
    # Report score
    score <- crps(CRPSobs, CRPSpred)
    #print(paste("Mean CRPS:", score$CRPS, "Mean ignorance score:", score$IGN))
    
    # Add score contribution to data frame
    comp$crps <- score$crps
    comp$ign <- score$ign
    
    
    # Save 
    scores <- rbind(
      scores,
      data.frame(
        year = comp$year,
        month = comp$month,
        crps = comp$crps,
        ign = comp$ign,
        geometry = comp$geometry
      )
    )
    
    # ----- Timeseries -----
    
    # Calculate averages for timeseries
    timeRow <- comp %>%
      group_by(month, year) %>%
      summarise(
        crps = mean(crps),
        ign = mean(ign)
      ) 
    
    # Save averages for timeseries
    timedf <- rbind(
      timedf,
      data.frame(
        year = timeRow$year,
        month = timeRow$month,
        crps = timeRow$crps,
        ign = timeRow$ign
      )
    )
    
    # Save for individual counties 
    for (l in 1:nrow(counties)){
      timeRowCounty <- comp %>%
        st_intersection(counties[l,]) %>%
        group_by(month, year) %>%
        summarise(
          crps = mean(crps),
          ign = mean(ign),
          county = unique(navn)
        ) 
      
      timedfc <- rbind(
        timedfc,
        data.frame(
          year = timeRowCounty$year,
          month = timeRowCounty$month,
          county = timeRowCounty$county,
          crps = timeRowCounty$crps,
          ign = timeRowCounty$ign
        )
      )
    }
  }
  
  # ------ Mean scores ------
  
  # Calculate mean crps and ign score
  CRPS <- scores %>%
    group_by(month) %>%
    summarise(CRPS = mean(crps), IGN = mean(ign), n = n(), minYear = min(year), maxYear = max(year)) 
  
  # Show and save mean CRPS
  print(CRPS)
  
  # Store CRPS 
  df <- rbind(df, CRPS)
  
  
  # Create plots if there is overlap between years and plotYears
  if (sum(years %in% plotYears) > 0) { 
    # Cast scores as sf object
    scores <- st_as_sf(scores)
    
    # Find which year(s) is a plotYear
    plotYear <- years[years %in% plotYears]
    print(paste("Plotting for", plotYear))
    
    # Extract coordinates for plotting
    xy <- st_coordinates(scores)
    scores <- cbind(scores, xy)
    
    # Loop over the (multiple) plotYears
    # if multiple plotYears are captured in same block.
    for (j in 1:length(plotYear)){ 
      
      # Plot score contributions
      ggCRPS <- ggplot(scores %>% filter(year == plotYear[j])) + 
        geom_sf(data = domain) +
        geom_tile(aes(x = X, y = Y, fill = crps)) +
        scale_fill_viridis_c(limits = c(0, 8)) +
        facet_wrap(~month, labeller = "monthLabels") +
        labs(x = "", y = "", fill = "CRPS") +
        theme(
          legend.title = element_text(size = 20),
          legend.key.height = unit(2, "in"),
          legend.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          strip.text = element_text(size = 18)
        )
      
      ggsave(
        paste0("../../plots/comparison/", modelName, "/CRPS", plotYear[j], ".png"),
        plot = ggCRPS, width = 12, height = 14) 
      
    }
  }
}

# ------ Assemble result ------

# Taking weighted averages to assemble final score
result <- df %>%
  group_by(month) %>%
  summarise(crps = sum(CRPS*n)/sum(n), ign = sum(IGN*n)/sum(n),)
  
print(result)
# Save scores
saveRDS(result, file=paste0("../../data/objects/predictions/", modelName, "/crps.rds"))

# Save timeseries
saveRDS(timedf, file=paste0("../../data/objects/comps/", modelName, "/crps-time.rds"))
saveRDS(timedfc, file=paste0("../../data/objects/comps/", modelName, "/crps-timeCounty.rds"))


# Notify that code is finished.
print("Comparison scores saved. Finished gracefully.")
