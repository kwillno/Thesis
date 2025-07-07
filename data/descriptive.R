# Creation of descriptive plots

# TOC
#   Load libararies
#   Load data
#   Helping functions
#   Descriptive plots
#   Stations
#   Availability of data
#   Year-to-Year variation
#   Reanalysis


# Load libraries
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(inlabru)
library(stringr)

# ------ Load datasets ------
crs <- readRDS("../../data/objects/models/crs.rds")
domain <- readRDS("../../data/objects/models/domain.rds")

ECA <- st_transform(read_sf("../../data/ECA/ECA.nc"), crs)
altitude <- rast("../../data/elevation/altitude.nc")
alt_sf <- st_as_sf(
            cbind(
              values(altitude, dataframe=T),
              crds(altitude) ),
            coords = c("x","y"),
            crs = crs
          )
alt_sf <- cbind(alt_sf, st_coordinates(alt_sf))


months <- readRDS("../../data/objects/months.rds")

# ------ Helping functions ------

monthLabels <- function(index){
  return(list(months))
}

# Remove years with incomplete data
ECA <- ECA %>% filter(year <= 2018)


ECA_monthly <- ECA %>% group_by(STAID, year, month) %>% group_by(STAID, year) %>% filter(n() < 12) %>% select(STAID, year, month)


# ------ Descriptive plots ------

# Find maximal height of domain
altMax <- max(alt_sf$altitude)

ggplot(ECA, aes(x = year, y = monthlyMean, color = altitude)) + 
  geom_point() +
  scale_x_continuous(breaks = c(1981, 2000)) +
  scale_color_viridis_c(limits = c(0, altMax)) +
  facet_wrap(~month, labeller = "monthLabels") +
  labs(color = "Altitude") + 
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18)
  )
ggsave(
  filename = "../../plots/descriptive/scatter.pdf",
  plot = last_plot(),
  width = 8, height = 8
)

# Plot scatter with cutoff at 1988
ggplot(ECA %>% filter(year >= 1988), aes(x = year, y = monthlyMean, color = altitude)) + 
  geom_point() +
  scale_x_continuous(breaks = c(1988, 2005)) +
  scale_color_viridis_c(limits = c(0, altMax)) +
  facet_wrap(~month, labeller = "monthLabels") +
  labs(color = "Altitude") + 
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18)
  )
ggsave(
  filename = "../../plots/descriptive/scatterCut.pdf",
  plot = last_plot(),
  width = 8, height = 8
)



ggplot() +
  geom_tile(data = st_intersection(alt_sf, domain), mapping = aes(x = X, y = Y, fill = altitude)) +
  scale_fill_viridis_c() +
  geom_sf(data = ECA %>% group_by(STAID) %>% summarise(), color = "red", shape = 18, size = 5) +
  labs(fill = "Altitude", x = "", y = "") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(1, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
  )

ggsave(
  filename = "../../plots/descriptive/stations.pdf",
  plot = last_plot(),
  width = 8, height = 8
)

# ------ Stations ------

staAlts <- ECA %>%
  group_by(STAID) %>%
  summarise(altitude = mean(altitude)) %>%
  arrange(STAID)

# Read data file
stations <- readLines("../../data/ECA/ECA_homblend_tg/stations.txt")
stations <- stations[18:length(stations)] # Remove the preface
stations <- read.csv(text = paste(stations, collapse = "\n")) # Store stations in dataframe

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
for (i in 1:(nrow(stations))) {
  stations$LAT[i] <- degToFloat(stations$LAT[i])
  stations$LON[i] <- degToFloat(stations$LON[i])
}

stations <- stations[stations$STAID %in% staAlts$STAID, ]


stationsTex <- ""

lines <- c()
for (i in 1:nrow(stations)){
  row <- paste0(
      str_pad(stations[i,]$STANAME, width = 25, side = "right", pad = " "), # Name of station
      " & ",
      str_pad(stations[i,]$STAID, width = 10, pad = " "), # Station ID
      " & ",
      str_pad(stations[i,]$HGHT, width = 6, pad = " "), # Station elevation
      " & ",
      paste0(str_pad(stations[i,]$LON, width = 7, pad =" "), "°E"), # Longitude
      " & ",
      paste0(str_pad(stations[i,]$LAT, width = 7, pad =" "), "°N"), # Latitude
      "\\\\ "
    )
  
  lines <- c(lines, row)
}

fileConnection <- file("../../doc/Appendices/stationsRaw.tex")
writeLines(lines, fileConnection)
close(fileConnection)

ggplot(staAlts) + 
  geom_histogram(aes(x = altitude)) + 
  scale_x_continuous(breaks = c(0, 500, 1000, 1400)) + 
  scale_y_continuous(breaks = c(1,3,6,8,10)) + 
  labs(x = "Altitude [masl]", y = "Count") + 
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18)
  )
ggsave(file = "../../plots/descriptive/stations_hist.pdf", width = 8, height = 8)



# ------ Availability of data ------

ggplot(ECA %>% group_by(month) %>% summarise(n = n()),
       aes(x = month, y = n),) +
  geom_line()

ggplot(ECA_monthly,
       aes(x = year, y = as.factor(STAID))) + 
  geom_line()+ 
  facet_wrap(~month, labeller = "monthLabels")


ggplot(ECA %>%                              # Of the stations that are missing data for at least one month
         group_by(STAID, year, month) %>%   # how many months does it still have data for
         group_by(STAID, year) %>%
         filter(n() < 12) %>%
         select(STAID, year) %>%
         mutate(n = n()),
       aes(x = n)) +
  geom_histogram(bins = 11)

# ------ Year-to-year variation ------

ECA_variation <- ECA %>% 
                  group_by(year) %>%
                  summarise(tn = min(monthlyMean),
                            tg = mean(monthlyMean),
                            tx = max(monthlyMean),
                            spread = max(monthlyMean) - min(monthlyMean)
                            ) 

ggplot(ECA_variation, aes(x = year)) +
  geom_line(aes(y = tn)) +
  geom_line(aes(y = tg)) +
  geom_line(aes(y = tx))

ggplot(ECA_variation, aes(x = year, y = spread)) + geom_line()

# ------ Reanalysis ------

rea_sf <- readRDS(file = "../../data/NORA3/rea_sf.rds")

years <- c(1995)
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


reaMonth <- cbind(reaMonth, st_coordinates(reaMonth))

ggplot(st_intersection(reaMonth, domain)) +
  geom_sf(data = domain) +
  geom_tile(aes(x = X, y = Y, color = rea_t2m)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") + 
  facet_wrap(~ month, labeller = monthLabels) +
  labs(x = "", y = "", color = "Temperature") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(2, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18, face = "bold")
  )

ggsave(
  filename = "../../plots/descriptive/NORA3.png",
  plot = last_plot(),
  width = 12, height = 14  
)
  
