# Aggregation of ECA data

# TOC
#   Load libraries
#   Aggregate data by month
#   Store aggregated data

# Load libraries
library(sf)
library(stringr)
library(dplyr)
library(ggplot2)

# ------ Aggregate data by month ------

# Read raw data
df <- read_sf("../../data/ECA/dataset.nc")

# Add separate columns for month and year
df$year <- as.numeric(str_split_i(as.character(df$DATE), pattern = "[0-9]{4}$", i = 1))
df$month <- as.numeric(str_split_i(str_split_i(as.character(df$DATE), pattern = "[0-9]{2}$", i = 1), pattern = "^[0-9]{4}", i = 2))
df$date <- as.numeric(str_split_i(as.character(df$DATE), pattern = "^[0-9]{6}", i = 2))

# Filter and aggregate data by month
dfc <- df %>% 
  filter(TG > -9990) %>%                           # Remove datapoints with unknown temperatures
  filter(1980 < year & year < 2019) %>%            # Include data only for years where data and reanalysis has coverage
  group_by(STAID, altitude, year, month) %>%       
  mutate(dateLag = lag(date)) %>%                  # Find if any jumps are larger than or equal to 5 days
  mutate(diff = date - dateLag) %>%
  filter(diff < 5) %>%
  group_by(STAID, altitude, year, month) %>%
  filter(n() >= (31-10) ) %>%                      # Remove entries with missing data
  summarise(monthlyMean = round(mean(TG)/10, digits = 2), n = n())
  # Get mean of each month transformed to degrees Celsius, count datapoint used in each calculation

# Check if some years have systematic missing data
ggplot(dfc %>% filter(year <= 1990, year >= 1985),
       aes(x = year, y = monthlyMean, color = n)) +
  geom_point() + 
  facet_wrap(~month)

ggplot(dfc %>% filter(year <= 1990, year >= 1985, month == 1),
       aes(x = year, y = monthlyMean, color = n)) +
  geom_point()



# ------ Store aggregated data ------

write_sf(obj = dfc, dsn = "../../data/ECA/ECA.nc")

# Diagnostic plot
ggplot(dfc, aes(x = year, y = monthlyMean, color = altitude)) +
  geom_point() +
  facet_wrap(~month)
