# Construction of time plots

# TOC
#   Libraries
#   Load data
#   Generate Plots


# ----- Libraries -----
library(sf)
library(ggplot2)


# ----- Load data -----

# Monthnames
months <- readRDS("../../data/objects/months.rds")
monthLabels <- function(index){
  return(list(months))
}

# BIAS TIMESERIES
# Timeseries for entire domain
biasdf <- readRDS("../../data/objects/comps/distStat/time.rds")

# Timeseries by county
biasdfc <- readRDS("../../data/objects/comps/distStat/timeCounty.rds")

# CRPS TIMESERIES
# Timeseries for entire domain
crpsdf <- readRDS("../../data/objects/comps/distStat/crps-time.rds")

# Timeseries by county
crpsdfc <- readRDS("../../data/objects/comps/distStat/crps-timeCounty.rds")


# Get county names
counties <- unique(biasdfc$county)

# ----- Generate plots -----

# BIAS COMMON
ggtime <- ggplot(biasdf, aes(x = year)) +
  geom_point(aes(y = bias, color = !(biasU*biasL < 0)) , size = 2) +   # Set color based on significance
  geom_errorbar(aes(ymin = biasL, ymax = biasU, color = !(biasU*biasL < 0)), width = 1, linewidth = 0.8) +
  scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  #scale_color_viridis_d(option = "C") + 
  facet_wrap(~month, labeller = "monthLabels") +
  labs(x = "", y = "BIAS", color = "Significant") +
  theme(
    legend.position = "none",
    #legend.title = element_text(size = 20),
    #legend.key.height = unit(2, "in"),
    #legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18, face = "bold")
  )
ggtime

ggsave(
  filename = "../../plots/comparison/distStat/timeBiasCommon.png",
  plot = ggtime, height = 14, width = 12
)


# BIAS by county
for (j in 1:length(counties)){
  ggplot(biasdfc %>% filter(county == counties[j]), aes(x = year)) +
    geom_point(aes(y = bias, color = !(biasU*biasL < 0)), size = 2) +
    geom_errorbar(aes(ymin = biasL, ymax = biasU, color = !(biasU*biasL < 0)), width = 1, linewidth = 0.8) +
    scale_x_continuous(breaks = c(1990, 2000, 2010)) +
    facet_wrap(~month, labeller = "monthLabels") +
    labs(x = "", y = "BIAS", color = "Significant") +
    theme(
      legend.position = "none",
      #legend.title = element_text(size = 20),
      #legend.key.height = unit(2, "in"),
      #legend.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 18, face = "bold")
    )
  
  
  ggsave(
    filename = paste0("../../plots/comparison/distStat/counties/timeBias", counties[j], ".png"),
    height = 14, width = 12
  )
}

# CRPS COMMON
ggcrps <- ggplot(crpsdf, aes(x = year)) +
  geom_point(aes(y = crps), size = 2) +
  scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  facet_wrap(~month, labeller = "monthLabels") +
  labs(x = "", y = "CRPS") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(2, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18, face = "bold")
  )


ggsave(
  filename = "../../plots/comparison/distStat/timeCRPSCommon.png",
  plot = ggcrps, height = 14, width = 12
)

# CRPS by county
for (j in 1:length(counties)){
  ggplot(crpsdfc %>% filter(county == counties[j]), aes(x = year)) +
    geom_point(aes(y = crps), size = 2) +
    scale_x_continuous(breaks = c(1990, 2000, 2010)) +
    facet_wrap(~month, labeller = "monthLabels") +
    labs(x = "", y = "CRPS") +
    theme(
      legend.title = element_text(size = 20),
      legend.key.height = unit(2, "in"),
      legend.text = element_text(size = 18),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 18, face = "bold")
    )
  
  
  ggsave(
    filename = paste0("../../plots/comparison/distStat/counties/timeCRPS", counties[j], ".png"),
    height = 14, width = 12
  )
}

# CRPS by county common
ggplot(crpsdfc, aes(x = year)) +
  geom_line(aes(y = crps, color = county)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  scale_color_viridis_d() +
  facet_wrap(~month, labeller = "monthLabels") +
  labs(x = "", y = "CRPS", color = "County") +
  theme(
    legend.title = element_text(size = 20),
    legend.key.height = unit(0.6, "in"),
    legend.key.width = unit(0.5, "in"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text = element_text(size = 18, face = "bold")
  ) +
  guides(color = guide_legend(override.aes = list(size = 10, linewidth = 4)))


ggsave(
  filename = paste0("../../plots/comparison/distStat/timeCRPSCounties.png"),
  height = 14, width = 14
)

