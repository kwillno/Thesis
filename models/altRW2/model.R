# Define model

# TOC:
#   Libraries
#   Data loading
#   Parameter specification
#   Model Fit and Storage
#   Diagnostic Plots
#   Ridge Plots
#   Model Validation Scores
  

# Load libraries
library(sf)
library(terra)
library(inlabru)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)

modelName <- "altRW2"

time.initial <- Sys.time()

# ------ Load data ------

crs <- readRDS("../../../data/objects/models/crs.rds")
domain <- readRDS("../../../data/objects/models/domain.rds")
mesh <- readRDS("../../../data/objects/models/mesh.rds")

ECA <- st_transform(read_sf("../../../data/ECA/ECA.nc"), crs)

alt <- rast("../../../data/elevation/altitude.nc")

# ------ Set model parameters ------

# Using informative priors
# For the Intercepts use the (rounded) mean of the temperatures for that month
# as the mean of the prior, and 1 as the precision of the prior
intMeans <- round((ECA %>% group_by(month) %>% summarise(temp = mean(monthlyMean)))$temp)
intPrecision <- rep(1, 12)

# Set priors for hyperparameters of the Matern field
maternField <- INLA::inla.spde2.pcmatern(
  mesh,
  prior.sigma = c(1, 0.01),  # P(sigma > 1) < 0.01  (Deviation in RF)
  prior.range = c(100, 0.95) # P(rho < 100) < 0.95  (Range of RF)
)

# ------ Fit the model ------

months <- readRDS("../../../data/objects/months.rds")

# Store parameter estimates from the different models
df <- data.frame()
randomWalk <- data.frame()

# Store data for ridgeplot
lnkFnc <- function(x){x}
intDists <- data.frame()
altDists <- data.frame()
rhoDists <- data.frame()

# Model objects
modelList <- list()

time.setup <- Sys.time()
print("Setup procedure: ")
print(time.setup - time.initial)

# Fit a separate model for each year
for (i in 1:12){
  print(paste("Fitting", modelName, "model", i, "of", 12))
  
  # Set model components
  comps <- ~ Intercept(intMeans[i], prec.linear = intPrecision[i]) +
    alt(alt, model='linear') +
    time_rw(year - 1980, model = 'rw2', scale.model = T) +
    space(     # Spatial field varying in time and space
      geometry,
      model = maternField,
      replicate = year - 1980
    )
  
  # Set the formula of the model
  formula <- monthlyMean ~ .
  
  # Define the likelihood
  lik <- like(
    formula = formula,
    data = subset(ECA, month == i),
    family = "gaussian"
  )
  
  # Run model fit
  altTimeModel <- bru(
    components = comps,
    lik
  )
  
  # Store parameter estimates
  row <- data.frame(
    # Intercept
    int = altTimeModel$summary.fixed$mean[1],
    intU = altTimeModel$summary.fixed$`0.975quant`[1],
    intL = altTimeModel$summary.fixed$`0.025quant`[1],
    
    # Altitude 
    alt = altTimeModel$summary.fixed$mean[2],
    altU = altTimeModel$summary.fixed$`0.975quant`[2],
    altL = altTimeModel$summary.fixed$`0.025quant`[2],
    
    # Marginal Precision
    prcMarg = altTimeModel$summary.hyperpar$mean[1],
    prcMargU = altTimeModel$summary.hyperpar$`0.975quant`[1],
    prcMargL = altTimeModel$summary.hyperpar$`0.025quant`[1],
    
    # Precision for Random Walk
    prcRW2 = altTimeModel$summary.hyperpar$mean[2],
    prcRW2U = altTimeModel$summary.hyperpar$`0.975quant`[2],
    prcRW2L = altTimeModel$summary.hyperpar$`0.025quant`[2],
    
    # Range for space
    rho = altTimeModel$summary.hyperpar$mean[3],
    rhoU = altTimeModel$summary.hyperpar$`0.975quant`[3],
    rhoL = altTimeModel$summary.hyperpar$`0.025quant`[3],
    
    # Stdev for space
    sig = altTimeModel$summary.hyperpar$mean[4],
    sigU = altTimeModel$summary.hyperpar$`0.975quant`[4],
    sigL = altTimeModel$summary.hyperpar$`0.025quant`[4]
  )
  df <- rbind(df, row)
  
  randomWalk <- rbind(randomWalk,
                      data.frame(month = i,
                                 year = c(1981:(1980 + length(altTimeModel$summary.random$time_rw$mean))),
                                 value = altTimeModel$summary.random$time_rw$mean
                                 )
                      )
  
  # Ridge plot storage
  intDists <- rbind(intDists, data.frame(INLA::inla.tmarginal(lnkFnc, altTimeModel$marginals.fixed[["Intercept"]]), month = i))
  altDists <- rbind(altDists, data.frame(INLA::inla.tmarginal(lnkFnc, altTimeModel$marginals.fixed[["alt"]]), month = i))
  rhoDists <- rbind(rhoDists, data.frame(INLA::inla.tmarginal(lnkFnc, altTimeModel$marginals.hyperpar[["Range for space"]]), month = i))
  
  # Store models
  modelList[[i]] <- altTimeModel
}

time.models <- Sys.time()
print("Model fit procedure: ")
print(time.models - time.setup)

# Store model objects 
print("Storing model objects...")
saveRDS(modelList, file = paste0("../../../data/objects/models/", modelName, "/models.rds"))

# This is done here to make sure the models are saved before plots are generated
time.saveModels <- Sys.time()
print("Model saving procedure: ")
print(time.saveModels - time.models)


df$month <- as.numeric(row.names(df))
randomWalk$month <- as.factor(randomWalk$month)

# ------ Plot diagnostics ------

intplot <- ggplot(df, aes(x = month, y = int, ymin = intL, ymax = intU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Intercept")

altplot <- ggplot(df, aes(x = month, y = alt, ymin = altL, ymax = altU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Alt")

margVarplot <- ggplot(df, aes(x = month, y = 1/prcMarg, ymin = 1/prcMargU, ymax = 1/prcMargL)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Marginal Variance")

RW2Varplot <- ggplot(df, aes(x = month, y = 1/prcRW2, ymin = 1/prcRW2U, ymax = 1/prcRW2L)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("RandomWalk2 Variance")

rhoplot <- ggplot(df, aes(x = month, y = rho, ymin = rhoL, ymax = rhoU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Range for Space")

spcVar <- ggplot(df, aes(x = month, y = sig, ymin = sigL, ymax = sigU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Stdev for Space")

RW2plot <- ggplot(randomWalk, aes(x = year, y = value, group = month, color = month)) +
  geom_point() +
  ggtitle("Random Walks")


# Show diagnostic plots
pdf(paste0("../../../plots/models/", modelName, "/diag1.pdf"))
multiplot(intplot, altplot)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag2.pdf"))
multiplot(margVarplot, RW2Varplot)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag3.pdf"))
multiplot(rhoplot, spcVar)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag4.pdf"))
multiplot(RW2plot)
dev.off()

# Save data objects in same folder as plots
saveRDS(df, file = paste0("../../../data/objects/models/", modelName, "/df.rds"))
saveRDS(randomWalk, file = paste0("../../../data/objects/models/", modelName, "/randomWalk.rds"))


# ------ Generate Ridgeplot for alt estimate -----
# INTERCEPT
# Create common x-axis
intAxis <- seq(min(intDists$x), max(intDists$x), length.out = 100)

# Init object
intParam <- data.frame()

for (i in 1:nrow(df)){
  intParam <- rbind(intParam,
                    data.frame(
                      x = intAxis,
                      value = INLA::inla.dmarginal(intAxis, intDists %>% filter(month == i)),
                      month = i
                    ))
}
intParam$month <- as.factor(intParam$month)

intRidgePlot <- ggplot(intParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/intRidge.pdf"), plot = intRidgePlot, width = 6, height = 8)


# ALTITUDE
# Create common x-axis
altAxis <- seq(min(altDists$x), max(altDists$x), length.out = 100)

# Init object
altParam <- data.frame()

for (i in 1:nrow(df)){
  altParam <- rbind(altParam,
                    data.frame(
                      x = altAxis,
                      value = INLA::inla.dmarginal(altAxis, altDists %>% filter(month == i)),
                      month = i
                    ))
}
altParam$month <- as.factor(altParam$month)

altRidgePlot <- ggplot(altParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/altRidge.pdf"), plot = altRidgePlot, width = 6, height = 8)

# RANGE
# Create common x-axis
rhoAxis <- seq(min(rhoDists$x), max(rhoDists$x), length.out = 100)

# Init object
rhoParam <- data.frame()

for (i in 1:nrow(df)){
  rhoParam <- rbind(rhoParam,
                    data.frame(
                      x = rhoAxis,
                      value = INLA::inla.dmarginal(rhoAxis, rhoDists %>% filter(month == i)),
                      month = i
                    ))
}
rhoParam$month <- as.factor(rhoParam$month)

rhoRidgePlot <- ggplot(rhoParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/rhoRidge.pdf"), plot = rhoRidgePlot, width = 6, height = 8)

time.plots <- Sys.time()
print("Plot procedure: ")
print(time.plots - time.saveModels)

# ------ Model Validation ------

# This section gets the model validation scores for all the models and reports them


reportScores <- function(modelObj, i = 1, name = modelName, roundDigits = 6){
  # DIC
  dic <- modelObj$dic$dic
  
  # WAIC
  waic <- modelObj$waic$waic
  
  ## LGOCV
  m <- 3
  LGOCV <- INLA::inla.group.cv(modelObj, num.level.sets = m)
  
  # MLS
  mls <- mean( - log(LGOCV$cv))
  
  # DSS
  Y_obs <- (ECA %>% filter(month == i))$monthlyMean       # Observations
  Y_est <- LGOCV$mean                                     # Estimates at observation locations
  Y_sd  <- LGOCV$sd + modelObj$summary.hyperpar$mean[1]   # St.Dev. of estimate at obsevation location
  
  dss <- mean(( (Y_obs - Y_est) / (Y_sd)  )^2 + log( Y_sd^2  ))
  
  print(
    paste0(
      name, "(", str_pad(i, 2), "): ",
      ", DIC: " , round(dic,  roundDigits),
      ", WAIC: ", round(waic, roundDigits),
      ", MLS: " , round(mls,  roundDigits),
      ", DSS: " , round(dss,  roundDigits)
    )
  )
  
  row <- data.frame(name = name, month = i, DIC=dic, WAIC = waic, MLS = mls, DSS = dss)
  return(row)
}

scores <- data.frame()
for (i in 1:12){
  if (is.null(modelList[[i]])){next} # Skip iteration for nondefined models
  
  row <- reportScores(modelObj = modelList[[i]], i)
  scores <- rbind(scores, row)
}

# Save model validation scores
saveRDS(scores, file = paste0("../../../data/objects/models/", modelName, "/CV.rds"))

time.validation <- Sys.time()
print("Model validation procedure: ")
print(time.validation - time.plots)

# Notify that code is finished.
print("Model fit prcedure finished gracefully.")
