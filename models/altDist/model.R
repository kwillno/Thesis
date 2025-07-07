# Defining model

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

modelName <- "altDistCut"
testing <- FALSE

time.initial <- Sys.time()

# ------ Load data ------

crs <- readRDS("../../../data/objects/models/crs.rds")
domain <- readRDS("../../../data/objects/models/domain.rds")
mesh <- readRDS("../../../data/objects/models/mesh.rds")

ECA <- st_transform(read_sf("../../../data/ECA/ECA.nc"), crs)

# Cut ECA data to remove shift in temperatures
ECA <- ECA %>% filter(year >= 1988)

if (testing){
  ECA <- ECA %>% filter(year < 2000, year >= 1990)
}

# Elevationd field
alt <- rast("../../../data/elevation/altitude.nc")

# Coordinate value rasters
east <- rast("../../../data/objects/models/east.nc")
north <- rast("../../../data/objects/models/north.nc")

# Distance from sea field
distToSea <- rast("../../../data/objects/models/distSea.nc")

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

# Hyperparameters for random effect in time
hyper <- list(prec = list(prior = "pc.prec", param = c(1, 0.9)) )

# ------ Fit the model ------

months <- readRDS("../../../data/objects/months.rds")

# Store parameter estimates from the different models
df <- data.frame()
randomWalk <- data.frame()

# Store data for ridgeplot
lnkFnc <- function(x){x}
intDists <- data.frame()
altDists <- data.frame()
seaDists <- data.frame()
easDists <- data.frame()
norDists <- data.frame()
tmlDists <- data.frame()
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
  comps <- ~ Intercept(1, mean.linear = intMeans[i], prec.linear = intPrecision[i]) +
    alt(alt, model='linear') +
    distSea(distToSea, model = 'linear') +
    ew(east, model = 'linear') +
    ns(north, model = 'linear') +
    time_lin(year - 1980, model = 'linear') +
    time_re(year - 1980, model = 'iid', constr = TRUE, hyper = hyper) +
    space(
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
  model <- bru(
    components = comps,
    lik
  )
  
  # Store parameter estimates
  row <- data.frame(
    # Intercept
    int = model$summary.fixed$mean[1],
    intU = model$summary.fixed$`0.975quant`[1],
    intL = model$summary.fixed$`0.025quant`[1],
    
    # Altitude 
    alt = model$summary.fixed$mean[2],
    altU = model$summary.fixed$`0.975quant`[2],
    altL = model$summary.fixed$`0.025quant`[2],
    
    # Distance to sea effect 
    sea = model$summary.fixed$mean[3],
    seaU = model$summary.fixed$`0.975quant`[3],
    seaL = model$summary.fixed$`0.025quant`[3],
    
    # Easting effect 
    eas = model$summary.fixed$mean[4],
    easU = model$summary.fixed$`0.975quant`[4],
    easL = model$summary.fixed$`0.025quant`[4],
      
    # Northing effect 
    nor = model$summary.fixed$mean[5],
    norU = model$summary.fixed$`0.975quant`[5],
    norL = model$summary.fixed$`0.025quant`[5],
    
    # Time linear effect 
    tml = model$summary.fixed$mean[6],
    tmlU = model$summary.fixed$`0.975quant`[6],
    tmlL = model$summary.fixed$`0.025quant`[6],
    
    # Marginal Precision
    prcMarg = model$summary.hyperpar$mean[1],
    prcMargU = model$summary.hyperpar$`0.975quant`[1],
    prcMargL = model$summary.hyperpar$`0.025quant`[1],
    
    # Precision for Time random effect
    prcTmr = model$summary.hyperpar$mean[2],
    prcTmrU = model$summary.hyperpar$`0.975quant`[2],
    prcTmrL = model$summary.hyperpar$`0.025quant`[2],
    
    # Range for space
    rho = model$summary.hyperpar$mean[3],
    rhoU = model$summary.hyperpar$`0.975quant`[3],
    rhoL = model$summary.hyperpar$`0.025quant`[3],
    
    # Stdev for space
    sig = model$summary.hyperpar$mean[4],
    sigU = model$summary.hyperpar$`0.975quant`[4],
    sigL = model$summary.hyperpar$`0.025quant`[4],
    
    
    # Add month to row
    month = i
  )
  df <- rbind(df, row)
  
  
  # Ridge plot storage
  intDists <- rbind(intDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["Intercept"]]), month = i))
  altDists <- rbind(altDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["alt"]]), month = i))
  seaDists <- rbind(seaDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["distSea"]]), month = i))
  easDists <- rbind(easDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["ew"]]), month = i))
  norDists <- rbind(norDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["ns"]]), month = i))
  tmlDists <- rbind(tmlDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.fixed[["time_lin"]]), month = i))
  rhoDists <- rbind(rhoDists, data.frame(INLA::inla.tmarginal(lnkFnc, model$marginals.hyperpar[["Range for space"]]), month = i))
  
  # Store models
  modelList[[i]] <- model
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

# ------ Plot diagnostics ------

intplot <- ggplot(df, aes(x = month, y = int, ymin = intL, ymax = intU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Intercept")

altplot <- ggplot(df, aes(x = month, y = alt, ymin = altL, ymax = altU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Alt")

seaplot <- ggplot(df, aes(x = month, y = sea, ymin = seaL, ymax = seaU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Time Linear effect")

easplot <- ggplot(df, aes(x = month, y = eas, ymin = easL, ymax = easU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Easting Linear effect")

norplot <- ggplot(df, aes(x = month, y = nor, ymin = norL, ymax = norU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Northing Linear effect")

tmlplot <- ggplot(df, aes(x = month, y = tml, ymin = tmlL, ymax = tmlU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Time Linear effect")

margVarplot <- ggplot(df, aes(x = month, y = 1/prcMarg, ymin = 1/prcMargU, ymax = 1/prcMargL)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Marginal Variance")

tmrVarplot <- ggplot(df, aes(x = month, y = 1/prcTmr, ymin = 1/prcTmrU, ymax = 1/prcTmrL)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Time Random effect Variance")

rhoplot <- ggplot(df, aes(x = month, y = rho, ymin = rhoL, ymax = rhoU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Range for Space")

spcVar <- ggplot(df, aes(x = month, y = sig, ymin = sigL, ymax = sigU)) + 
  geom_point() +
  geom_errorbar() +
  ggtitle("Stdev for Space")


# Show diagnostic plots
pdf(paste0("../../../plots/models/", modelName, "/diag1.pdf"))
multiplot(intplot, altplot, tmlplot)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag2.pdf"))
multiplot(seaplot, easplot, norplot)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag3.pdf"))
multiplot(margVarplot, tmrVarplot)
dev.off()
pdf(paste0("../../../plots/models/", modelName, "/diag4.pdf"))
multiplot(rhoplot, spcVar)
dev.off()

# Save data objects
saveRDS(df, file = paste0("../../../data/objects/models/", modelName, "/df.rds"))


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

# DISTANCE FROM SEA
# Create common x-axis
seaAxis <- seq(min(seaDists$x), max(seaDists$x), length.out = 100)

# Init object
seaParam <- data.frame()

for (i in 1:nrow(df)){
  seaParam <- rbind(seaParam,
                    data.frame(
                      x = seaAxis,
                      value = INLA::inla.dmarginal(seaAxis, seaDists %>% filter(month == i)),
                      month = i
                    ))
}
seaParam$month <- as.factor(seaParam$month)

seaRidgePlot <- ggplot(seaParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/seaRidge.pdf"), plot = seaRidgePlot, width = 6, height = 8)


# EASTING
# Create common x-axis
easAxis <- seq(min(easDists$x), max(easDists$x), length.out = 100)

# Init object
easParam <- data.frame()

for (i in 1:nrow(df)){
  easParam <- rbind(easParam,
                    data.frame(
                      x = easAxis,
                      value = INLA::inla.dmarginal(easAxis, easDists %>% filter(month == i)),
                      month = i
                    ))
}
easParam$month <- as.factor(easParam$month)

easRidgePlot <- ggplot(easParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/easRidge.pdf"), plot = easRidgePlot, width = 6, height = 8)

# NORTHING
# Create common x-axis
norAxis <- seq(min(norDists$x), max(norDists$x), length.out = 100)

# Init object
norParam <- data.frame()

for (i in 1:nrow(df)){
  norParam <- rbind(norParam,
                    data.frame(
                      x = norAxis,
                      value = INLA::inla.dmarginal(norAxis, norDists %>% filter(month == i)),
                      month = i
                    ))
}
norParam$month <- as.factor(norParam$month)

norRidgePlot <- ggplot(norParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/norRidge.pdf"), plot = norRidgePlot, width = 6, height = 8)

# TIME LINEAR
# Create common x-axis
tmlAxis <- seq(min(tmlDists$x), max(tmlDists$x), length.out = 100)

# Init object
tmlParam <- data.frame()

for (i in 1:nrow(df)){
  tmlParam <- rbind(tmlParam,
                    data.frame(
                      x = tmlAxis,
                      value = INLA::inla.dmarginal(tmlAxis, tmlDists %>% filter(month == i)),
                      month = i
                    ))
}
tmlParam$month <- as.factor(tmlParam$month)

tmlRidgePlot <- ggplot(tmlParam, aes(x = x, y = month, height = value, fill = month)) +
  geom_density_ridges(stat = "identity", scale = 1, alpha = 0.8) +
  scale_fill_viridis_d() +
  xlab("Parameter value") + ylab("Month") + 
  theme(
    panel.spacing = unit(2, "lines"),
    legend.position = "none",
    text = element_text(size = 20)
  )

ggsave(filename = paste0("../../../plots/models/", modelName, "/tmlRidge.pdf"), plot = tmlRidgePlot, width = 6, height = 8)


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
