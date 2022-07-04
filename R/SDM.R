library(tidyverse)
library(leaflet)
# library(spatialEco)
# library(dismo)
library(geodata)
library(terra)
library(sf)


# reading/downloading data ------------------------------------------------
# test a species
source("R/china_data.R")
# dir to the bioclim data
fdir <- "C:/Users/61423/Climate_data/CHELSA_1981_2010/bio/"

gbif_data <- geodata::sp_occurrence(genus = "Diaphorina",
                                    species = "citri")

sp_points <- gbif_data %>%
  dplyr::select(lon, lat, status = occurrenceStatus,
                country, species) %>%
  drop_na(lon, lat) %>% 
  filter(status == "PRESENT") %>% 
  dplyr::select(species, long = lon, lat) %>% 
  bind_rows(data_wang) %>%  # combine with wang data
  bind_rows(data_adidoo) %>% # combine with adidoo data
  st_as_sf(coords = c("long", "lat"), crs = 4326)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = sp_points,
    radius = 4,
    stroke = FALSE,
    color = "red",
    label = ~species,
    fillOpacity = 0.4)


# data cleaning -----------------------------------------------------------
bio1 <- rast(paste0(fdir, "CHELSA_bio1_1981-2010_V.2.1.tif"))
# aggregate to 10km cells to remove close records
dpmask <- terra::aggregate(bio1, fact = 25)
# remove duplicates
dup <- cellFromXY(dpmask, st_coordinates(sp_points)) %>% 
  duplicated()
occ <- sp_points[!dup, ]

elev <- rast("C:/Users/61423/Climate_data/Elevation_30s_worldclim/wc2.1_30s_elev.tif") %>% 
  setNames("elevation")
plot(elev)
plot(occ, add = TRUE, col = 'red')
# mapview::mapview(occ)


plot(c(bio1, elev))

extract(elev, terra::vect(occ)) %>% 
  as.data.frame() %>% 
  mutate(high = elevation <= 1500) %>% 
  pull(high) %>% 
  sum(na.rm = TRUE)

occ <- occ %>% 
  mutate(elev = terra::extract(elev, terra::vect(.), df = TRUE)$elevation) %>% 
  st_cast("POINT") %>% 
  filter(elev < 1500)
occ


# get world map to filter data
worldmap <- geodata::world(path = "data/")
cabi_loc <- sf::read_sf("data/CABI_diaphorina_citri.shp")
cabi_counties <- worldmap[vect(cabi_loc), ]
plot(cabi_counties)
plot(cabi_loc$geometry, add = TRUE, col = "red", pch = 16)

# filter the records based on CABI countries
occ_clean <- occ[st_as_sf(cabi_counties), ]
occ_clean
mapview::mapview(occ_clean, zcol = "elev", label = "elev")

# st_write(occ_clean, "data/occ_clean.gpkg")



# # make a mask with elevation and annual temperature
# elev[elev > 1500] <- NA
# plot(elev)
# bio1[bio1 <= 0] <- NA
# plot(bio1)
# bio1 <- terra::extend(bio1, elev)
# bio1
# plot(c(bio1, elev))
# 
# bgmask <- (bio1 + elev) %>%
#   terra::app(function(x) x > -10000) %>%
#   terra::mask(worldmap[!worldmap$GID_0 %in% c("AUS", "NZL")]) %>% 
#   setNames("mask")
# 
# terra::writeRaster(bgmask, "mask.tif", overwrite = TRUE)
# plot(rast("mask.tif"))



robproj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
mycols <- terrain.colors(30, rev = TRUE)

ggplot() +
  geom_sf(data = st_as_sf(worldmap), fill = NA, inherit.aes = FALSE) +
  geom_sf(data = st_as_sf(cabi_counties), fill = NA, col = "blue", inherit.aes = FALSE) +
  geom_sf(data = occ_clean, col = "red", alpha = 0.5, inherit.aes = FALSE) +
  coord_sf(crs = robproj) +
  theme_minimal() +
  labs(x = NULL, y = NULL)


# library(rasterVis)

# elev_rob <- terra::project(elev, robproj)

# gplot(elev_rob) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradientn(colours = mycols, na.value = NA) +
#   # scale_fill_viridis_c(option = "G", direction = -1, na.value = NA) +
#   geom_sf(data = st_as_sf(worldmap), fill = NA, inherit.aes = FALSE) +
#   geom_sf(data = st_as_sf(cabi_counties), fill = NA, col = "blue", inherit.aes = FALSE) +
#   geom_sf(data = occ_clean, col = "red", alpha = 0.5, inherit.aes = FALSE) +
#   coord_sf(crs = robproj) +
#   theme_minimal() +
#   labs(x = NULL, y = NULL)

# environmental data ------------------------------------------------------
# list of potential covars
flist <- c(
  "CHELSA_bio1_1981-2010_V.2.1.tif",
  "CHELSA_bio2_1981-2010_V.2.1.tif",
  "CHELSA_bio3_1981-2010_V.2.1.tif",
  "CHELSA_bio4_1981-2010_V.2.1.tif",
  "CHELSA_bio5_1981-2010_V.2.1.tif",
  "CHELSA_bio6_1981-2010_V.2.1.tif",
  "CHELSA_bio7_1981-2010_V.2.1.tif",
  "CHELSA_bio8_1981-2010_V.2.1.tif",
  "CHELSA_bio10_1981-2010_V.2.1.tif",
  "CHELSA_bio11_1981-2010_V.2.1.tif",
  "CHELSA_bio12_1981-2010_V.2.1.tif",
  "CHELSA_bio14_1981-2010_V.2.1.tif",
  "CHELSA_bio15_1981-2010_V.2.1.tif",
  "CHELSA_bio17_1981-2010_V.2.1.tif",
  "CHELSA_bio18_1981-2010_V.2.1.tif",
  "CHELSA_cmi_mean_1981-2010_V.2.1.tif",
  "CHELSA_gdd0_1981-2010_V.2.1.tif",
  "CHELSA_gdd10_1981-2010_V.2.1.tif",
  "CHELSA_gsl_1981-2010_V.2.1.tif",
  "CHELSA_hurs_mean_1981-2010_V.2.1.tif",
  "CHELSA_ngd0_1981-2010_V.2.1.tif",
  "CHELSA_ngd10_1981-2010_V.2.1.tif",
  "CHELSA_npp_1981-2010_V.2.1.tif",
  "CHELSA_vpd_mean_1981-2010_V.2.1.tif"
)

bios <- rast(paste0(fdir, flist))
names(bios) <- map_chr(names(bios), function(x) str_split(x, "_")[[1]][2])
plot(bios[[1:3]])


library(RStoolbox)

bios_pca <- RStoolbox::rasterPCA(
  img = bios,
  nSamples = 10000,
  nComp = 5,
  spca = TRUE,
)
plot(bios_pca)


library(factoextra)

pca_data <- terra::spatSample(x = bios,
                              size = 1e4,
                              method = "random",
                              xy = FALSE,
                              values = TRUE,
                              na.rm = TRUE)
head(pca_data)
pca_model <- prcomp(data = , scale = TRUE)


bios_pca <- predict(bios, pca_model)




# read a raster mask for the region
bgmask <- terra::rast("data/mask.tif") %>%
  terra::crop(bios[[1]]) %>%
  setNames("mask")

kde_mask <- terra::rast("data/kde_mask.tif") %>% 
  terra::app(function(x) sqrt(x + 0.1)) %>% 
  terra::mask(bgmask) %>%
  setNames("kde")
plot(raster::raster(kde_mask), zlim = c(0,0.6))

# reduce the size to fit in memory
kde_mask <- terra::aggregate(kde_mask, fact = 5)

# generate 10k random samples from the KDE raster file
bgs <- terra::spatSample(x = kde_mask,
                         size = 1e4,
                         method = "weights",
                         # method = "random",
                         xy = TRUE,
                         values = FALSE,
                         na.rm = TRUE)

rasterVis::gplot(kde_mask) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = terrain.colors(30, rev = TRUE), na.value = NA) +
  geom_point(data = as.data.frame(bgs), aes(x = x, y = y), alpha = 0.01) +
  theme_void() +
  coord_equal()




rasters <- bios_pca
rasters <- bios

bio_pr <- terra::extract(rasters, vect(occ_clean)) %>% 
  as_tibble() %>% 
  mutate(occ = 1) %>% 
  dplyr::select(-ID)
bio_bg <- terra::extract(rasters, bgs[,c("x","y")]) %>% 
  as_tibble() %>% 
  mutate(occ = 0)

train_data <- bind_rows(bio_pr, bio_bg) %>% 
  dplyr::relocate(occ) # get occ as first column

covars <- c(
  # "bio1",
  # "bio2",
  # "bio3",
  "bio4",
  # "bio5",
  # "bio6",
  # "bio7",
  "bio8",
  # "bio10",
  # "bio11",
  "bio12",
  "bio14",
  "bio15",
  # "bio17",
  "bio18",
  # "cmi",
  "gdd0",
  # "gdd10"
  # "gsl"
  # "npp"
  "hurs"
  # "vpd"
  # "ngd0"
  # "ngd10"
)

# usdm::vifstep(x = as.data.frame(train_data[, covars]), th = 10)
# 
# train_data[, covars] %>% 
#   # dplyr::select(-occ) %>% 
#   sample_n(5000) %>% 
#   PerformanceAnalytics::chart.Correlation(method = "pearson")

covars <- paste0("pc", 1:5)

training <- train_data[, c("occ", covars)] %>% 
  mutate(
    bio12 = log(bio12 + 1),
    bio14 = log(bio14 + 1),
    bio18 = log(bio18 + 1)
  ) %>%
  as.data.frame()
head(training)

#
# modelling ---------------------------------------------------------------
# # BRT ---------------------------------------------------------------------
library(dismo)
library(gbm)

# weighting
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

set.seed(32755)
brt <- gbm.step(data = training,
                gbm.x = which(!names(training) %in% "occ"),
                gbm.y = 1,
                family = "bernoulli",
                tree.complexity = 3,
                learning.rate = 0.01,
                bag.fraction = 0.75,
                max.trees = 10000,
                n.trees = 50,
                n.folds = 5,
                site.weights = wt,
                silent = FALSE)

dismo::gbm.interactions(brt)$rank.list



# plot all three interaction levels
varimp <- summary(brt)

ggplot(aes(y = rel.inf, x = reorder(var, rel.inf), fill = rel.inf),
       data = varimp) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(label = paste0(round(rel.inf, 2), "")),
            color = "gray5", size = 3.5, position = position_nudge(y = + 1.5)) +
  viridis::scale_fill_viridis(option = "C", direction = -1) +
  labs(y = "Relative influence (%)", x = "Variables") +
  guides(fill = "none") +
  theme_classic()

#
# # Lasso -------------------------------------------------------------------
# loading the library
library(glmnet)
library(myspatial)


quad_obj <- make_quadratic(training, cols = covars)
training_quad <- predict(quad_obj, newdata = training)

new_vars <- names(training_quad)[names(training_quad) != "occ"]
training_sparse <- sparse.model.matrix(~. -1, training_quad[, new_vars])

# calculating the case weights
prNum <- as.numeric(table(training_quad$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training_quad$occ)["0"]) # number of backgrounds
wt <- ifelse(training_quad$occ == 1, 1, prNum / bgNum)

lasso_cv <- cv.glmnet(x = training_sparse,
                      y = training_quad$occ,
                      family = "binomial",
                      alpha = 1, # fitting lasso
                      weights = wt,
                      nfolds = 10) # number of folds for cross-validation
plot(lasso_cv)


# # GAM ---------------------------------------------------------------------
# loading the packages
library(mgcv)

# calculating the case weights (equal weights)
# the order of weights should be the same as presences and backgrounds in the training data
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

form <- occ ~ s(bio1, bs  = "tp", k = 10) +
  s(bio4, bs  = "tp", k = 10) +
  s(bio5, bs  = "tp", k = 10) +
  s(bio8, bs  = "tp", k = 10) +
  s(bio12, bs  = "tp", k = 10) +
  s(bio18, bs  = "tp", k = 10) +
  s(hurs, bs  = "tp", k = 10) +
  s(bio1, bio12)

tmp <- Sys.time()
set.seed(32639)
gm <- mgcv::gam(formula = as.formula(form),
                data = training,
                family = binomial(link = "logit"),
                # family = binomial(link = "cloglog"),
                weights = wt,
                method = "REML")
Sys.time() - tmp
summary(gm)

# check the appropriateness of Ks
# gam.check(gm)
# plot(gm, pages = 1, rug = TRUE, shade = TRUE)

# # Maxent ------------------------------------------------------------------
# load the package
library(dismo)


tmp <- Sys.time()
# fit a maxent model with the tuned parameters
maxmod <- dismo::maxent(x = training[, covars],
                        p = training$occ,
                        removeDuplicates = FALSE,
                        path = "output/maxent_files",
                        args = c("nothreshold", "noautofeature",
                                 "nothreshold", "nolinear", "noquadratic", "noproduct", # H
                                 # "noproduct", # LQH
                                 "betamultiplier=2"))
Sys.time() - tmp

#
# RF - ranger -------------------------------------------------------------
# function for tuning the parameters of shallow model
tune_ranger <- function(data, 
                        y = "occ", # name of presence-background column
                        k = 5, # number of cross-validation folds
                        max.depth = 2:5, # this can be a vector of numbers
                        splitrule = c("gini", "extratrees", "hellinger"), # only allowed options
                        num.trees = 1000, # this can be a vector of numbers
                        mtry = NULL, # this can be a vector of numbers; NULL = default 
                        threads = 4){ # number of CPU cores
  require(ranger)
  # splitrule <- match.arg(splitrule)
  names(data)[which(names(data) == y)] <- "po"
  data$po <- as.factor(data$po)
  if(is.null(mtry)){
    mtry <- floor(sqrt(ncol(data) - 1))
  }
  grid <- expand.grid(max.depth = max.depth, 
                      splitrule = splitrule,
                      num.trees = num.trees,
                      mtry = mtry,
                      stringsAsFactors = FALSE)
  # create balanced CV folds
  folds <- caret::createFolds(y = as.factor(data$po), k = k)
  evalmodel <- data.frame(depth = rep(NA, nrow(grid)), split = NA)
  iteration <- nrow(grid)
  # pb <- progress::progress_bar$new(format = "Progress [:bar] :percent in :elapsed",
  #                                  total = iteration, clear = FALSE, width = 75)
  for(i in seq_along(grid[,1])){
    modauc <- c()
    for(k in seq_along(folds)){
      trainSet <- unlist(folds[-k])
      testSet <- unlist(folds[k])
      prNum <- as.numeric(table(data[trainSet, ]$po)["1"]) # number of presences
      bgNum <- as.numeric(table(data[trainSet, ]$po)["0"]) # number of backgrounds
      casewts <- ifelse(data[trainSet, ]$po == 1, 1, prNum / bgNum)
      mod <- ranger::ranger(formula = po ~ .,
                            data = data[trainSet, ], 
                            probability = TRUE,
                            num.trees = grid$num.trees[i],
                            splitrule = grid$splitrule[i],
                            max.depth = grid$max.depth[i],
                            mtry = grid$mtry[i],
                            sample.fraction = prNum / bgNum,
                            case.weights = casewts,
                            num.threads = threads,
                            replace = TRUE)
      pred <- predict(mod, data[testSet, ], type = "response")$predictions[,"1"]
      modauc[k] <- precrec::auc(precrec::evalmod(scores = pred, 
                                                 labels = data$po[testSet]))[1,4]
    }
    evalmodel$depth[i] <- grid$max.depth[i]
    evalmodel$split[i] <- grid$splitrule[i]
    evalmodel$ntrees[i] <- grid$num.trees[i]
    evalmodel$mtry[i] <- grid$mtry[i]
    evalmodel$aucme[i] <- mean(modauc)
    # evalmodel$aucse[i] <- sd(modauc) / sqrt(5)
    # pb$tick()
  }
  bestparam <- which.max(evalmodel$aucme)
  print(evalmodel[bestparam, ])
  prNum <- as.numeric(table(data$po)["1"]) # number of presences
  bgNum <- as.numeric(table(data$po)["0"]) # number of backgrounds
  casewts <- ifelse(data$po == 1, 1, prNum / bgNum)
  finalmod <- ranger::ranger(formula = po ~ .,
                             data = data, 
                             probability = TRUE,
                             num.trees = evalmodel$ntrees[bestparam],
                             splitrule = evalmodel$split[bestparam],
                             max.depth = evalmodel$depth[bestparam],
                             sample.fraction = prNum / bgNum,
                             case.weights = casewts,
                             num.threads = threads,
                             replace = TRUE)
  return(finalmod)
}

# fitting ranger-tuned model
rf_shallow_tuned <- tune_ranger(data = training,
                                y = "occ",
                                max.depth = 2:8,
                                splitrule = c("hellinger", 'gini'),
                                num.trees = c(1000),
                                threads = 8)


# # Random Forest -----------------------------------------------------------
# loading the package
library(randomForest)

# convert the response to factor for producing class relative likelihood
training$occ <- as.factor(training$occ)

prNum <- as.numeric(table(training$occ)["1"]) # number of presences
# the sample size in each class; the same as presence number
smpsize <- c("0" = prNum, "1" = prNum)

tmp <- Sys.time()
set.seed(32639)
rf_downsample <- randomForest(formula = occ ~.,
                              data = training,
                              ntree = 1000,
                              sampsize = smpsize,
                              importance = TRUE,
                              replace = TRUE)
Sys.time() - tmp



plot(rf_downsample, main = "RF down-sampled")

importance(rf_downsample, scale = FALSE) %>% 
  as.data.frame() %>% 
  arrange(MeanDecreaseGini)


rf_pred <- raster::predict(object = raster::stack(bios_au[[covars]]),
                           model = rf_downsample,
                           progress = "text",
                           index = 2,
                           type = "prob")
names(rf_pred) <- "RF"
plot(rf_pred, zlim = c(0,1))

writeRaster(rf_pred, "Results/rf_current_225.tif", overwrite = TRUE)


# # -------------------------------------------------------------------------
# Australian map ----------------------------------------------------------
# aggregate to reduce prediction time and easy visualization
bios_agg <- terra::aggregate(bios[[covars]], fact = 5)
# bios_agg <- terra::mask(bios_agg, worldmap)
plot(bios_agg)

# crop to Australia
bios_au <- bios_agg[[covars]] %>% 
  terra::crop(worldmap[worldmap$GID_0 == "AUS"]) %>% 
  terra::mask(worldmap[worldmap$GID_0 == "AUS"])

bios_au$bio12 <- log(bios_au$bio12)
bios_au$bio14 <- log(bios_au$bio14)
bios_au$bio18 <- log(bios_au$bio18)
plot(bios_au)




# predict on rasters
pred_au5k_brt <- raster::predict(
  object = raster::stack(bios_au),
  model = brt,
  n.trees = brt$gbm.call$best.trees,
  progress = "text",
  type = "response"
)
names(pred_au5k_brt) <- "BRT"
plot(pred_au5k_brt, zlim = c(0,1))

# predicting glment on rasters with myspatial package
pred_au5k_glm <- predict_glmnet_raster(
  r = raster::stack(bios_au[[covars]]),
  model = lasso_cv, # the lasso cv object
  quadraticObj = quad_obj, # make_quadratic object
  type = "response",
  # slambda = "lambda.min"
  slambda = "lambda.1se"
)
names(pred_au5k_glm) <- "GLM"
plot(pred_au5k_glm, zlim = c(0,1))

pred_au5k_gam <- raster::predict(object = raster::stack(bios_au[[covars]]),
                            model = gm,
                            progress = "text",
                            type = "response")
names(pred_au5k_gam) <- "GAM"
plot(pred_au5k_gam, zlim = c(0,1))

pred_au5k_max <- raster::predict(object = raster::stack(bios_au[[covars]]),
                               model = maxmod,
                               progress = "text",
                               type = c("cloglog"))
names(pred_au5k_max) <- "Maxent"
plot(pred_au5k_max, zlim = c(0,1))


# predict to raster layers
pred_au5k_rf <- raster::predict(
  object = raster::stack(bios_au[[covars]]),
  model = rf_shallow_tuned,
  progress = "text",
  fun = function(model, ...) predict(model, ...)$predictions[,"1"]
)
names(pred_au5k_rf) <- "RF"
plot(pred_au5k_rf, zlim = c(0,1))

# stack all raster predictions
all_pred_au5k <- list(pred_au5k_brt,
                      pred_au5k_glm, 
                      pred_au5k_gam,
                      pred_au5k_max, 
                      pred_au5k_rf) %>% 
  lapply(function(x) raster::calc(x, function(y) scales::rescale(y, c(0,1)))) %>% 
  raster::stack() %>% 
  terra::rast()
plot(all_pred_au5k)

pred_au5k_ens <- terra::app(all_pred_au5k, fun = "mean")
names(pred_au5k_ens) <- "Ensemble"
plot(pred_au5k_ens)


# writeRaster(pred_au5k_brt, "results/pred_au5k_brt.tif", overwrite = TRUE)
# writeRaster(pred_au5k_glm, "Results/pred_au5k_glm.tif", overwrite = TRUE)
# writeRaster(pred_au5k_gam, "Results/pred_au5k_gam.tif", overwrite = TRUE)
# writeRaster(pred_au5k_max, "Results/pred_au5k_max.tif", overwrite = TRUE)
# writeRaster(pred_au5k_rf, "Results/pred_au5k_rf.tif", overwrite = TRUE)
# writeRaster(pred_au5k_ens, "Results/pred_au5k_ens.tif", overwrite = TRUE)


sd_au5k_ens <- terra::app(all_pred_au5k, fun = "sd")
plot(sd_au5k_ens, col = c("white", viridis::viridis(30, option = "A", direction = -1)))



par(mfrow=c(2,3))
for(i in 1:nlyr(all_pred_au5k)){
  plot(all_pred_au5k[[i]], main = names(all_pred_au5k)[i])
}
plot(sd_pred, main = "Standard Deviation",
     col = c("white", viridis::viridis(30, option = "A", direction = -1)))
par(mfrow=c(1,1))



# Global map --------------------------------------------------------------
bios_globe <- terra::aggregate(bios[[covars]], fact = 10) %>% 
  terra::mask(worldmap)
plot(bios_globe)


# predict on rasters
tmp <- Sys.time()
pred_glob_brt <- raster::predict(
  object = raster::stack(bios_globe),
  model = brt,
  n.trees = brt$gbm.call$best.trees,
  progress = "text",
  type = "response"
)
Sys.time() - tmp
names(pred_glob_brt) <- "BRT"
plot(pred_glob_brt, zlim = c(0,1))

pred_glob_max <- raster::predict(
  object = raster::stack(bios_globe[[covars]]),
  model = maxmod,
  progress = "text",
  type = c("cloglog")
)
names(pred_glob_max) <- "Maxent"
plot(pred_glob_max, zlim = c(0,1))

# predicting glment on rasters with myspatial package
pred_glob_glm <- predict_glmnet_raster(
  r = raster::stack(bios_globe[[covars]]),
  model = lasso_cv, # the lasso cv object
  quadraticObj = quad_obj, # make_quadratic object
  type = "response",
  # slambda = "lambda.min"
  slambda = "lambda.1se"
)
names(pred_glob_glm) <- "GLM"
plot(pred_glob_glm, zlim = c(0,1))

pred_glob_gam <- raster::predict(
  object = raster::stack(bios_globe[[covars]]),
  model = gm,
  progress = "text",
  type = "response"
)
names(pred_glob_gam) <- "GAM"
plot(pred_glob_gam, zlim = c(0,1))



# predict to raster layers
pred_glob_rf <- raster::predict(
  object = raster::stack(bios_globe[[covars]]),
  model = rf_shallow_tuned,
  progress = "text",
  fun = function(model, ...) predict(model, ...)$predictions[,"1"]
)
names(pred_glob_rf) <- "RF"
plot(pred_glob_rf, zlim = c(0,1))

# stack all raster predictions
all_pred_glob <- list(pred_glob_brt,
                      pred_glob_glm, 
                      pred_glob_gam,
                      pred_glob_max, 
                      pred_glob_rf) %>% 
  lapply(function(x) raster::calc(x, function(y) scales::rescale(y, c(0,1)))) %>% 
  raster::stack() %>% 
  terra::rast()
plot(all_pred_glob)

pred_glob_ens <- terra::app(all_pred_glob, fun = "mean")
names(pred_glob_ens) <- "Ensemble"
plot(pred_glob_ens)

# the end -----------------------------------------------------------------
