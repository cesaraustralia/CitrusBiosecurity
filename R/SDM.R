library(tidyverse)
library(leaflet)
# library(dismo)
library(terra)
library(geodata)
library(rasterVis)
library(sf)
# library(factoextra)


# reading/downloading data ------------------------------------------------
# test a species
source("R/published_data.R")
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
dpmask <- terra::aggregate(bio1, fact = 10)
# remove duplicates
dup <- cellFromXY(dpmask, st_coordinates(sp_points)) %>% 
  duplicated()
occ <- sp_points[!dup, ]

elev <- rast("C:/Users/61423/Climate_data/Elevation_30s_worldclim/wc2.1_30s_elev.tif") %>% 
  setNames("elevation")
plot(elev)
plot(occ, add = TRUE, col = 'red')
# mapview::mapview(occ)


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

#
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
plot(bios[[1:4]])

# # take random points for PCA
# pca_samples <- terra::spatSample(x = bios,
#                                  size = 5e4,
#                                  method = "random",
#                                  xy = TRUE,
#                                  values = FALSE,
#                                  na.rm = TRUE)
# head(pca_samples)
# 
# pca_model <- terra::extract(bios, pca_samples) %>%
#   as.data.frame() %>%
#   prcomp(scale = TRUE, center = TRUE)
# 
# saveRDS(pca_model, "data/pca_model.rds")
pca_model <- readRDS("data/pca_model.rds")


factoextra::fviz_eig(pca_model)
factoextra::fviz_pca_var(
  pca_model,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE     # Avoid text overlapping
)


# read a raster mask for the region
bgmask <- terra::rast("data/mask.tif")

# kde_mask <- terra::rast("data/kde_mask.tif") %>% 
#   # terra::app(function(x) sqrt(x + 0.01)) %>% 
#   terra::mask(bgmask) %>%
#   terra::aggregate(fact = 5) %>%  # reduce the size to fit in memory
#   setNames("kde")
# 
# # generate 10k random samples from the KDE raster file
# bgs <- terra::spatSample(x = kde_mask,
#                          size = 1e4,
#                          method = "weights",
#                          # method = "random",
#                          xy = TRUE,
#                          values = FALSE,
#                          na.rm = TRUE)

# rasterVis::gplot(kde_mask) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_gradientn(colours = terrain.colors(30, rev = TRUE), na.value = NA) +
#   geom_point(data = as.data.frame(bgs), aes(x = x, y = y), alpha = 0.02) +
#   theme_void() +
#   coord_equal()


# saveRDS(bgs, "data/bgs.rds")
bgs <- readRDS("data/bgs.rds")

# extract the climatic data
bio_pr <- terra::extract(bios, vect(occ_clean)) %>% 
  as_tibble() %>% 
  mutate(occ = 1) %>% 
  dplyr::select(-ID)
bio_bg <- terra::extract(bios, bgs[,c("x","y")]) %>% 
  as_tibble() %>% 
  mutate(occ = 0)

train_data <- bind_rows(bio_pr, bio_bg) %>% 
  dplyr::relocate(occ) %>%  # get occ as first column
  dplyr::select(-ID)
names(train_data)

# covars <- c(
#   # "bio1",
#   # "bio2",
#   "bio3",
#   # "bio4",
#   "bio5",
#   # "bio6",
#   # "bio7",
#   "bio8",
#   # "bio10",
#   # "bio11",
#   # "bio12",
#   # "bio14",
#   "bio15",
#   # "bio17",
#   # "bio18",
#   # "cmi",
#   # "gdd0",
#   # "gdd10"
#   # "gsl"
#   # "npp"
#   "hurs"
#   # "vpd"
#   # "ngd0"
#   # "ngd10"
# )
# 
# usdm::vifstep(x = as.data.frame(train_data[, covars]), th = 10)
# 
# train_data[, covars] %>%
#   # dplyr::select(-occ) %>%
#   sample_n(5000) %>%
#   PerformanceAnalytics::chart.Correlation(method = "pearson")

# # covars <- paste0("pc", 1:5)
# 
# training <- train_data[, c("occ", covars)] %>% 
#   # mutate(
#   #   # bio18 = log(bio18 + 1),
#   #   # bio14 = log(bio14 + 1),
#   #   bio12 = log(bio12 + 1)
#   # ) %>%
#   as.data.frame()
# head(training)
# 
# training %>%
#   dplyr::select(-occ) %>%
#   usdm::vifstep(th = 5)
# 
# training %>%
#   dplyr::select(-occ) %>%
#   sample_n(5000) %>%
#   PerformanceAnalytics::chart.Correlation(method = "pearson")

pcs <- seq_len(11)[-c(5,6,8)]
covars <- paste0("PC", pcs)
covars

training <- predict(pca_model, train_data[,-1]) %>% 
  as.data.frame() %>% 
  dplyr::select(all_of(covars)) %>% 
  mutate(occ = train_data$occ) %>% 
  relocate(occ) %>% 
  drop_na()
head(training)
table(training$occ)
names(training)


#
# modelling ---------------------------------------------------------------
# # BRT ---------------------------------------------------------------------
library(dismo)
library(gbm)

# weighting
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

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

# dismo::gbm.interactions(brt)$rank.list

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
library(glmnet)
library(myspatial)


quad_obj <- make_quadratic(training, cols = covars)
training_quad <- predict(quad_obj, newdata = training)
quad_obj

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

#
# # GAM ---------------------------------------------------------------------
library(mgcv)

# calculating the case weights (equal weights)
# the order of weights should be the same as presences and backgrounds in the training data
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

form <- paste("occ ~", 
  paste(paste0("s(", covars, ", bs  = 'tp', k = ", 5, ")"), collapse = " + ")
)

gm <- mgcv::gam(formula = as.formula(form),
                data = training,
                family = binomial(link = "logit"),
                # family = binomial(link = "cloglog"),
                weights = wt,
                method = "REML")

# check the appropriateness of Ks
# gam.check(gm)
plot(gm, pages = 1, rug = TRUE, shade = TRUE)

#
# # Maxent ------------------------------------------------------------------
library(dismo)

maxmod <- dismo::maxent(x = training[, covars],
                        p = training$occ,
                        removeDuplicates = FALSE,
                        path = "output/maxent_files",
                        args = c("nothreshold", 
                                 # "noautofeature",
                                 # "nolinear", "noquadratic", "noproduct", # H
                                 # "noproduct", "nohinge", # LQ
                                 # "noproduct", # LQH
                                 "betamultiplier=2"))


predictors_au <- raster::stack(pca_au)[[covars]]
names(predictors_au)
# plot(pca_au)

pred_au5k_max <- raster::predict(
  object = predictors_au,
  model = maxmod,
  progress = "text",
  type = c("cloglog")
)
names(pred_au5k_max) <- "Maxent"
plot(pred_au5k_max, zlim = c(0,1))


#
# RF - ranger -------------------------------------------------------------
library(ranger)

source("R/tune_ranger.R")

rf_shallow_tuned <- tune_ranger(data = training,
                                y = "occ",
                                max.depth = 2:8,
                                splitrule = c("hellinger", 'gini'),
                                num.trees = c(1000),
                                threads = 8)


#
# save model objects ------------------------------------------------------
saveRDS(brt, "models/brt.rds")
saveRDS(lasso_cv, "models/glm.rds")
saveRDS(gm, "models/gam.rds")
saveRDS(maxmod, "models/max.rds")
saveRDS(rf_shallow_tuned, "models/rfs.rds")

#
# # -------------------------------------------------------------------------
# Global map --------------------------------------------------------------
bios_globe <- terra::aggregate(bios, fact = 10) %>% 
  terra::mask(worldmap)
plot(bios_globe)
plot(bios_globe[[17:nlyr(bios_globe)]])

pca_globe <- predict(bios_globe, pca_model)
plot(pca_globe)
# plot(pca_globe[[17:nlyr(pca_globe)]])
predictors_globe <- raster::stack(pca_globe)[[covars]]
plot(predictors_globe)

# predict on rasters
tmp <- Sys.time()
pred_glob_brt <- raster::predict(
  object = predictors_globe,
  model = brt,
  n.trees = brt$gbm.call$best.trees,
  progress = "text",
  type = "response"
)
Sys.time() - tmp
names(pred_glob_brt) <- "BRT"
plot(pred_glob_brt, zlim = c(0,1))


pred_glob_max <- raster::predict(
  object = predictors_globe,
  model = maxmod,
  progress = "text",
  type = c("cloglog")
)
names(pred_glob_max) <- "Maxent"
plot(pred_glob_max, zlim = c(0,1))


# predicting glment on rasters with myspatial package
pred_glob_glm <- predict_glmnet_raster(
  r = predictors_globe,
  model = lasso_cv, # the lasso cv object
  quadraticObj = quad_obj, # make_quadratic object
  type = "response",
  # slambda = "lambda.min"
  slambda = "lambda.1se"
)
names(pred_glob_glm) <- "GLM"
plot(pred_glob_glm, zlim = c(0,1))


pred_glob_gam <- raster::predict(
  object = predictors_globe,
  model = gm,
  progress = "text",
  type = "response"
)
names(pred_glob_gam) <- "GAM"
plot(pred_glob_gam)
plot(pred_glob_gam, zlim = c(0,1))


# predict to raster layers
pred_glob_rf <- raster::predict(
  object = predictors_globe,
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
plot(all_pred_glob, range = c(0,1))

pred_glob_ens <- terra::app(all_pred_glob, fun = "mean")
names(pred_glob_ens) <- "Ensemble"
plot(pred_glob_ens, range = c(0,1))


robproj <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
mycols <- terrain.colors(30, rev = TRUE)

ens_glob_proj <- terra::project(pred_glob_ens, robproj)

rasterVis::gplot(ens_glob_proj) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = mycols, na.value = NA) +
  geom_sf(data = st_as_sf(worldmap), fill = NA, inherit.aes = FALSE) +
  # geom_sf(data = st_as_sf(cabi_counties), fill = NA, col = "blue", inherit.aes = FALSE) +
  coord_sf(crs = robproj) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Suitability")

ggsave("results/global_map.jpg", 
       width = 2800,
       height = 2000, 
       units = "px",
       dpi = 300)

# Australian map ----------------------------------------------------------
# aggregate and crop to Australia
bios_au <- bios %>% 
  terra::crop(worldmap[worldmap$GID_0 == "AUS"]) %>% 
  terra::mask(worldmap[worldmap$GID_0 == "AUS"]) %>% 
  terra::aggregate(fact = 5)


# predictors_au <- bios_au
pca_au <- predict(bios_au, pca_model)
plot(pca_au)
# plot(pca_au[[17:nlyr(pca_au)]])
predictors_au <- raster::stack(pca_au)[[covars]]

# predict on rasters
pred_au5k_brt <- raster::predict(
  object = predictors_au,
  model = brt,
  n.trees = brt$gbm.call$best.trees,
  progress = "text",
  type = "response"
)
names(pred_au5k_brt) <- "BRT"
plot(rast(pred_au5k_brt), range = c(0,1), background = "blue")

# predicting glment on rasters with myspatial package
pred_au5k_glm <- predict_glmnet_raster(
  r = predictors_au,
  model = lasso_cv, # the lasso cv object
  quadraticObj = quad_obj, # make_quadratic object
  type = "response",
  # slambda = "lambda.min"
  slambda = "lambda.1se"
)
names(pred_au5k_glm) <- "GLM"
plot(pred_au5k_glm, zlim = c(0,1))

pred_au5k_gam <- raster::predict(
  object = predictors_au,
  model = gm,
  progress = "text",
  type = "response"
)
names(pred_au5k_gam) <- "GAM"
plot(pred_au5k_gam, zlim = c(0,1))



pred_au5k_max <- raster::predict(
  object = predictors_au,
  model = maxmod,
  progress = "text",
  type = c("cloglog")
)
names(pred_au5k_max) <- "Maxent"
plot(pred_au5k_max, zlim = c(0,1))



# predict to raster layers
pred_au5k_rf <- raster::predict(
  object = predictors_au,
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
plot(all_pred_au5k, range = c(0, 1))

pred_au5k_ens <- terra::app(all_pred_au5k, fun = "mean")
names(pred_au5k_ens) <- "Ensemble"
plot(pred_au5k_ens, range = c(0, 1))


mycols <- terrain.colors(30, rev = TRUE)

ens_au_proj <- terra::project(pred_au5k_ens, "epsg:3112")

rasterVis::gplot(ens_au_proj) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = mycols, na.value = NA) +
  # scale_fill_viridis_c(option = "G", direction = -1, na.value = NA) +
  geom_sf(data = st_as_sf(worldmap[worldmap$GID_0 == "AUS", ]),
          fill = NA, inherit.aes = FALSE) +
  # geom_sf(data = st_as_sf(cabi_counties), fill = NA, col = "blue", inherit.aes = FALSE) +
  scale_x_continuous(limits = c(-23e5, 22e5)) +
  scale_y_continuous(limits = c(-52e5, -1e6)) +
  coord_sf(crs = 3112) +
  theme_minimal() +
  labs(x = NULL, y = NULL, fill = "Suitability")

ggsave("results/australia_map.jpg", 
       width = 2000,
       height = 1500, 
       units = "px",
       dpi = 300)

# writeRaster(pred_au5k_brt, "results/pred_au5k_brt.tif", overwrite = TRUE)
# writeRaster(pred_au5k_glm, "Results/pred_au5k_glm.tif", overwrite = TRUE)
# writeRaster(pred_au5k_gam, "Results/pred_au5k_gam.tif", overwrite = TRUE)
# writeRaster(pred_au5k_max, "Results/pred_au5k_max.tif", overwrite = TRUE)
# writeRaster(pred_au5k_rf, "Results/pred_au5k_rf.tif", overwrite = TRUE)
# writeRaster(pred_au5k_ens, "Results/pred_au5k_ens.tif", overwrite = TRUE)


sd_au5k_ens <- terra::app(all_pred_au5k, fun = "sd")
plot(sd_au5k_ens, col = c("gray", viridis::viridis(30, option = "A", direction = +1)))


# the end -----------------------------------------------------------------
