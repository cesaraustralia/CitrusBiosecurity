library(tidyverse)
library(leaflet)
library(geodata)
library(terra)
library(sf)


# reading/downloading data ------------------------------------------------
# test a species
source("R/china_data.R")

gbif_data <- geodata::sp_occurrence(genus = "Diaphorina",
                                    species = "citri")

sp_coords <- gbif_data %>%
  dplyr::select(lon, lat, status = occurrenceStatus,
                country, species, genus, family) %>%
  drop_na(lon, lat)

sp_pts <- st_as_sf(sp_coords, coords = c("lon", "lat"))
# bg_mask <- st_read("data/background_mask.gpkg")

leaflet() %>%
  addTiles() %>%
  # addPolygons(opacity = 0.4, color = NA) %>%
  addCircleMarkers(
    data = sp_pts,
    radius = 4,
    stroke = FALSE,
    color = "red",
    label = ~species,
    fillOpacity = 0.4) %>%
  addCircleMarkers(
    data = china_data,
    radius = 4,
    stroke = FALSE,
    color = "blue",
    label = ~species,
    fillOpacity = 0.4)


# combine both datasets
sp_points <- sp_coords %>% 
  filter(status == "PRESENT") %>% 
  dplyr::select(species, long = lon, lat) %>% 
  bind_rows(china_data) %>% 
  st_as_sf(coords = c("long", "lat"))

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = sp_points,
    radius = 4,
    stroke = FALSE,
    color = "red",
    label = ~species,
    fillOpacity = 0.4)

# environmental data ------------------------------------------------------
r <- rast("C:/Users/61423/Climate_data/CHELSA_1981_2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif")
plot(r)
plot(sp_points, add = TRUE, col = 'black')

cellFromXY(r, st_coordinates(sp_points))


# modelling ---------------------------------------------------------------
# # BRT ---------------------------------------------------------------------
# weighting
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

set.seed(32755)
brt <- gbm.step(data = training,
                gbm.x = which(names(training) %in% final_vars),
                gbm.y = which(names(training) == "occ"),
                family = "bernoulli",
                tree.complexity = ifelse(prNum < 50, 1, 5),
                learning.rate = 0.001,
                bag.fraction = 0.75,
                max.trees = 10000,
                n.trees = 50,
                n.folds = 5,
                site.weights = wt,
                silent = FALSE)


# plot all three interaction levels
varimp <- summary(brt)

ggplot(aes(y = rel.inf, x = reorder(var, rel.inf), fill = rel.inf),
       data = varimp) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(label = paste0(round(rel.inf, 2), "")),
            color = "gray5", size = 3.5, position = position_nudge(y = + 3.5)) +
  viridis::scale_fill_viridis(option = "C", direction = -1) +
  labs(y = "Relative influence (%)", x = "Variables") +
  guides(fill = "none") +
  theme_classic()

# response curves
ggResponse(models = brt,
           # type = "response",
           covariates = training[,final_vars]
)

dismo::gbm.interactions(brt)

# predict on rasters
brt_pred <-  raster::predict(object = raster::stack(r[[final_vars]]),
                             model = brt,
                             n.trees = brt$gbm.call$best.trees,
                             progress = "text",
                             type = "response")

names(brt_pred) <- "BRT"
plot(brt_pred, zlim = c(0,1))
points(occ)

writeRaster(brt_pred, "Results/brt_current_225.tif", overwrite = TRUE)


# # Lasso -------------------------------------------------------------------
# loading the library
library(glmnet)
library(myspatial)


quad_obj <- make_quadratic(training, cols = final_vars)
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

# predicting glment on rasters with myspatial package
lasso_pred <- predict_glmnet_raster(
  r = raster::stack(r[[final_vars]]),
  model = lasso_cv, # the lasso cv object
  quadraticObj = quad_obj, # make_quadratic object
  type = "response",
  slambda = "lambda.min"
  # slambda = "lambda.1se"
)

names(lasso_pred) <- "Lasso"
plot(lasso_pred, zlim = c(0,1))
points(occ)

writeRaster(lasso_pred, "Results/lasso_current_225.tif", overwrite = TRUE)


# # GAM ---------------------------------------------------------------------
# loading the packages
library(mgcv)

# calculating the case weights (equal weights)
# the order of weights should be the same as presences and backgrounds in the training data
prNum <- as.numeric(table(training$occ)["1"]) # number of presences
bgNum <- as.numeric(table(training$occ)["0"]) # number of backgrounds
wt <- ifelse(training$occ == 1, 1, prNum / bgNum)

form <- occ ~ s(bio4, bs  = "tp", k = 10) +
  s(bio6, bs  = "tp", k = 10) +
  s(bio12, bs  = "tp", k = 10) +
  s(gsl, bs  = "tp", k = 10) +
  s(scd, bs  = "tp", k = 10) +
  s(topo_diversity, bs  = "tp", k = 10) +
  s(slope_position11, bs  = "tp", k = 10)
  # s(bio6, bio12)

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

gam_pred <- raster::predict(object = raster::stack(r[[final_vars]]),
                            model = gm,
                            progress = "text",
                            type = "response")
names(gam_pred) <- "GAM"
plot(gam_pred, zlim = c(0,1))

writeRaster(gam_pred, "Results/gam_current_225.tif", overwrite = TRUE)

# # Maxent ------------------------------------------------------------------
# load the package
library(dismo)


tmp <- Sys.time()
set.seed(32639)
# fit a maxent model with the tuned parameters
maxmod <- dismo::maxent(x = training[, final_vars],
                        p = training$occ,
                        removeDuplicates = FALSE,
                        path = "output/maxent_files",
                        args = c("nothreshold"))
Sys.time() - tmp

maxnet_pred <- raster::predict(object = raster::stack(r[[final_vars]]),
                               model = maxmod,
                               progress = "text",
                               type = c("cloglog"))
names(maxnet_pred) <- "Maxent"
plot(maxnet_pred, zlim = c(0,1))

writeRaster(maxnet_pred, "Results/max_current_225.tif", overwrite = TRUE)


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


rf_pred <- raster::predict(object = raster::stack(r[[final_vars]]),
                           model = rf_downsample,
                           progress = "text",
                           index = 2,
                           type = "prob")
names(rf_pred) <- "RF"
plot(rf_pred, zlim = c(0,1))

writeRaster(rf_pred, "Results/rf_current_225.tif", overwrite = TRUE)


# # -------------------------------------------------------------------------
# # Ensemble ----------------------------------------------------------------
# stack all raster predictions
all_pred <- raster::stack(brt_pred, lasso_pred, gam_pred, maxnet_pred, rf_pred) |>
  terra::rast()
plot(all_pred)

# ## rescaling prediction would be wrong as might reduce the effects of
# ## reduction in future projections **********
# predictions <- all_pred |>
#   as.list() |>
#   lapply(FUN = terra::app, fun = function(x){scales::rescale(x, to = c(0, 1))}) |>
#   rast() |>
#   setNames(names(all_pred))

ens_pred <- terra::app(all_pred, fun = "mean")
names(ens_pred) <- "Ensemble"
plot(ens_pred)

sd_pred <- terra::app(all_pred, fun = "sd")
plot(sd_pred, col = c("white", viridis::viridis(30, option = "A", direction = -1)))


par(mfrow=c(2,3))
for(i in 1:nlyr(all_pred)){
  plot(all_pred[[i]], main = names(all_pred)[i])
}
plot(sd_pred, main = "Standard Deviation",
     col = c("white", viridis::viridis(30, option = "A", direction = -1)))
par(mfrow=c(1,1))






