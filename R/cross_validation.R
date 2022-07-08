library(ranger)
library(mgcv)
library(glmnet)
library(myspatial)
library(dismo)
library(gbm)

source("R/tune_ranger.R")


# pcs <- seq_len(15)[-c(5,6,8)]
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
covars




# create balanced 5-fold
set.seed(42)
folds <- caret::createFolds(y = as.factor(training$occ), k = 5)


modauc <- c()
modboyce <- c()
for(i in seq_len(5)){
  
  train_set <- training[unlist(folds[-i]), ]
  test_set <- training[unlist(folds[i]), ]
  
  # model 1 ****************************
  prNum <- as.numeric(table(train_set$occ)["1"]) # number of presences
  bgNum <- as.numeric(table(train_set$occ)["0"]) # number of backgrounds
  wt <- ifelse(train_set$occ == 1, 1, prNum / bgNum)
  
  model1 <- gbm.step(data = train_set,
                     gbm.x = which(!names(train_set) %in% "occ"),
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
  cat("*")
  
  # model 2 ****************************
  quad_obj <- make_quadratic(train_set, cols = covars)
  train_set_quad <- predict(quad_obj, newdata = train_set)
  testing_quad <- predict(quad_obj, newdata = test_set)
  
  new_vars <- names(train_set_quad)[names(train_set_quad) != "occ"]
  train_set_sparse <- sparse.model.matrix(~. -1, train_set_quad[, new_vars])
  testing_sparse <- sparse.model.matrix( ~. -1, testing_quad[, new_vars])
  
  # calculating the case weights
  prNum <- as.numeric(table(train_set_quad$occ)["1"]) # number of presences
  bgNum <- as.numeric(table(train_set_quad$occ)["0"]) # number of backgrounds
  wt <- ifelse(train_set_quad$occ == 1, 1, prNum / bgNum)
  
  model2 <- cv.glmnet(x = train_set_sparse,
                      y = train_set_quad$occ,
                      family = "binomial",
                      alpha = 1, # fitting lasso
                      weights = wt,
                      nfolds = 10)
  plot(model2)
  cat("*")
  
  # model 3 ****************************
  model3 <- dismo::maxent(x = train_set[, covars],
                          p = train_set$occ,
                          removeDuplicates = FALSE,
                          path = "output/maxent_files",
                          args = c("nothreshold", 
                                   # "noautofeature",
                                   # "nolinear", "noquadratic", "noproduct", # H
                                   # "noproduct", "nohinge", # LQ
                                   # "noproduct", # LQH
                                   "betamultiplier=2"))
  cat("*")
  
  # model 4 ****************************
  form <- paste(
    "occ ~", paste(paste0("s(", covars, ", bs  = 'tp', k = ", 5, ")"), collapse = " + ")
  )
  
  model4 <- mgcv::gam(formula = as.formula(form),
                      data = train_set,
                      family = binomial(link = "logit"),
                      weights = wt,
                      method = "REML")
  cat("*")
  
  # model 5 ****************************
  model5 <- tune_ranger(data = train_set,
                        y = "occ",
                        max.depth = 2:8,
                        splitrule = c("hellinger", 'gini'),
                        num.trees = c(1000),
                        threads = 8)
  cat("*")
  
  # ensemble ***************************
  predictions <- data.frame(
    pred1 = predict(model1, test_set, n.trees = model1$gbm.call$best.trees, type = "response"), # brt
    pred2 = predict(model2, testing_sparse, type = "response", s = "lambda.1se"), # glm
    pred3 = predict(model3, test_set, type = c("cloglog")), # max
    pred4 = predict(model4, test_set, type = "response"), # gam
    pred5 = predict(model5, test_set, type = "response")$predictions[,"1"] # rng
  ) %>% 
    mutate_all(.funs = function(y) scales::rescale(y, c(0,1)))
  cat("*\n")
  
  
  predictions_ens <- apply(predictions, 1, mean)
  
  modauc[i] <- precrec::auc(precrec::evalmod(scores =predictions_ens, 
                                             labels = test_set$occ))[1,4]
  
  # Calculate Boyce index
  pres_indx <- which(test_set$occ == 1)
  modboyce[i] <- ecospat::ecospat.boyce(fit = predictions_ens,
                                        obs = predictions_ens[pres_indx],
                                        PEplot = FALSE)$cor
  
}

modauc
modboyce

mean(modauc)
mean(modboyce)

# PCs-11
# > modauc
# [1] 0.7861459 0.8182557 0.8010683 0.7869186 0.7475951
# > modboyce
# [1] 0.958 0.975 0.977 0.971 0.940
# > mean(modauc)
# [1] 0.7879967
# > mean(modboyce)
# [1] 0.9642

# PCs-15
# > mean(modauc)
# [1] 0.7994154
# > mean(modboyce)
# [1] 0.9674

