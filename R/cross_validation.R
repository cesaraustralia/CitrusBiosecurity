library(ranger)
library(glmnet)
library(myspatial)
library(dismo)
library(gbm)

source("R/tune_ranger.R")



train_set <- cvfolds
test_set <- cvfold

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

# model 2 ****************************
quad_obj <- make_quadratic(train_set, cols = covars)
train_set_quad <- predict(quad_obj, newdata = train_set)
quad_obj

new_vars <- names(train_set_quad)[names(train_set_quad) != "occ"]
train_set_sparse <- sparse.model.matrix(~. -1, train_set_quad[, new_vars])

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

# model 4 ****************************
form <- paste(
  "occ ~", paste(paste0("s(", covars, ", bs  = 'tp', k = ", 5, ")"), collapse = " + ")
)

model4 <- mgcv::gam(formula = as.formula(form),
                    data = train_set,
                    family = binomial(link = "logit"),
                    weights = wt,
                    method = "REML")

# model 5 ****************************
model5 <- tune_ranger(data = train_set,
                      y = "occ",
                      max.depth = 2:8,
                      splitrule = c("hellinger", 'gini'),
                      num.trees = c(1000),
                      threads = 8)

# ensemble ***************************








