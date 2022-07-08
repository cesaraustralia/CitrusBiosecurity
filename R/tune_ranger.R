# function for tuning the parameters of shallow model
tune_ranger <- function(data, 
                        y = "occ", # name of presence-background column
                        k = 5, # number of cross-validation folds
                        max.depth = 2:5, # this can be a vector of numbers
                        splitrule = c("gini", "extratrees", "hellinger"), # only allowed options
                        num.trees = 1000, # this can be a vector of numbers
                        mtry = NULL, # this can be a vector of numbers; NULL = default 
                        threads = 4, # number of CPU cores
                        progress = TRUE){ # show progress bar
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
  if(progress){
    pb <- progress::progress_bar$new(format = "Progress [:bar] :percent in :elapsed",
                                     total = iteration, clear = FALSE, width = 70)
  }
  message(sprintf("Initiating %s-fold cross-validation...", k))
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
    if(progress){
      pb$tick()
    }
  }
  bestparam <- which.max(evalmodel$aucme)
  message("Fitting the final model with:")
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
