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
