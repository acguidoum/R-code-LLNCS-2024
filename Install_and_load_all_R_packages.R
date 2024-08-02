# List of packages to install and load
packages <- c("dplyr", "statip", "moments", "corrplot", "randomForest", 
              "data.table", "ggplot2", "doParallel", "caret", "glmnet", 
              "elasticnet", "gbm", "e1071", "nnet", "frbs", "pls", 
              "xgboost", "monomvn", "kedd","skimr")

# Function to install packages if they are not already installed
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  } else {
    library(p, character.only = TRUE)
  }
}

# Install and load all packages
lapply(packages, install_if_missing)
