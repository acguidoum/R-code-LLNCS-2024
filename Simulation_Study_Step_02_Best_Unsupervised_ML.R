## Algorithm 5: Evaluation of Unsupervised ML Models for Bandwidth Prediction

# Separate data into features (X) and target (Y) 
# dataset with important features and bw

X <- dataset[, !"bw", with = FALSE]
Y <- dataset$bw

# scaled data
preProcValues <- caret::preProcess(X, method = c("center", "scale"))
X_scaled <- predict(preProcValues, X)

# Split data into training and testing sets
set.seed(123)
trainIndex <- caret::createDataPartition(Y, p = .7, list = FALSE)
X_train <- X_scaled[trainIndex, ]
Y_train <- Y[trainIndex]
X_test <- X_scaled[-trainIndex, ]
Y_test <- Y[-trainIndex]

# Define the training control
fitControl <- caret::trainControl(
  method = "cv",
  number = 15,
  returnResamp = "all",
  savePredictions = 'all',
  allowParallel = TRUE,
  verboseIter = TRUE,
  search = "random"
)


##################################################################
##############       Ridge Regression model          #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Ridge Regression model
ridge_model <- train(x = X_train,y = Y_train,method = "ridge",
                     trControl = fitControl,tuneLength = 10,
                     metric = "RMSE")
ridge_model
# Predictions on test
predictions <- predict(ridge_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
##############       Lasso Regression model          #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Lasso Regression model
lasso_model <- train(x = X_train,y = Y_train,method = "lasso",
                     trControl = fitControl,tuneLength = 10,
                     metric = "RMSE")
lasso_model
# Predictions on test
predictions <- predict(lasso_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
######    Bayesian Ridge Regression (Model Averaged) #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Non-Negative Least Squares Model
ABridge_model <- train(x = X_train, y = Y_train, method = "blassoAveraged",
                       trControl = fitControl, tuneLength = 10,
                       metric = "RMSE")
ABridge_model
# Predictions on test
predictions <- predict(ABridge_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
##############      Gradient Boosting Regression     #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Gradient Boosting Regression
GBM_model <- train(x = X_train,y = Y_train,method = "gbm",
                   trControl = fitControl,tuneLength = 10,
                   metric = "RMSE", verbose = FALSE)
GBM_model
# Predictions on test
predictions <- predict(GBM_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
##############    Support Vector Machine (SVM)       #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Support Vector Machine Regression
SVM_model <- train(x = X_train,y = Y_train,method = "svmRadial",
                   trControl = fitControl,tuneLength = 10,
                   metric = "RMSE", verbose = FALSE)
SVM_model
# Predictions on test
predictions <- predict(SVM_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
#########     Neural Network Regression (nnet)       #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Neural Network Regression
nnet_model <- train(x = X_train, y = Y_train, method = "nnet",
                    trControl = fitControl, tuneLength = 10,
                    metric = "RMSE", linout = TRUE, trace = FALSE)
nnet_model
# Predictions on test
predictions <- predict(nnet_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
#########  Model Averaged Neural Network (avNNet)    #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Model Averaged Neural Network
avNNet_model <- train(x = X_train, y = Y_train, method = "avNNet",
                      trControl = fitControl, tuneLength = 10,
                      metric = "RMSE", linout = TRUE, trace = FALSE)
avNNet_model
# Predictions on test
predictions <- predict(avNNet_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
###### Neural Networks with Feature Extraction (pcaNNet) #########

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Neural Networks with Feature Extraction
pcaNNet_model <- train(x = X_train, y = Y_train, method = "pcaNNet",
                       trControl = fitControl, tuneLength = 10,
                       metric = "RMSE", trace = FALSE)
pcaNNet_model
# Predictions on test
predictions <- predict(pcaNNet_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
#########       Elastic Net Regression (enet)        #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Elastic Net Regression
enet_model <- train(x = X_train, y = Y_train, method = "enet",
                    trControl = fitControl, tuneLength = 10,
                    metric = "RMSE")
enet_model
# Predictions on test
predictions <- predict(enet_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

##################################################################
######### Principal Component Analysis (pcr)         #############

set.seed(10000)
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
# Principal Component Analysis
pcr_model <- train(x = X_train, y = Y_train, method = "pcr",
                   trControl = fitControl, tuneLength = 10,
                   metric = "RMSE")
pcr_model
# Predictions on test
predictions <- predict(pcr_model, X_test)
# performances
results <- postResample(pred = predictions, obs = Y_test)
results
stopCluster(cl)
registerDoSEQ()

############################################
############################################
## Run resamples() to compare the models 
############################################

# Compare model performances using resample()
# Assuming the models are stored in a list
models_list <- list(Ridge = ridge_model, 
                    Lasso = lasso_model,
                    ABridge = ABridge_model, 
                    GBM = GBM_model, 
                    SVM = SVM_model, 
                    nnet = nnet_model, 
                    avNNet = avNNet_model, 
                    pcaNNet = pcaNNet_model,
                    enet = enet_model, 
                    PCAr = pcr_model)
models_compare <- caret::resamples(list(Ridge=ridge_model, Lasso=lasso_model,
                                        ABridge = ABridge_model, GBM = GBM_model, 
                                        SVM = SVM_model, nnet = nnet_model, 
                                        avNNet = avNNet_model, pcaNNet = pcaNNet_model,
                                        enet = enet_model, PCAr = pcr_model))
# Extract summary of the resampling results
summary_models <- summary(models_compare)

# Find the best model based on RMSE (minimum value)
best_rmse_model <- summary_models$statistics$RMSE
best_rmse_model_name <- rownames(best_rmse_model)[which.min(best_rmse_model[, "Mean"])]

# Find the best model based on R2 (maximum value)
best_r2_model <- summary_models$statistics$Rsquared
best_r2_model_name <- rownames(best_r2_model)[which.max(best_r2_model[, "Mean"])]

#
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
dotplot(models_compare, scales=scales)
bwplot(models_compare, scales=scales, metric = c("RMSE", "Rsquared"))

#
best_model <- models_list[[best_rmse_model_name]]
best_model
