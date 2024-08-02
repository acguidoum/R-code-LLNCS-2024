############################################################
# Simulation Study
############################################################

set.seed(123)
original_sample <- rweibull(n = 500, shape = 2, scale = 4)
m <- 10000 # num_samples

## Algorithm 1: Generation of Synthetic Samples

samples_bootstrap <- syn_samples(original_sample, method = 2, num_samples = m)
head(samples_bootstrap)

## Algorithm 2: Calculation of Statistical Parameters (Factors) 

factors <- stat_para(samples_bootstrap)
factors

## Algorithm 3: Calculation of Bandwidth (bw) for Synthetic Samples with Fixed Kernel

Ker = "triweight"
bw <- sapply(seq_along(samples_bootstrap), function(i) 
  MISE_bw(samples_bootstrap[[i]], kernel = Ker))
bw
#skimmed <- skimr::skim(bw)
#skimmed

## Algorithm 4: Feature Importance Evaluation for Bandwidth

# Step 1: Prepare the data
data <- data.table::data.table(factors,bw)
cols <- names(data)[sapply(data, is.numeric)] 
data[, (cols) := lapply(.SD, round, 6), .SDcols = cols]
data
anyNA(data)
X <- data[, !"bw", with = FALSE]  
Y <- data[, bw] 

# Step 2: Train a Random Forest Model

set.seed(123)  # Set a random seed for reproducibility
rf_model <- randomForest(X, Y, importance = TRUE, proximity=TRUE, 
                         type ="unsupervised") # Train the random forest model
rf_model  # Display the random forest model

# Step 3: Evaluate Feature Importance

importance_scores <- importance(rf_model, type = 1)  # Type 1 for mean decrease in accuracy
importance_scores
threshold <- quantile(importance_scores,probs = 0.25)[[1]]
threshold
important_features <- rownames(importance_scores)[importance_scores >= threshold]
important_features

# plot of feature importance

importance_scores_df <- data.frame(
  Feature = rownames(importance_scores),
  Importance = importance_scores[, 1]
)
ggplot(importance_scores_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = threshold), linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Feature Importance",
       x = "Features",
       y = "Importance (Mean Decrease in Accuracy)") +
  annotate("text", x = -1, y = threshold, label = paste("Threshold =", round(threshold, 2)), color = "red", vjust = -1) +
  theme_minimal()


# New dateset with important features

dataset <- data[,c(important_features,"bw"), with = FALSE]
head(dataset)

# Correlate matrix

cor_numVar <- cor(dataset, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(cor_numVar[,'bw'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) >= 0)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot::corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt",tl.cex=0.8,
                         number.cex=0.6,addCoef.col=1,lower = 'ellipse', 
                         upper = 'ellipse')

