# --------------------------
# XGBoost Example
# --------------------------

options(device = "windows")

# Install if missing
# install.packages("xgboost")
# install.packages("caret")
# install.packages("ROCR")

library(xgboost)
library(caret)
library(ROCR)
library(caTools)

set.seed(1)

# Load data
data <- read.csv("preprocessed.csv", header = TRUE, sep = ",")
cat("Dataset loaded:", nrow(data), "rows,", ncol(data), "columns\n\n")

# Split train/test (80/20)
# train_index <- sample(1:nrow(data), 0.8 * nrow(data)) # nolint
# training <- data[train_index, ]
# testing  <- data[-train_index, ]

split <- sample.split(data, SplitRatio = 0.8)

training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")

cat("Training set size:", nrow(training), "rows\n")
cat("Testing set size:", nrow(testing), "rows\n\n")

# Prepare data for XGBoost (must be numeric matrices)
train_x <- as.matrix(training[, colnames(training) != "Is_Ad"])
train_y <- training$Is_Ad
test_x  <- as.matrix(testing[, colnames(testing) != "Is_Ad"])
test_y  <- testing$Is_Ad

# Build DMatrix (optimized XGBoost data structure)
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest  <- xgb.DMatrix(data = test_x, label = test_y)

# Train XGBoost model
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  max_depth = 8,   # make trees deeper or shallower (6)
  eta = 0.05,      # learning rate (smaller = slower but more accurate) (0.05)
  subsample = 0.8, # how much data to sample for each tree (0.8)
  colsample_bytree = 0.8 # how many features per tree (0.8)
)
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 500) #200 rounds


cat("XGBoost model trained!\n\n")

# Predictions
probs <- predict(xgb_model, test_x)

# Evaluate with F1 score at different thresholds
pred <- prediction(probs, test_y)
perf <- performance(pred, "f")

f1_scores <- perf@y.values[[1]]
thresholds <- perf@x.values[[1]]
best_index <- which.max(f1_scores)
best_threshold <- thresholds[best_index]
best_f1 <- f1_scores[best_index]

cat("Best threshold (max F1):", best_threshold, "\n")
cat("Best F1 score:", best_f1, "\n\n")

# Confusion Matrix
predicted_labels <- ifelse(probs > best_threshold, 1, 0)
actual <- as.factor(testing$Is_Ad);
cm <- table(Actual = test_y, Predicted = predicted_labels)
cat("------ Confusion Matrix ------\n")
print(cm)
cat("\n")

# Extra metrics
TP <- cm[2, 2]
FP <- cm[1, 2]
TN <- cm[1, 1]
FN <- cm[2, 1]

accuracy  <- (TP + TN) / sum(cm)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * precision * recall / (precision + recall)


cat("Accuracy :", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall   :", round(recall, 3), "\n")
cat("F1 Score :", round(f1_score, 3), "\n")

# Create confusion matrix using caret (requires factors)
cm_result <- confusionMatrix(
  as.factor(predicted_labels),
  as.factor(test_y),
  positive = "1"
)

# Display full results
print(cm_result)



# Plot F1 vs Threshold
windows()
plot(thresholds, f1_scores, type = "l",
     xlab = "Threshold", ylab = "F1 Score",
     main = "XGBoost: F1 Score vs Threshold")
abline(v = best_threshold, col = "red", lty = 2)

