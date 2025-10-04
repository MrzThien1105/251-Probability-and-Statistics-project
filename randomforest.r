# --------------------------
# Random Forest Example
# --------------------------

options(device = "windows")

# Install if missing
# install.packages("randomForest")
# install.packages("caret")
# install.packages("ROCR")

library(randomForest)
library(caret)
library(ROCR)

set.seed(1)

# --------------------------
# Load Data
# --------------------------
data <- read.csv("preprocessed.csv", header = TRUE, sep = ",")
cat("Dataset loaded:", nrow(data), "rows,", ncol(data), "columns\n\n")


# Convert target to factor for classification
data$Is_Ad <- as.factor(data$Is_Ad)


# --------------------------
# Split train/test (80/20)
# --------------------------

# train_index <- sample(1:nrow(data), 0.8 * nrow(data))
# training <- data[train_index, ]
# testing  <- data[-train_index, ]
split <- sample.split(data, SplitRatio = 0.8)

training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")

cat("Training set size:", nrow(training), "rows\n")
cat("Testing set size:", nrow(testing), "rows\n\n")

# --------------------------
# Train Random Forest Model
# --------------------------
cat("Training Random Forest model...\n")

rf_model <- randomForest(
  Is_Ad ~ Aspect_ratio * (Height + Width + Local),                 # Target = Is_Ad, all others as predictors
  data = training,
  ntree = 300,               # Number of trees
  mtry = sqrt(ncol(training) - 1), # Features per split (common rule of thumb)
  importance = TRUE
)


cat("Random Forest model trained!\n\n")

# --------------------------
# Predictions
# --------------------------
probs <- predict(rf_model, testing, type = "prob")[, 2]  # predicted probabilities
predicted_labels <- ifelse(probs > 0.5, 1, 0)

# --------------------------
# Evaluate with F1 score
# --------------------------
pred <- prediction(probs, testing$Is_Ad)
perf <- performance(pred, "f")

f1_scores <- perf@y.values[[1]]
thresholds <- perf@x.values[[1]]
best_index <- which.max(f1_scores)
best_threshold <- thresholds[best_index]
best_f1 <- f1_scores[best_index]

cat("Best threshold (max F1):", best_threshold, "\n")
cat("Best F1 score:", best_f1, "\n\n")

# --------------------------
# Confusion Matrix
# --------------------------
predicted_labels <- ifelse(probs > best_threshold, 1, 0)
cm <- table(Actual = testing$Is_Ad, Predicted = predicted_labels)

cat("------ Confusion Matrix ------\n")
print(cm)
cat("\n")

# Extra metrics
TP <- cm[2, 2]; FP <- cm[1, 2]; TN <- cm[1, 1]; FN <- cm[2, 1]
accuracy  <- (TP + TN) / sum(cm)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * precision * recall / (precision + recall)

cat("Accuracy :", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall   :", round(recall, 3), "\n")
cat("F1 Score :", round(f1_score, 3), "\n\n")

# --------------------------
# caret Confusion Matrix
# --------------------------
cm_result <- confusionMatrix(
  as.factor(predicted_labels),
  as.factor(testing$Is_Ad),
  positive = "1"
)
print(cm_result)

# --------------------------
# Plot F1 vs Threshold
# --------------------------
windows()
plot(thresholds, f1_scores, type = "l",
     xlab = "Threshold", ylab = "F1 Score",
     main = "Random Forest: F1 Score vs Threshold")
abline(v = best_threshold, col = "red", lty = 2)

# --------------------------
# Feature Importance Plot
# --------------------------
windows()
varImpPlot(rf_model, main = "Random Forest Feature Importance")
