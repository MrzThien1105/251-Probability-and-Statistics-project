library(caTools)
library(pscl)
library(caret)
library(ROCR)


# Make this reproducible
set.seed(1)

data <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",")

split <- sample.split(data, SplitRatio = 0.8)
split

training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")

model <- glm(Is_Ad ~ ., training, family = "binomial")

probs <- predict(model, newdata = testing, type = "response")

# Create a prediction object
pred <- prediction(probs, testing$Is_Ad)

# Calculate performance for different thresholds (e.g., F1 score)
perf <- performance(pred, "f")

# Find the threshold with the maximum F1 score
f1_scores <- perf@y.values[[1]]
thresholds <- perf@x.values[[1]]
best_index <- which.max(f1_scores)
best_threshold <- thresholds[best_index]
cat("Best threshold (max F1):", best_threshold, "\n")

# You can also plot F1 vs threshold
print(plot(thresholds, f1_scores, type = "l", xlab = "Threshold", ylab = "F1 Score"))

print(table(ActualValue = testing$Is_Ad, PredictedValue = probs > best_threshold))
abline(v = best_threshold, col = "red", lty = 2)

print(summary(model))
