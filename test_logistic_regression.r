library(caTools)
library(pscl)
library(caret)
library(ROCR)


# Make this reproducible
set.seed(1)

data <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",")

split <- sample.split(data, SplitRatio = 0.8)

training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")

model0 <- glm(Is_Ad ~ 1, training, family = binomial()) # intercept only
model1 <- glm(Is_Ad ~ Height + Width + Aspect_ratio + Local, training,
    family = binomial()
)
model2 <- glm(Is_Ad ~ Local * (Height + Width + Aspect_ratio),
    training,
    family = binomial()
)

model3 <- glm(Is_Ad ~ Aspect_ratio * (Height + Width + Local),
    training,
    family = binomial()
)


print(summary(model1))
print(summary(model2))
print(summary(model3))


anova(model0, model1)
anova(model1, model2)
anova(model2, model3)

model_final <- glm(Is_Ad ~ Height + Width + Aspect_ratio + Aspect_ratio:Width,
    training,
    family = binomial()
)

anova(model3, model_final)

probs <- predict(model_final, newdata = testing, type = "response")

# Create a prediction object
pred <- prediction(probs, testing$Is_Ad)

#Calculate performance for different thresholds (e.g., accuracy score)
perf <- performance(pred, "acc")

# Find the threshold with the maximum F1 score
acc_scores <- perf@y.values[[1]]
thresholds <- perf@x.values[[1]]
best_index <- which.max(acc_scores)
best_threshold <- thresholds[best_index]
cat("Best threshold (max accuracy):", best_threshold, "\n")
# print(table(PredictedValue = ifelse(probs > best_threshold, 1, 0), ActualValue = testing$Is_Ad))


# plot accuracy vs threshold
# X11()
plot(thresholds, acc_scores, type = "l", xlab = "Threshold", ylab = "Accuracy")
abline(v = best_threshold, col = "red", lty = 2)
# Use this threshold to form the confusion matrix


library(caret)

# For model_final, using the best threshold
predicted <- as.factor(ifelse(probs > best_threshold, 1, 0))
actual <- as.factor(testing$Is_Ad)

# Print confusion matrix with statistics
cm <- confusionMatrix(predicted, actual, positive = "1")
print(cm)


library(ggplot2)

# Convert confusion matrix to data frame
cm_df <- as.data.frame(cm$table)
colnames(cm_df) <- c("Actual", "Predicted", "Freq")

# Plot confusion matrix as a heatmap
ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Confusion Matrix (Model 3) with default threshold (0.5)", x = "Actual", y = "Predicted") +
  theme_minimal()