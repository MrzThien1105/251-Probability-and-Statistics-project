data <- read.csv(file = "add.csv", header = TRUE, sep = ",")

# Drop the numbering column
# data <- data[, -1]

# Drop unrelated columns
data <- data[, -c(1, 6:1559)]

# Rename columns
colnames(data) <- c("Height", "Width", "Aspect_ratio", "Local", "Ad_prediction")

summary(data)

head(data)


# Replace ? with NA
data <- as.data.frame(lapply(data, function(data) ifelse(grepl("\\?", data), NA, data)))


# Convert Ad_prediction to binary value (0 = nonad, 1 = ad) (uses regex pattern matching)
data$Ad_prediction <- ifelse(data$Ad_prediction == "ad.", 1, 0)

table(data$Ad_prediction)

# Convert all values to numeric
data <- as.data.frame(sapply(data, as.numeric))

# Count number of NA
na_count <- sapply(data, function(x) sum(is.na(x)))
na_count

# Fill NA with median value (in Height, Width and Local)
for (col in c("Height", "Width")) {
        data[col] <- lapply(data[col], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
    }

# For Local, use the rounded mean
data["Local"] <- lapply(data["Local"], function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE)), x))


# Recalculate aspect ratio
data["Aspect_ratio"] <- round(data["Width"] / data["Height"], 4)

# Export preprocessed data
write.csv(data, "preprocessed.csv", row.names = FALSE)