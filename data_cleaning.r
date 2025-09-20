data <- read.csv(file = "add.csv", header = TRUE, sep = ",")

# Drop unrelated columns (including the numbering column)
data <- data[, -c(1, 5:1558)]

# Rename columns
colnames(data) <- c("Height", "Width", "Aspect_ratio", "Local", "Ad_prediction")

print(summary(data))
print("")

print(head(data))
print(tail(data))

# Replace ? with NA
data <- as.data.frame(lapply(data, function(data) ifelse(grepl("\\?", data), NA, data)))


# Convert Ad_prediction to binary value (0 = nonad, 1 = ad) (uses regex pattern matching)
data$Ad_prediction <- ifelse(gsub("^ad\\.$", "", data$Ad_prediction) == "", 1, 0)

# Convert all values to numeric
data <- as.data.frame(sapply(data, as.numeric))

# Count number of NA
print("Count the number of NA values in each column:")
na_count <- sapply(data, function(x) sum(is.na(x)))
print(na_count)

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