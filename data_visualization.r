data2 <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",") # Read the preprocessed data
colnames(data2) <- c("Height", "Width", "Aspect_ratio", "Local", "is_Ad") # Rename columns for easier reference

#Documentation for functions used
?hist
?par
?pairs
?ggplot #must include ggplot2 library
?plot
?boxplot
?x11    #Linux graphics device
?summary
?mean
# pch stands for plotting character, cex stands for character expansion


X11()        # Opens a plotting window on Linux
par(mfrow = c(4, 1))    # Put graphs in 4 rows and 1 column # par stands for parameter, mfrow = multi figure row-wise
# Plot histograms for Height, Width, Aspect_ratio, Local
hist(data2$Height[data2$is_Ad == 1], main = "Distribution of Height (Ads)", xlab = "Height", col = "lightblue", border = "black")
hist(data2$Height[data2$is_Ad == 0], main = "Distribution of Height (Non-Ads)", xlab = "Height", col = "violet", border = "black")
hist(data2$Width[data2$is_Ad == 1], main = "Distribution of Width (Ads)", xlab = "Width", col = "lightblue", border = "black")
hist(data2$Width[data2$is_Ad == 0], main = "Distribution of Width (Non-Ads)", xlab = "Width", col = "violet", border = "black")


X11()        # Opens a plotting window on Linux
par(mfrow = c(2, 1)) # Put graphs in 2 rows and 1 column
hist(data2$Aspect_ratio[data2$is_Ad == 1], main = "Distribution of Aspect Ratio (Ads)", xlab = "Aspect Ratio", col = "yellow", border = "black")
hist(data2$Aspect_ratio[data2$is_Ad == 0], main = "Distribution of Aspect Ratio (Non-Ads)", xlab = "Aspect Ratio", col = "violet", border = "black")


# Scatter plot of Height vs Width, color-coded by is_Ad, with appropriate labels and title
X11()        # Opens a plotting window on Linux
par(mfrow = c(1, 1)) # Put graphs in 1 row and 1 column
plot(data2$Height, data2$Width, main = "Height vs Width", xlab = "Height", ylab = "Width", col = ifelse(data2$is_Ad == 1, "blue", "red"), pch = 20, cex = 1.5)
legend("topright", legend = c("Ad", "Non-Ad"), col = c("blue", "red"), pch = 20)

# Summary statistics
summary(data2$Height)
summary(data2$Width)
summary(data2$Aspect_ratio)
summary(data2$Local)
summary(data2$is_Ad)



#TODO: Are there any outliers in the dimensions of ads vs non-ads?
boxplot(data2$Height ~ data2$is_Ad, main = "Boxplot of Height by Ad Type", xlab = "Ad Type", ylab = "Height")
boxplot(data2$Width ~ data2$is_Ad, main = "Boxplot of Width by Ad Type", xlab = "Ad Type", ylab = "Width")
boxplot(data2$Aspect_ratio ~ data2$is_Ad, main = "Boxplot of Aspect Ratio by Ad Type", xlab = "Ad Type", ylab = "Aspect Ratio")


# Scatter plot using pairs for better visualization
X11()        # Opens a plotting window on Linux
# Put graphs in 1 row and 1 column
par(mfrow = c(1, 1))
pairs(data2[, c("Height", "Width", "Aspect_ratio", "Local", "is_Ad")], col = ifelse(data2$is_Ad == 1, "blue", "red"))

#Alternative scatter plot using ggplot2 for better aesthetics
library(ggplot2)
ggplot(data2, aes(x = Height, y = Width, color = factor(is_Ad))) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Height vs Width", x = "Height", y = "Width", color = "Ad Type") +
  scale_color_manual(values = c("red", "blue"), labels = c("Non-Ad", "Ad")) +
  theme_minimal()

#TODO: Draw graphs in which median, variance, and standard deviation are clearly visible
#TODO: Graphs to represent the distribution of aspect ratios for ads vs non-ads
#TODO: Among horizontal and vertical ads, which type is more prevalent?
#TODO: Compare horizontal ads (aspect ratio > 1)to horizontal non-ads, and vertical(aspect ratio < 1) ads to vertical non-ads
#TODO: What is the average aspect ratio of ads vs non-ads?
mean(data2$Aspect_ratio[data2$is_Ad == 1])
mean(data2$Aspect_ratio[data2$is_Ad == 0])

#TODO: Add legend to previous figures where applicable

