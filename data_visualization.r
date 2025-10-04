data2 <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",") # Read the preprocessed data
colnames(data2) <- c("Height", "Width", "Aspect_ratio", "Local", "is_Ad") # Rename columns for easier reference

# #Documentation for functions used
# ?hist
# ?par
# ?pairs
# ?ggplot #must include ggplot2 library
# ?plot
# ?boxplot
# ?x11    #Linux graphics device
# ?summary
# ?mean
# # pch stands for plotting character, cex stands for character expansion

summary(data2)

X11()        # Opens a plotting window on Linux
par(mfrow = c(4, 1))    # Put graphs in 4 rows and 1 column # par stands for parameter, mfrow = multi figure row-wise
# Plot histograms for Height, Width, Aspect_ratio, Local
hist(data2$Height[data2$is_Ad == 1], main = "Distribution of Height (Ads)", xlab = "Height", col = "#FF3700", border = "black")
hist(data2$Height[data2$is_Ad == 0], main = "Distribution of Height (Non-Ads)", xlab = "Height", col = "#0066CC", border = "black")
hist(data2$Width[data2$is_Ad == 1], main = "Distribution of Width (Ads)", xlab = "Width", col = "#FF3700", border = "black")
hist(data2$Width[data2$is_Ad == 0], main = "Distribution of Width (Non-Ads)", xlab = "Width", col = "#0066CC", border = "black")


X11()        # Opens a plotting window on Linux
par(mfrow = c(2, 1)) # Put graphs in 2 rows and 1 column
hist(data2$Aspect_ratio[data2$is_Ad == 1], main = "Distribution of Aspect Ratio (Ads)", xlab = "Aspect Ratio", col = "#FF3700", border = "black")
hist(data2$Aspect_ratio[data2$is_Ad == 0], main = "Distribution of Aspect Ratio (Non-Ads)", xlab = "Aspect Ratio", col = "#0066CC", border = "black")


# Scatter plot of Height vs Width, color-coded by is_Ad, with appropriate labels and title
X11()        # Opens a plotting window on Linux
par(mfcol = c(1, 2)) # Put graphs in 1 row and 2 columns, filled column-wise, square plotting area, no spaces between plots
plot(data2$Height[data2$is_Ad == 1], data2$Width[data2$is_Ad == 1], main = "Distribution relatively to height and width of ad", xlab = "Height", ylab = "Width", col = "#FF3700", pch = 20, cex = 1.5)
plot(data2$Height[data2$is_Ad == 0], data2$Width[data2$is_Ad == 0], main = "Distribution relatively to height and width of non-ad", xlab = "Height", ylab = "Width", col = "#0066CC", pch = 20, cex = 1.5)
?legend
#Pie chart for proportion of ads vs non-ads
X11()
prop_table <- prop.table(table(data2$is_Ad))
print(prop_table)
sizegap <- prop_table
names(sizegap) = c("Non-Ad", "Ad") # give names
pct <- round(sizegap*100)
lbls <- paste(names(sizegap), "\n", pct, "%", sep = "") # Add numbers to the pie chart
pie(sizegap, labels = lbls, col=c("#0066CC","#FF3700"), main="Proportion of Ads vs Non-Ads")   
# Add legend
legend("topright", legend = c("Non-Ad", "Ad"), fill = c("#0066CC", "#FF3700"))



X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Height ~ data2$is_Ad, main = "Boxplot of Height by Ad Type", xlab = "Ad Type", ylab = "Height", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700") )
X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Width ~ data2$is_Ad, main = "Boxplot of Width by Ad Type", xlab = "Ad Type", ylab = "Width", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700"))
X11()
par(mfcol = c(1, 1)) # Put graphs in 3 columns and 1 row
boxplot(data2$Aspect_ratio ~ data2$is_Ad, main = "Boxplot of Aspect Ratio by Ad Type", xlab = "Ad Type", ylab = "Aspect Ratio", names = c("Non-Ad", "Ad"), col = c("#0066CC", "#FF3700"))

# Scatter plot using pairs for better visualization
X11()        # Opens a plotting window on Linux
# Put graphs in 1 row and 1 column
panel.cor <- function(x, y, digits = 4, prefix = "", cex.cor, ...)
{
    par(usr = c(0, 1, 0, 1))
    r <- (cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2, vfont = c("sans serif", "bold"))
}
par(mfrow = c(1, 1))
pairs(data2[, c("Height", "Width", "Aspect_ratio")], upper.panel = panel.cor, col = ifelse(data2$is_Ad == 1, "#FF3700", "#0066CC"), gap = 1/10, pch = 20, cex = 1.5, main = "Scatterplot Matrix of Height, Width, and Aspect Ratio" )

# Summary statistics
summary_stats <- function(a) {
    var_name <- deparse(substitute(a))
    cat("Summary statistics for ", var_name, ":", sep = "")
    # summary(a)  
    cat("\n- Mean:", mean(a))
    cat("\n- Median:", median(a))
    cat("\n- Min:", min(a))
    cat("\n- Max:", max(a))
    cat("\n- Quantiles:", quantile(a))
    cat("\n- Standard Deviation:", sd(a))
    cat("\n- Variance:", var(a), "\n")
}
summary_stats(data2$Height)
summary_stats(data2$Width)
summary_stats(data2$Aspect_ratio)
#Alternative scatter plot using ggplot2 for better aesthetics
# X11()        # Opens a plotting window on Linux
# library(ggplot2)
# ggplot(data2, aes(x = Height, y = Width, color = factor(is_Ad))) +
#   geom_point(alpha = 0.6, size = 2) +
#   labs(title = "Height vs Width", x = "Height", y = "Width", color = "Ad Type") +
#   scale_color_manual(values = c("#0066CC", "#FF3700"), labels = c("Non-Ad", "Ad")) +
#   theme_minimal()

#TODO: Draw graphs in which median, variance, and standard deviation are clearly visible
#TODO: Graphs to represent the distribution of aspect ratios for ads vs non-ads
#TODO: Among horizontal and vertical ads, which type is more prevalent?
#TODO: Compare horizontal ads (aspect ratio > 1)to horizontal non-ads, and vertical(aspect ratio < 1) ads to vertical non-ads
#TODO: What is the average aspect ratio of ads vs non-ads?
#TODO: Are there any outliers in the dimensions of ads vs non-ads?
#TODO: Add legend to previous figures where applicable

