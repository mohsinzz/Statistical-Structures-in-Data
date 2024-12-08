workers_data <- read.csv("C:\\Users\\mohsi\\OneDrive\\Documents\\SSDassignment\\garments_worker_productivity.csv")

head(workers_data)

str(workers_data)

View(head(workers_data))

# 1. Data Overview
cat("Number of observations:", nrow(workers_data), "\n")
cat("Number of variables:", ncol(workers_data), "\n")

# 2. Summary Statistics for `actual_productivity`
summary(workers_data$actual_productivity)

mean_actual_productivity <- mean(workers_data$actual_productivity, na.rm = TRUE)
median_actual_productivity <- median(workers_data$actual_productivity, na.rm = TRUE)
sd_actual_productivity <- sd(workers_data$actual_productivity, na.rm = TRUE)
min_actual_productivity <- min(workers_data$actual_productivity, na.rm = TRUE)
max_actual_productivity <- max(workers_data$actual_productivity, na.rm = TRUE)

cat("Mean of actual productivity:", mean_actual_productivity, "\n")
cat("Median of actual productivity:", median_actual_productivity, "\n")
cat("Standard deviation of actual productivity:", sd_actual_productivity, "\n")
cat("Minimum of actual productivity:", min_actual_productivity, "\n")
cat("Maximum of actual productivity:", max_actual_productivity, "\n")

# 3. Distribution Visualization for `actual_productivity`
# Histogram
hist(workers_data$actual_productivity, 
     main = "Histogram of Actual Productivity", 
     xlab = "Actual Productivity", 
     col = "lightblue", 
     border = "black")

# Boxplot
boxplot(workers_data$actual_productivity, 
        main = "Boxplot of Actual Productivity", 
        ylab = "Actual Productivity", 
        col = "lightgreen")

# 4. Categorical Variable Analysis: Department vs Actual Productivity
library(ggplot2)
ggplot(workers_data, aes(x = department, y = actual_productivity)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Actual Productivity by Department",
       x = "Department",
       y = "Mean Actual Productivity") +
  theme_minimal()

# 5. Correlation Analysis between `actual_productivity` and `overtime`
cor_actual_vs_overtime <- cor(workers_data$actual_productivity, 
                              workers_data$over_time, 
                              use = "complete.obs")
cat("Pearson correlation coefficient between actual productivity and overtime:", cor_actual_vs_overtime, "\n")

# 6. ScatterPlot Visualization for `actual_productivity` and `overtime`
plot(workers_data$over_time, workers_data$actual_productivity, 
     main = "Scatter Plot of Overtime vs. Actual Productivity", 
     xlab = "Overtime", 
     ylab = "Actual Productivity", 
     pch = 19, col = "blue")
abline(lm(actual_productivity ~ over_time, data = workers_data), col = "red")


# 7. Multiple Regression
model <- lm(actual_productivity ~ smv + over_time + no_of_workers, data = workers_data)

model_summary <- summary(model)

coefficients_df <- as.data.frame(model_summary$coefficients)
View(coefficients_df)

residuals_df <- data.frame(residuals = residuals(model))

residuals_summary <- data.frame(
  Min = min(residuals_df$residuals),
  Q1 = quantile(residuals_df$residuals, 0.25),
  Median = median(residuals_df$residuals),
  Mean = mean(residuals_df$residuals),
  Q3 = quantile(residuals_df$residuals, 0.75),
  Max = max(residuals_df$residuals),
  Std_Dev = sd(residuals_df$residuals),
  IQR = IQR(residuals_df$residuals)
)

View(residuals_summary)

r_squared <- data.frame(
  R_Squared = model_summary$r.squared,
  Adjusted_R_Squared = model_summary$adj.r.squared
)
View(r_squared)

# 8. Model Diagnostics
# Residual plots
par(mfrow = c(2, 2))
plot(model)

# Breusch-Pagan test
library(lmtest)
bptest(model)

# Shapiro-Wilk test for normality
shapiro.test(residuals(model))

# 9. Principal Component Analysis
# Check for missing values
any(is.na(workers_data))

# Replace missing values with the mean
for (i in 1:ncol(workers_data)) {
  if (any(is.na(workers_data[, i]))) {
    workers_data[, i][is.na(workers_data[, i])] <- mean(workers_data[, i], na.rm = TRUE)
  }
}

# PCA on numeric columns
workers_data_numeric <- workers_data[, sapply(workers_data, is.numeric)]
pca_result <- prcomp(workers_data_numeric, scale. = TRUE)

par(mfrow = c(1, 1))
# Scree plot
plot(pca_result, type = "l", main = "Scree Plot of PCA")

#10. PCA interpretation Using Bi PLot
biplot(pca_result, main = "PCA Biplot", col = c("white", "red"),cex=0.8)
