
library(openxlsx)
gym_data <- read.xlsx("C:\\Users\\mohsi\\OneDrive\\Documents\\SSDassignment\\gym_data.xlsx")


# Overview of the dataset
View(head(gym_data))

# Check structure of the dataset
str(gym_data)

# Summary statistics for numerical columns
summary(gym_data$Age)
summary(gym_data$Weight_in_kg)
summary(gym_data$Height_in_m)
summary(gym_data$Max_BPM)
summary(gym_data$Resting_BPM)
summary(gym_data$Session_Duration_hrs)
summary(gym_data$Calories_Burned)
summary(gym_data$Fat_Percentage)
summary(gym_data$Water_Intake_ltrs)
summary(gym_data$BMI)

# Visualizations
hist(gym_data$Age, breaks = 20, main = "Histogram of Age", xlab = "Age", col = "lightblue")
hist(gym_data$Weight_in_kg, breaks = 20, main = "Histogram of Weight", xlab = "Weight (kg)", col = "lightblue")
hist(gym_data$Height_in_m, breaks = 20, main = "Histogram of Height", xlab = "Height (m)", col = "lightblue")
hist(gym_data$Max_BPM, breaks = 20, main = "Histogram of Max BPM", xlab = "Max BPM", col = "lightblue")
hist(gym_data$Resting_BPM, breaks = 20, main = "Histogram of Resting BPM", xlab = "Resting BPM", col = "lightblue")
hist(gym_data$Session_Duration_hrs, breaks = 20, main = "Histogram of Session Duration (hrs)", xlab = "Session Duration (hrs)", col = "lightblue")
hist(gym_data$Calories_Burned, breaks = 20, main = "Histogram of Calories Burned", xlab = "Calories Burned", col = "lightblue")
hist(gym_data$Fat_Percentage, breaks = 20, main = "Histogram of Fat Percentage", xlab = "Fat Percentage", col = "lightblue")
hist(gym_data$Water_Intake_ltrs, breaks = 20, main = "Histogram of Water Intake (liters)", xlab = "Water Intake (liters)", col = "lightblue")
hist(gym_data$BMI, breaks = 20, main = "Histogram of BMI", xlab = "BMI", col = "lightblue")

# Boxplots for workout types
boxplot(BMI ~ Workout_Type, data = gym_data, main = "Boxplot of BMI by Workout Type", xlab = "Workout Type", ylab = "BMI", col = "lightblue")
boxplot(Calories_Burned ~ Workout_Type, data = gym_data, main = "Boxplot of Calories Burned by Workout Type", xlab = "Workout Type", ylab = "Calories Burned", col = "lightblue")

# Data Overview
cat("Number of observations:", nrow(gym_data), "\n")
cat("Number of variables:", ncol(gym_data), "\n")

# Summary Statistics for BMI
summary(gym_data$BMI)

# Additional calculations for BMI
mean_BMI <- mean(gym_data$BMI, na.rm = TRUE)
median_BMI <- median(gym_data$BMI, na.rm = TRUE)
sd_BMI <- sd(gym_data$BMI, na.rm = TRUE)
min_BMI <- min(gym_data$BMI, na.rm = TRUE)
max_BMI <- max(gym_data$BMI, na.rm = TRUE)

cat("Mean of BMI:", mean_BMI, "\n")
cat("Median of BMI:", median_BMI, "\n")
cat("Standard deviation of BMI:", sd_BMI, "\n")
cat("Minimum of BMI:", min_BMI, "\n")
cat("Maximum of BMI:", max_BMI, "\n")

# Distribution Visualization for BMI
# Histogram
hist(gym_data$BMI, main = "Histogram of BMI", xlab = "BMI", col = "lightblue", border = "black")

# Boxplot
boxplot(gym_data$BMI, main = "Boxplot of BMI", ylab = "BMI", col = "lightgreen")

# Categorical Variable Analysis: Workout Type
library(ggplot2)
# Bar plot for Calories Burned by Workout Type
ggplot(gym_data, aes(x = factor(Workout_Type), y = Calories_Burned)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Calories Burned by Workout Type",
       x = "Workout Type",
       y = "Mean Calories Burned") +
  theme_minimal()

# Correlation Analysis between BMI and Calories Burned
cor_BMI_Calories <- cor(gym_data$BMI, gym_data$Calories_Burned, use = "complete.obs")
cat("Pearson correlation coefficient between BMI and Calories Burned:", cor_BMI_Calories, "\n")

# ScatterPlot Visualization of BMI vs Calories Burned
plot(gym_data$BMI, gym_data$Calories_Burned, main = "Scatter Plot of BMI vs. Calories Burned", xlab = "BMI", ylab = "Calories Burned", pch = 19, col = "blue")
abline(lm(Calories_Burned ~ BMI, data = gym_data), col = "red")

# Multiple Regression to predict Calories Burned using BMI, Age, and Weight
model <- lm(Calories_Burned ~ BMI + Age + Weight_in_kg, data = gym_data)

# Model summary
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


par(mfrow = c(2, 2))
plot(model)

# Check for heteroscedasticity using Breusch-Pagan test
library(lmtest)
bptest(model)

# Check for normality of residuals using Shapiro-Wilk test
shapiro.test(residuals(model))

# hapiro-Wilk normality test
# 
# data:  residuals(model)
# W = 0.99389, p-value = 0.0005255

# Principal Component Analysis (PCA)
# Perform PCA
any(is.na(gym_data))

# Replace NA with the mean of the column (can apply to specific columns as needed)
for (i in 1:ncol(gym_data)) {
  if (any(is.na(gym_data[, i]))) {
    gym_data[, i][is.na(gym_data[, i])] <- mean(gym_data[, i], na.rm = TRUE)
  }
}
gym_data_numeric <- gym_data[, sapply(gym_data, is.numeric)]
pca_result <- prcomp(gym_data_numeric, scale. = TRUE)

# Scree plot
par(mfrow = c(1, 1))
plot(pca_result, type = "l", main = "Scree Plot of PCA")

# PCA interpretation using Biplot
biplot(pca_result, main = "PCA Biplot", col = c("white", "red"),cex=0.6)