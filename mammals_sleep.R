
mammals_sleep <- read.csv("C:\\Users\\mohsi\\Downloads\\mammals.csv", header = TRUE)
#Overview of the dataset

View(head(mammals_sleep))

#species  body_wt brain_wt non_dreaming dreaming total_sleep life_span
#1        Africanelephant 6654.000   5712.0           NA       NA         3.3      38.6
#2 Africangiantpouchedrat    1.000      6.6          6.3      2.0         8.3       4.5
#3              ArcticFox    3.385     44.5           NA       NA        12.5      14.0
#4   Arcticgroundsquirrel    0.920      5.7           NA       NA        16.5        NA
#5          Asianelephant 2547.000   4603.0          2.1      1.8         3.9      69.0
#6                 Baboon   10.550    179.5          9.1      0.7         9.8      27.0
#gestation predation exposure danger
#1       645         3        5      3
#2        42         3        1      3
#3        60         1        1      1
#4        25         5        2      3
#5       624         3        5      4
#6       180         4        4      4

#predation
#   An index of how likely the mammal is to be preyed upon. 
#   1 = least likely to be preyed upon. 5 = most likely to be preyed upon.

#exposure
#   An index of the how exposed the mammal is during sleep. 
#   1 = least exposed (e.g., sleeps in a well-protected den). 5 = most exposed.

#danger
#   An index of how much danger the mammal faces from other animals. This index is based upon Predation and Exposure.
#   1 = least danger from other animals. 5 = most danger from other animals.

summary(mammals_sleep$body_wt)
summary(mammals_sleep$brain_wt)
summary(mammals_sleep$total_sleep)
summary(mammals_sleep$life_span)
summary(mammals_sleep$non_dreaming)
summary(mammals_sleep$danger)

#summary(mammals_sleep$body_wt)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.005    0.600    3.342  198.790   48.202 6654.000 
#summary(mammals_sleep$body_wt)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.005    0.600    3.342  198.790   48.202 6654.000 
#summary(mammals_sleep$brain_wt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.14    4.25   17.25  283.13  166.00 5712.00 
# summary(mammals_sleep$total_sleep)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2.60    8.05   10.45   10.53   13.20   19.90       4 
#summary(mammals_sleep$life_span)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2.000   6.625  15.100  19.878  27.750 100.000       4 
#summary(mammals_sleep$non_dreaming)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   2.100   6.250   8.350   8.673  11.000  17.900      14 
#summary(mammals_sleep$danger)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.000   1.000   2.000   2.613   4.000   5.000 

hist(mammals_sleep$body_wt,breaks=100)
hist(mammals_sleep$brain_wt,breaks=100)
hist(mammals_sleep$total_sleep,breaks=20)
hist(mammals_sleep$life_span,breaks=20) # exponential decay
hist(mammals_sleep$non_dreaming,breaks =20) #closertonormal
hist(mammals_sleep$danger)

boxplot(life_span ~ predation, data = mammals_sleep,
        main = "Boxplot of Lifespan by Predation Level",
        xlab = "Predation Level",
        ylab = "Lifespan",
        col = "lightblue")
boxplot(life_span ~ exposure, data = mammals_sleep,
        main = "Boxplot of Lifespan by Predation Level",
        xlab = "Predation Level",
        ylab = "Lifespan",
        col = "lightblue")
boxplot(life_span ~ danger, data = mammals_sleep,
        main = "Boxplot of Lifespan by Predation Level",
        xlab = "Predation Level",
        ylab = "Lifespan",
        col = "lightblue")
boxplot(total_sleep~ exposure, data = mammals_sleep,
        main = "Boxplot of Lifespan by Predation Level",
        xlab = "Predation Level",
        ylab = "Total_sleeptime",
        col = "lightblue")

#1 Data Overview

cat("Number of observations:", nrow(mammals_sleep), "\n")
cat("Number of variables:", ncol(mammals_sleep), "\n")

#2 Summary Statistics for Life Span


summary(mammals_sleep$life_span)

# Additional calculations
mean_life_span <- mean(mammals_sleep$life_span, na.rm = TRUE)
median_life_span <- median(mammals_sleep$life_span, na.rm = TRUE)
sd_life_span <- sd(mammals_sleep$life_span, na.rm = TRUE)
min_life_span <- min(mammals_sleep$life_span, na.rm = TRUE)
max_life_span <- max(mammals_sleep$life_span, na.rm = TRUE)

cat("Mean of life span:", mean_life_span, "\n")
cat("Median of life span:", median_life_span, "\n")
cat("Standard deviation of life span:", sd_life_span, "\n")
cat("Minimum of life span:", min_life_span, "\n")
cat("Maximum of life span:", max_life_span, "\n")

#3. Distribution Visualization for Life Span

# Histogram
hist(mammals_sleep$life_span, main = "Histogram of Life Span", xlab = "Life Span (years)", col = "lightblue", border = "black")

# Boxplot
boxplot(mammals_sleep$life_span, main = "Boxplot of Life Span", ylab = "Life Span (years)", col = "lightgreen")

#4. Categorical Variable Analysis

library(ggplot2)
# Bar plot for Level of Danger
ggplot(mammals_sleep, aes(x = factor(danger), y = life_span)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Mean Lifespan by Level of Danger",
       x = "Level of Danger",
       y = "Mean Lifespan (Years)") +
  theme_minimal()

#5. Correlation Analysis between lifespan and total_sleep of Species
cor_life_span_body_wt <- cor(mammals_sleep$life_span, mammals_sleep$total_sleep, use = "complete.obs")
cat("Pearson correlation coefficient between life span and total_sleep:", cor_life_span_body_wt, "\n")

#6. ScatterPlot Visualization of lifeespan and bodyweight of Species
# Scatter plot
plot(mammals_sleep$total_sleep, mammals_sleep$life_span, main = "Scatter Plot of Total Sleep vs. Life Span", xlab = "Total Sleep", ylab = "Life Span (years)", pch = 19, col = "blue")
abline(lm(life_span~total_sleep, data = mammals_sleep), col = "red")

#7. Multiple Regression
# Fit linear model
model <- lm(life_span ~ body_wt + gestation, data = mammals_sleep)

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


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -22.622  -7.372  -2.424   5.891  68.684 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.575040   2.999333   2.526   0.0146 *  
#   body_wt     -0.003231   0.002750  -1.175   0.2453    
# gestation    0.089669   0.017644   5.082 5.17e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 14.58 on 52 degrees of freedom
# (7 observations deleted due to missingness)
# Multiple R-squared:  0.3941,	Adjusted R-squared:  0.3708 
# F-statistic: 16.91 on 2 and 52 DF,  p-value: 2.198e-06

# The coefficient for body_wt is -0.003231, which implies that 
# for each unit increase in body weight, the dependent variable is 
# expected to decrease by 0.003231 units, holding all other variables
# constant. However, the p-value (0.2453) is greater than 0.05, 
# meaning that this variable is not statistically significant at the
# 5% level, and it does not have a strong effect on the dependent variable.
# 
# 
# The coefficient for gestation is 0.089669, which suggests that 
# for each additional day of gestation, the dependent variable is 
# expected to increase by 0.089669 units, holding all other variables 
# constant. The p-value (5.17e-06) is extremely small and well below 
# the 0.01 threshold, indicating that gestation is a highly significant
# predictor of the dependent variable.

#8 Model Diagnostics
# Residual plots
par(mfrow = c(2, 2))
plot(model)

library(lmtest)
bptest(model)
# Results of BP-Test
#   BP = 2.4536, df = 2, p-value = 0.2932

# Since the p-value (0.2932) is greater than the common significance level 
# (e.g., 0.05 or 0.01), you fail to reject the null hypothesis. The null hypothesis
# of the Breusch-Pagan test states that the residuals have constant variance 
# (i.e., they are homoscedastic).

#Conclusion :
#This indicates that there is no significant evidence of heteroscedasticity in your
#model. The residuals appear to have constant variance across the range of fitted 
#values, satisfying the assumption of homoscedasticity.

# Shapiro-Wilk test for normality
shapiro.test(residuals(model))

# W = 0.80971, p-value = 5.82e-07

# The p-value (5.82e-07) is much smaller than the common significance levels (e.g., 0.05 or 0.01). 
# Therefore, you reject the null hypothesis of the Shapiro-Wilk test, which states that the 
# data are normally distributed.

#Conclusion
# This indicates that the residuals do not follow a normal distribution. The data show significant
# deviation from normality, suggesting that the assumption of normality of residuals is violated. 
# This may require further investigation or data transformation to address the non-normality.

#9. Principal Component Analysis
# Perform PCA

any(is.na(mammals_sleep))

# Replace NA with the mean of the column (can apply to specific columns as needed)
for (i in 1:ncol(mammals_sleep)) {
  if (any(is.na(mammals_sleep[, i]))) {
    mammals_sleep[, i][is.na(mammals_sleep[, i])] <- mean(mammals_sleep[, i], na.rm = TRUE)
  }
}
mammals_sleep_numeric <- mammals_sleep[, sapply(mammals_sleep, is.numeric)]
pca_result <- prcomp(mammals_sleep_numeric, scale. = TRUE)

# Scree plot
plot(pca_result, type = "l", main = "Scree Plot of PCA")

# The Scree Plot Shows that After 5 PCs the Variability is very less and remains
# almost constant.

#10. PCA interpretation Using Bi PLot
par(mar = c(5, 4, 4, 2))
par(mfrow = c(1, 1))
biplot(pca_result, main = "PCA Biplot",col = c("white", "red"),cex=0.7)

# There seems to be a cluster of observations near the center,
# suggesting moderate similarity in their variable characteristics.
# Variables like "Body Wt" and "Reproduction" appear to contribute 
# significantly (long arrows).Some variables (e.g., "Dreaming," "Gestation")
# have similar directions,indicating a positive correlation.
# Variables pointing in opposing directions (e.g., "Reproduction" vs. "Body Wt")
# might suggest trade-offs or negative correlations.




