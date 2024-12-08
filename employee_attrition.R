# Loading the data set
employee_attrition <- read.csv("C:\\Users\\mohsi\\OneDrive\\Desktop\\Employee_attrition_dataset.csv",header= TRUE)
# Overview of Data set
head(employee_attrition)
View(head(employee_attrition[, 1:7], 5))

# Age Attrition    BusinessTravel DailyRate             Department
# 1  41       Yes     Travel_Rarely      1102                  Sales
# 2  49        No Travel_Frequently       279 Research & Development
# 3  37       Yes     Travel_Rarely      1373 Research & Development
# 4  33        No Travel_Frequently      1392 Research & Development
# 5  27        No     Travel_Rarely       591 Research & Development
# 6  32        No Travel_Frequently      1005 Research & Development
# DistanceFromHome Education EducationField EmployeeCount EmployeeNumber
# 1                1         2  Life Sciences             1              1
# 2                8         1  Life Sciences             1              2
# 3                2         2          Other             1              4
# 4                3         4  Life Sciences             1              5
# 5                2         1        Medical             1              7
# 6                2         2  Life Sciences             1              8
# EnvironmentSatisfaction Gender HourlyRate JobInvolvement JobLevel
# 1                       2 Female         94              3        2
# 2                       3   Male         61              2        2
# 3                       4   Male         92              2        1
# 4                       4 Female         56              3        1
# 5                       1   Male         40              3        1
# 6                       4   Male         79              3        1
# JobRole JobSatisfaction MaritalStatus MonthlyIncome MonthlyRate
# 1       Sales Executive               4        Single          5993       19479
# 2    Research Scientist               2       Married          5130       24907
# 3 Laboratory Technician               3        Single          2090        2396
# 4    Research Scientist               3       Married          2909       23159
# 5 Laboratory Technician               2       Married          3468       16632
# 6 Laboratory Technician               4        Single          3068       11864
# NumCompaniesWorked Over18 OverTime PercentSalaryHike PerformanceRating
# 1                  8      Y      Yes                11                 3
# 2                  1      Y       No                23                 4
# 3                  6      Y      Yes                15                 3
# 4                  1      Y      Yes                11                 3
# 5                  9      Y       No                12                 3
# 6                  0      Y       No                13                 3
# RelationshipSatisfaction StandardHours StockOptionLevel TotalWorkingYears
# 1                        1            80                0                 8
# 2                        4            80                1                10
# 3                        2            80                0                 7
# 4                        3            80                0                 8
# 5                        4            80                1                 6
# 6                        3            80                0                 8
# TrainingTimesLastYear WorkLifeBalance YearsAtCompany YearsInCurrentRole
# 1                     0               1              6                  4
# 2                     3               3             10                  7
# 3                     3               3              0                  0
# 4                     3               3              8                  7
# 5                     3               3              2                  2
# 6                     2               2              7                  7
# YearsSinceLastPromotion YearsWithCurrManager
# 1                       0                    5
# 2                       1                    7
# 3                       0                    0
# 4                       3                    0
# 5                       2                    2
# 6                       3                    6

colnames(employee_attrition)

# Summary statistics for key variables
summary(employee_attrition$Age)
summary(employee_attrition$DailyRate)
summary(employee_attrition$MonthlyIncome)
summary(employee_attrition$YearsAtCompany)

# Visualization of distribution
hist(employee_attrition$Age, breaks = 20, main = "Histogram of Age", col = "lightblue", xlab = "Age")
hist(employee_attrition$MonthlyIncome, breaks = 30, main = "Histogram of Monthly Income", col = "lightgreen", xlab = "Monthly Income")
boxplot(YearsAtCompany ~ Attrition, data = employee_attrition, main = "Boxplot of Years at Company by Attrition", col = "lightcoral")

#1. Data OVerview
cat("Number of observations:", nrow(employee_attrition), "\n")
cat("Number of variables:", ncol(employee_attrition), "\n")

#2 Summary Statistics for Age

mean_MonthlyIncome <- mean(employee_attrition$MonthlyIncome, na.rm = TRUE)
median_MonthlyIncome <- median(employee_attrition$MonthlyIncome, na.rm = TRUE)
sd_MonthlyIncome <- sd(employee_attrition$MonthlyIncome, na.rm = TRUE)
min_MonthlyIncome <- min(employee_attrition$MonthlyIncome, na.rm = TRUE)
max_MonthlyIncome <- max(employee_attrition$MonthlyIncome, na.rm = TRUE)

cat("Mean of MonthlyIncome:", mean_MonthlyIncome, "\n")
cat("Median of MonthlyIncome:", median_MonthlyIncome, "\n")
cat("Standard deviation of MonthlyIncome:", sd_MonthlyIncome, "\n")
cat("Minimum of MonthlyIncome:", min_MonthlyIncome, "\n")
cat("Maximum of MonthlyIncome:", max_MonthlyIncome, "\n")

# Step 3: Distribution visualization for 'Age'
hist(employee_attrition$Age, main = "Histogram of Age", xlab = "Age", col = "lightblue", border = "black")
boxplot(employee_attrition$Age, main = "Boxplot of Age", ylab = "Age", col = "lightgreen")

# Step 4: Categorical variable analysis
# Bar plot for 'Attrition'
ggplot(employee_attrition, aes(x = JobSatisfaction)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Attrition Distribution", x = "Job Satisfaction", y = "Count") +
  theme_minimal()

# Step 5: Correlation analysis
cor_age_monthly_income <- cor(employee_attrition$Age, employee_attrition$MonthlyIncome, use = "complete.obs")
cat("Pearson correlation coefficient between Age and Monthly Income:", cor_age_monthly_income, "\n")

# Step 6: Scatter plot visualization
plot(employee_attrition$Age, employee_attrition$MonthlyIncome, main = "Scatter Plot of Age vs. Monthly Income",
     xlab = "Age", ylab = "Monthly Income", pch = 19, col = "blue")
abline(lm(MonthlyIncome ~ Age, data = employee_attrition), col = "red")

# Step 7: Multiple regression
model <- lm(MonthlyIncome ~ YearsSinceLastPromotion + TotalWorkingYears, data = employee_attrition)
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
#   Min       1Q   Median       3Q      Max 
# -10907.1  -1740.6    -56.5   1402.5  11313.9 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              1211.32     137.36   8.819   <2e-16 ***
#   YearsSinceLastPromotion    56.03      26.43   2.120   0.0342 *  
#   TotalWorkingYears         458.26      10.95  41.865   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2985 on 1467 degrees of freedom
# Multiple R-squared:  0.5986,	Adjusted R-squared:  0.598 
# F-statistic:  1094 on 2 and 1467 DF,  p-value: < 2.2e-16

# Step 8: Model diagnostics
# Residual plots
par(mfrow = c(2, 2))
plot(model)

# Breusch-Pagan test for homoscedasticity
bptest(model)

# data:  model
# P = 287.89, df = 2, p-value < 2.2e-16

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals(model))

# data:  residuals(model)
# W = 0.98222, p-value = 1.69e-12

# Step 9: Principal component analysis (PCA)
# Replace NA with mean in numeric columns
# Impute missing values for numeric columns only
for (i in 1:ncol(employee_attrition)) {
  if (is.numeric(employee_attrition[, i]) && any(is.na(employee_attrition[, i]))) {
    employee_attrition[, i][is.na(employee_attrition[, i])] <- mean(employee_attrition[, i], na.rm = TRUE)
  }
}

# Select numeric columns
employee_attrition_numeric <- employee_attrition[, sapply(employee_attrition, is.numeric)]

# Remove constant or zero-variance columns
non_constant_cols <- apply(employee_attrition_numeric, 2, function(x) sd(x, na.rm = TRUE) > 0)
employee_attrition_numeric <- employee_attrition_numeric[, non_constant_cols]

# Perform PCA
pca_result <- prcomp(employee_attrition_numeric, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Scree plot
par(mfrow = c(1, 1))
plot(pca_result, type = "l", main = "Scree Plot of PCA")

# 5 PCs explain maximum variability.

#10. PCA interpretation Using Bi PLot

biplot(pca_result, main = "PCA Biplot", col = c("white", "red"),cex=0.6)
#The PCA biplot reveals the relationships between variables in the dataset, 
#with the first two principal components (PC1 and PC2) capturing the most variance. 
#Variables pointing in similar directions, such as PerformanceRating, YearsAtCompany,
#and YearsInCurrentRole, are positively correlated, suggesting that employees with
#higher performance ratings tend to have longer tenures. Variables like Age, 
#NumCompaniesWorked, and Education show distinct relationships, with NumCompaniesWorked
#negatively correlated with Age, implying younger employees may have worked at fewer
#companies. Some variables, such as JobSatisfaction and EnvironmentSatisfaction, are
#closely aligned, indicating a strong correlation. Overall, the biplot helps identify
#key relationships and patterns in the data, offering insights into employee 
#characteristics and potential factors affecting attrition or satisfaction.
