# A. Load Dataset

# Load the dataset
data("airquality")
df <- airquality

# check the first 5 rows of your data
head(df)
# Understand the structure of your data. You can check the type of variable, total samples, column name.
str(df)

# Summarize your data statistically
summary(df)

#B. Data Exploration

# Check for missing values
colSums(is.na(df))


# C. Data Cleaning

## Method 1: Remove NA rows
df_clean <- df
df_clean <- na.omit(df)

# Check dimensions before and after
dim(df)
dim(df_clean)

#Method 2: Mean Imputation
df_mean <- df
df_mean$Ozone[is.na(df_mean$Ozone)] <- mean(df_mean$Ozone, na.rm
                                            = TRUE)
df_mean$Solar.R[is.na(df_mean$Solar.R)] <- mean(df_mean$Solar.R,
                                                na.rm = TRUE)
# Check dimensions before and after
dim(df_mean)
summary(df_mean)

#Method 3: Median Imputation
df_median <- df
df_median$Ozone[is.na(df_median$Ozone)] <-
  median(df_median$Ozone, na.rm = TRUE)
df_median$Solar.R[is.na(df_median$Solar.R)] <-
  median(df_median$Solar.R, na.rm = TRUE)
summary(df_median)


#Method 4: Interpolation (for time-based data)
library(zoo)
library(dplyr)
# Interpolate missing values linearly
df_interp <- df %>%
  mutate(Ozone = na.approx(Ozone, na.rm = FALSE),
         Solar.R = na.approx(Solar.R, na.rm = FALSE))
summary(df_interp)

#Method 5: Custom Rule; Replace with 0
df_zero <- df
df_zero$Ozone[is.na(df_zero$Ozone)] <- 0

# D. Data Manipulation with dplyr

library(dplyr)

# Create a new column: temperature in Celsius.
df_clean <- df_clean %>%
  mutate(TempC = round((Temp - 32) * 5/9, 1))

# Filter: Data for June only
june_data <- df_clean %>% filter(Month == 6)

# Group by month and summarize average Ozone
monthly_summary <- df_clean %>%
  group_by(Month) %>%
  summarise(Avg_Ozone = mean(Ozone), .groups = 'drop')
print(monthly_summary)


# E. Outlier Detection, Normalization, and Logarithmic Transformation

# Boxplot for Outlier Detection
# Boxplots for each numeric variable
boxplot(df_clean$Ozone, main="Boxplot of Ozone",
        col="lightblue")
boxplot(df_clean$Wind, main="Boxplot of Wind", col="lightgreen")
boxplot(df_clean$TempC, main="Boxplot of Temperature",
        col="lightpink")

# Normalization (Min-Max Scaling)
# Min-Max normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Apply to numeric columns
df_norm <- df_clean %>%
  mutate(across(c(Ozone, Solar.R, Wind, TempC), normalize))

head(df_norm)

# Logarithmic Transformation
# Step 1: Check Data Distribution
# Histogram of Ozone and Solar Radiation
par(mfrow = c(1,2))
hist(df_clean$Ozone, main = "Ozone Distribution", col =
       "lightblue", xlab = "Ozone")
hist(df_clean$Solar.R, main = "Solar Radiation Distribution",
     col = "lightgreen", xlab = "Solar Radiation")
par(mfrow = c(1,1))
















