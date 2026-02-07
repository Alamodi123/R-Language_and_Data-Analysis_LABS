# i. Dataset Selection
library(dplyr)
library(readr)

obesity <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")
dim(obesity)

# ----------------------------------------------------------------------------

# ii. Define the objectives
# (No code for this section; objectives are stated in the report.)

# ----------------------------------------------------------------------------

# iii. Data Understanding

# View the structure of the dataset
str(obesity)

# Get a summary of all variables
summary(obesity)

# View the first few rows
head(obesity)

# ----------------------------------------------------------------------------

# iv. Exploratory Data Analysis (EDA), Data Manipulation, and Data Cleaning

# Check for missing values in each column
colSums(is.na(obesity))

# Summarize key numerical variables
summary(select(obesity, Age, Height, Weight, FCVC, NCP, CH2O, FAF, TUE))

# Frequency counts for key categorical variables
table(obesity$Gender)
table(obesity$family_history_with_overweight)
table(obesity$FAVC)
table(obesity$NObeyesdad)

# Rename NObeyesdad to Obesity_Level for clarity
names(obesity)[names(obesity) == "NObeyesdad"] <- "Obesity_Level"

# Encode Gender and family_history_with_overweight as factors
obesity$Gender <- as.factor(obesity$Gender)
obesity$family_history_with_overweight <- as.factor(obesity$family_history_with_overweight)

# Feature transformation: Normalize all continuous numerical variables (z-score)
num_cols <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
obesity[paste0(num_cols, "_z")] <- scale(obesity[num_cols])

# dplyr example: Calculate mean weight by gender
obesity %>%
  group_by(Gender) %>%
  summarise(mean_weight = mean(Weight))

# Quick histogram of Age (EDA visualization)
hist(obesity$Age, main = "Histogram of Age", xlab = "Age", col = "skyblue")

# ----------------------------------------------------------------------------

# v. Statistical Analysis

# Descriptive statistics: mean, median, sd for main numerical features
mean(obesity$Age)
median(obesity$Age)
sd(obesity$Age)
mean(obesity$Weight)
median(obesity$Weight)
sd(obesity$Weight)

# Frequency counts for categorical features
table(obesity$Gender)
table(obesity$Obesity_Level)

# Example: t-test - Compare mean weight by gender
t.test(Weight ~ Gender, data = obesity)

# Example: ANOVA - Compare mean Age by Obesity_Level
aov_age <- aov(Age ~ Obesity_Level, data = obesity)
summary(aov_age)

# ----------------------------------------------------------------------------


# vi. Data Visualisation

library(ggplot2)
library(corrplot)

# 1. Bar plot: Distribution of obesity levels
ggplot(obesity, aes(x = Obesity_Level, fill = Obesity_Level)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Obesity Levels", x = "Obesity Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Histogram: Age distribution
ggplot(obesity, aes(x = Age)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age", y = "Count")

# 3. Boxplot: Weight by Obesity Level
ggplot(obesity, aes(x = Obesity_Level, y = Weight, fill = Obesity_Level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Weight by Obesity Level", x = "Obesity Level", y = "Weight") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Scatter plot: Height vs. Weight, colored by Obesity Level
ggplot(obesity, aes(x = Height, y = Weight, color = Obesity_Level)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Height vs. Weight by Obesity Level", x = "Height (m)", y = "Weight (kg)")

# 5. Correlation analysis for numerical features
num_cols <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
num_data <- obesity[, num_cols]
cor_matrix <- cor(num_data)

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)

# ----------------------------------------------------------------------------











