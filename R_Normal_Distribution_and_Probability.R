# Set the working directory for the subsequent import and export operations
setwd("C:/Users/LENOVO/Desktop/R")


# Read the CSV files using read.csv() function
height <- read.csv("height.csv")

# View the summary of the data frame
summary(height)

# Find the mean and the standard deviation of the height variable
mean(height$Height)
sd(height$Height)

# Create a histogram for the heights
hist(height$Height)

# Create a histogram for the heights with breaks = 8
hist(height$Height, breaks = 8)

# Calculate mean and standard deviation of the Height column
mean_height <- mean(height$Height)
sd_height <- sd(height$Height)

# a. What is the probability of a randomly picked student from the student dataset with a height less than or equal to 164cm?
p_less_equal_164 <- pnorm(164, mean = mean_height, sd = sd_height)

# b. What is the probability of a randomly picked student from the student data set with a height higher or equal to 188cm?
p_greater_equal_188 <- 1 - pnorm(188, mean = mean_height, sd = sd_height)

# c. What is the probability of a randomly picked student from the student data set with a height between 150cm and 160cm?
p_between_150_160 <- pnorm(160, mean = mean_height, sd = sd_height) - pnorm(150, mean = mean_height, sd = sd_height)