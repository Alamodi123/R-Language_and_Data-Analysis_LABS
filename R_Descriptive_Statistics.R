# Descriptive Statistic =================

# Set the working directory for the subsequent import and export operations
setwd("C:/Data")

# Read the CSV files using read.csv() function
sales <- read.csv("C:/Users/Haziqah Shamsudin/Desktop/Teaching/S241_CPC351_CPM351/Lectures/03 STATISTICAL CONCEPTS/Data/yearly_sales.csv")

# Convert gender variable to factor in the data frame sales  
sales$gender <- factor(sales$gender)

# Create an additional spender variable to categorize sales_totals into three groups-small, medium,
#  and big-according to the amount of the sales with the following code

# build an empty character vector of the same length as sales
sales_group <- vector(mode="character", length=length(sales$sales_total))

# group the customers according to the sales amount
sales_group[sales$sales_total<100] <- "small"
sales_group[sales$sales_total>=100 & sales$sales_total<500] <- "medium"
sales_group[sales$sales_total>=500] <- "big"

# create and add the ordered factor to the sales data frame
spender <- factor(sales_group,levels=c("small", "medium", "big"),
                  ordered = TRUE)
sales <- cbind(sales, spender)

# Print the summary of the imported data (to view the descriptive statistic)
summary(sales)

x <- sales$sales_total
y <- sales$num_of_orders

cov(x, y)   # returns 345.2111 (covariance)
cor(x, y)   # returns 0.7508015 (correlation)
IQR(x)      # returns 215.21 (interquartile range, difference between the third and the first quartiles)
mean(x)     # returns 249.4557 (mean)
median(x)   # returns 151.65 (median)
min(x)      # returns 30.02 (minimum)
max(x)      # returns 7606.09 (maximum)
range(x)    # returns 30.02 7606.09 (min max)
sd(x)       # returns 319.0508 (std. dev.)
var(x)      # returns 101793.4 (variance)
