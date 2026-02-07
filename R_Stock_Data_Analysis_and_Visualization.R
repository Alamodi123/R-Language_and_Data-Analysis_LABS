# Set the working directory for the subsequent import and export operations
setwd("C:/Users/LENOVO/Desktop/R/Lab (Topic 03) - Data Manipulation and Visualization")

# Read a CSV files using read.csv() function
maybank_daily_prices <- read.csv("1155.KL.csv")
maybank_daily_prices <- read.csv("1155.KL - Copy.csv")

# Print the summary of the imported data
summary(maybank_daily_prices)

which(is.na(maybank_daily_prices))

# Delete a row with NULL values, dated 30 May 2023
maybank_daily_prices_new <- maybank_daily_prices[!maybank_daily_prices$Date=='2023-05-30',]

# Transform all the columns (except Date) to numeric columns
maybank_daily_prices_new$Open <- as.numeric (maybank_daily_prices_new$Open)
maybank_daily_prices_new$High <- as.numeric (maybank_daily_prices_new$High)
maybank_daily_prices_new$Low <- as.numeric (maybank_daily_prices_new$Low)
maybank_daily_prices_new$Close <- as.numeric (maybank_daily_prices_new$Close)
maybank_daily_prices_new$Adj.Close <- as.numeric (maybank_daily_prices_new$Adj.Close)
maybank_daily_prices_new$Volume <- as.numeric (maybank_daily_prices_new$Volume)

# Print the summary of the data frame after the deletion and transformation
summary(maybank_daily_prices_new)

# Insert a column named R_High_Low for the range of highest and lowest prices
maybank_daily_prices_new$R_High_Low <- maybank_daily_prices_new$High - maybank_daily_prices_new$Low

# Find the number of days where the closing price is more than RM9.00
x <- maybank_daily_prices_new[maybank_daily_prices_new$Close > 9.0,]
print(x)
length(x$Date)

# Extract the share prices in the month of Aug 2023
aug_2023 <- maybank_daily_prices_new [months(as.Date (maybank_daily_prices_new$Date, "%Y-%m-%d")) == "August",]

# Compute the average closing price of Aug 2023
mean(aug_2023$Close)

# Export the share prices of Maybank in Aug 2023 as a CSV file
write.csv(aug_2023, "maybank_aug_2023.csv", row.names=FALSE)

# Create a boxplot for Maybank closing prices in Aug 2023
boxplot(aug_2023$Close,
        main="Boxplot for Maybank Closing Prices in Aug 2023",
        ylab="Closing Prices (in RM)"
)

# Create a histogram for Maybank closing prices
# from 14 Nov 2022 to 14 Nov 2023
hist(maybank_daily_prices_new$Close,
     xlab="Closing Prices (in RM)",
     col="yellow",
     border="blue"
)

# Create a scatter plot for Maybank closing prices 
# from 14 Nov 2022 to 14 Nov 2023
plot(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d"),
     maybank_daily_prices_new$Close,
     main="Maybank closing prices from 14 Nov 2022 to 14 Nov 2023",
     xlab="Date", 
     ylab="Closing Prices (in RM)"
)

#V. Exercise
# Q1. Sort the maybank_daily_prices data frame based on volume (in ascending and descending order).

maybank_daily_prices_ascending <- maybank_daily_prices_new[order(maybank_daily_prices_new$Volume), ]
maybank_daily_prices_descending <- maybank_daily_prices_new[order(-maybank_daily_prices_new$Volume), ]

head(maybank_daily_prices_ascending)
head(maybank_daily_prices_descending)

# Q2. Find the average volume in July and August 2023 separately. Also, find the average volume for both July and August 2023.

# Extract data for July 2023
july_2023 <- maybank_daily_prices_new[months(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d")) == "July" & 
                                        format(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d"), "%Y") == "2023", ]

avg_volume_july <- mean(july_2023$Volume, na.rm = TRUE)
print(paste("Average Volume in July 2023:", avg_volume_july))

# Extract data for August 2023
august_2023 <- maybank_daily_prices_new[months(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d")) == "August" & 
                                          format(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d"), "%Y") == "2023", ]


avg_volume_august <- mean(august_2023$Volume, na.rm = TRUE)
print(paste("Average Volume in August 2023:", avg_volume_august))


# Combine July and August data
combined_july_august <- rbind(july_2023, august_2023)

# Calculate the average volume for the combined data
avg_volume_combined <- mean(combined_july_august$Volume, na.rm = TRUE)
print(paste("Average Volume for July and August 2023 Combined:", avg_volume_combined))

# Q3. Find the date(s) with the maximum and minimum volume.

# Find the index of the row with the maximum volume
max_volume_index <- which.max(maybank_daily_prices_new$Volume)

# Find the date with the maximum volume
max_volume_date <- maybank_daily_prices_new$Date[max_volume_index]
print(paste("Date with Maximum Volume:", max_volume_date))

# Find the index of the row with the minimum volume
min_volume_index <- which.min(maybank_daily_prices_new$Volume)

# Find the date with the minimum volume
min_volume_date <- maybank_daily_prices_new$Date[min_volume_index]
print(paste("Date with Minimum Volume:", min_volume_date))

# Q4. Identify the dates with the maximum and minimum ranges, based on the column of R_High_Low.
# Find the index of the row with the maximum range in R_High_Low
max_range_index <- which.max(maybank_daily_prices_new$R_High_Low)

# Find the date with the maximum range
max_range_date <- maybank_daily_prices_new$Date[max_range_index]
print(paste("Date with Maximum Range:", max_range_date))

# Find the index of the row with the minimum range in R_High_Low
min_range_index <- which.min(maybank_daily_prices_new$R_High_Low)

# Find the date with the minimum range
min_range_date <- maybank_daily_prices_new$Date[min_range_index]
print(paste("Date with Minimum Range:", min_range_date))


# Q5. Create two boxplots for Maybank closing price, one for May 2023, and another one for Jun 2023. Place them side by side as one figure.
# Extract May 2023 data
may_2023 <- maybank_daily_prices_new[months(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d")) == "May" & 
                                       format(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d"), "%Y") == "2023", ]

# Extract June 2023 data
jun_2023 <- maybank_daily_prices_new[months(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d")) == "June" & 
                                       format(as.Date(maybank_daily_prices_new$Date, "%Y-%m-%d"), "%Y") == "2023", ]

# Create two boxplots side by side
par(mfrow=c(1,2))  # Set layout to side by side
boxplot(may_2023$Close, main="Boxplot for May 2023 Closing Prices", ylab="Closing Prices (in RM)")
boxplot(jun_2023$Close, main="Boxplot for June 2023 Closing Prices", ylab="Closing Prices (in RM)")

















