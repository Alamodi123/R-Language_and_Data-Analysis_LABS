# Load necessary packages
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

#Import the datasets
sales <- read.csv("sales.csv")
products <- read.csv("products.csv")
inventory <- read.csv("inventory.csv")
stores <- read.csv("stores.csv")

# View structure of each dataset
cat("=== sales ===\n")
str(sales)

cat("\n=== products ===\n")
str(products)

cat("\n=== inventory ===\n")
str(inventory)

cat("\n=== stores ===\n")
str(stores)

# Clean and convert date columns
sales$Date <- as.Date(sales$Date)
stores$Store_Open_Date <- as.Date(stores$Store_Open_Date)

# Remove $ and spaces from price and cost columns, convert to numeric
products <- products %>%
  mutate(
    Product_Cost = as.numeric(str_replace_all(Product_Cost, "[$ ]", "")),
    Product_Price = as.numeric(str_replace_all(Product_Price, "[$ ]", ""))
  )

# i

# Filter for transactions that occurred in 2018
sales_2018 <- sales %>% filter(year(Date) == 2018)

# Count the number of sales transactions in 2018
num_transactions_2018 <- nrow(sales_2018)
print(paste("Number of sales transactions in 2018:", num_transactions_2018))

#------------------------------------------------------------------------------

# ii

# Match product prices into sales
sales$Product_Price <- products$Product_Price[match(sales$Product_ID, products$Product_ID)]

# Calculate revenue
product_revenue <- sales %>%
  mutate(Revenue = Units * Product_Price) %>%
  group_by(Product_ID) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(TotalRevenue))

# Get the top product
top_product_id <- product_revenue$Product_ID[1]
top_product_name <- products$Product_Name[products$Product_ID == top_product_id]

# Get the store IDs that have this product in inventory
top_product_stores <- inventory %>%
  filter(Product_ID == top_product_id) %>%
  pull(Store_ID) %>%
  unique()

# Print results
print(paste("Top Product ID:", top_product_id))
print(paste("Product Name:", top_product_name))
print("Store IDs stocking the top product:")
print(top_product_stores)

# -----------------------------------------------------------------------

# iii

# Step 1: How many cities and locations?
num_cities <- n_distinct(stores$Store_City)
num_locations <- n_distinct(stores$Store_Location)

print(paste("Number of cities:", num_cities))
print(paste("Number of distribution locations:", num_locations))

# Step 2: Add Store_City to inventory using match()
inventory$Store_City <- stores$Store_City[match(inventory$Store_ID, stores$Store_ID)]

# Step 3: Group by Store_City and Product_ID, then sum
library(dplyr)

product_distribution <- inventory %>%
  group_by(Store_City, Product_ID) %>%
  summarise(Total_Distributed = sum(Stock_On_Hand, na.rm = TRUE)) %>%
  arrange(Store_City, desc(Total_Distributed))

# Show sample results
print(head(product_distribution, 10))

# ----------------------------------------------------------------

# iv

# Group by Store_ID and calculate average Stock_On_Hand
store_avg_stock <- inventory %>%
  group_by(Store_ID) %>%
  summarise(Average_Stock = mean(Stock_On_Hand, na.rm = TRUE)) %>%
  arrange(Average_Stock)

# Get the store with the lowest average stock
lowest_avg_store <- store_avg_stock[1, ]

# Show result
print("Store with the lowest average quantity available:")
print(lowest_avg_store)

# ---------------------------------------------------------------------------

# v

# Step 1: Add Product_Price and Product_Cost to sales using match
sales$Product_Price <- products$Product_Price[match(sales$Product_ID, products$Product_ID)]
sales$Product_Cost <- products$Product_Cost[match(sales$Product_ID, products$Product_ID)]

# Step 2: Calculate Revenue and Profit
sales <- sales %>%
  mutate(
    Revenue = Units * Product_Price,
    Profit = Units * (Product_Price - Product_Cost),
    Month = format(as.Date(Date), "%Y-%m")  # Extract year-month
  )

# Step 3: Total sales/profit by month
monthly_summary <- sales %>%
  group_by(Month) %>%
  summarise(
    Total_Sales = sum(Revenue, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  ) %>%
  arrange(Month)

print("Monthly sales and profit summary:")
print(head(monthly_summary, 10))

# Step 4: Total sales/profit by store
store_summary <- sales %>%
  group_by(Store_ID) %>%
  summarise(
    Total_Sales = sum(Revenue, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  ) %>%
  arrange(Total_Sales)

# Add city and location using match()
store_summary$Store_City <- stores$Store_City[match(store_summary$Store_ID, stores$Store_ID)]
store_summary$Store_Location <- stores$Store_Location[match(store_summary$Store_ID, stores$Store_ID)]

# Step 5: Find highest/lowest by store
lowest_store <- store_summary[1, ]
highest_store <- store_summary[nrow(store_summary), ]

print("Lowest sales store:")
print(lowest_store)
print("Highest sales store:")
print(highest_store)

# Step 6: Total sales/profit by city
city_summary <- store_summary %>%
  group_by(Store_City) %>%
  summarise(
    City_Sales = sum(Total_Sales, na.rm = TRUE),
    City_Profit = sum(Total_Profit, na.rm = TRUE)
  ) %>%
  arrange(City_Sales)

lowest_city <- city_summary[1, ]
highest_city <- city_summary[nrow(city_summary), ]

print("City with lowest sales:")
print(lowest_city)
print("City with highest sales:")
print(highest_city)

# ------------------------------------------------------------------------------

# vi

library(dplyr)
library(tidyr)

# Step 1: Sort products by Product_Cost in descending order
products_sorted <- products %>%
  arrange(desc(Product_Cost))

products %>% filter(grepl("-", Product_Name))

# Step 2: Separate Product_Name by "-"
products_separated <- products_sorted %>%
  separate(Product_Name, into = c("Product", "Brand"), sep = "-", fill = "right", extra = "merge")

products_separated %>% filter(!is.na(Brand))

# Step 3: Get the Brand of the 3rd most expensive product
third_most_expensive_brand <- products_separated$Brand[3]

print(paste("Brand of the third most expensive product:", third_most_expensive_brand))

head(products_separated, 5)

# ----------------------------------------------------------------------------

# vii

library(dplyr)
library(lubridate)

# Step 1: Add Product_Price and Product_Cost using match
sales$Product_Price <- products$Product_Price[match(sales$Product_ID, products$Product_ID)]

# Step 2: Add Month column
sales$Month <- floor_date(sales$Date, "month") #or sales$Month <- format(sales$Date, "%Y-%m")

# Step 3: Get monthly average price and total quantity per product
monthly_summary <- sales %>%
  group_by(Product_ID, Month) %>%
  summarise(
    Avg_Price = mean(Product_Price, na.rm = TRUE),
    Total_Units = sum(Units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Product_ID, Month)

# Step 4: Calculate percentage changes and elasticity
monthly_pct_change <- monthly_summary %>%
  group_by(Product_ID) %>%
  arrange(Month) %>%
  mutate(
    Lag_Price = lag(Avg_Price),
    Lag_Units = lag(Total_Units),
    Pct_Change_Price = 100 * (Avg_Price - Lag_Price) / Lag_Price,
    Pct_Change_Qty = 100 * (Total_Units - Lag_Units) / Lag_Units,
    Elasticity = Pct_Change_Qty / Pct_Change_Price
  ) %>%
  filter(!is.na(Elasticity), is.finite(Elasticity), Lag_Price != 0, Lag_Units != 0) %>%
  ungroup()

# Step 5: Compute average elasticity per product
elasticity_summary <- monthly_pct_change %>%
  group_by(Product_ID) %>%
  summarise(
    Avg_Elasticity = mean(Elasticity, na.rm = TRUE)
  ) %>%
  arrange(desc(abs(Avg_Elasticity)))  # Sort by sensitivity

# Step 6: Show results
print("Most price-sensitive products:")
print(head(elasticity_summary, 5))

print("Least price-sensitive products:")
print(tail(elasticity_summary, 5))

table(table(monthly_summary$Product_ID))

monthly_summary %>%
  group_by(Product_ID) %>%
  summarise(n_months = n(), n_prices = n_distinct(Avg_Price)) %>%
  arrange(desc(n_prices))

monthly_summary %>%
  group_by(Product_ID) %>%
  summarise(n_months = n(), n_units = n_distinct(Total_Units)) %>%
  arrange(desc(n_units))
