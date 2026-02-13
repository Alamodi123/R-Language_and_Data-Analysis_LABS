# 📊 R Labs & Applied Data Analysis

> **Author:** Abdulrahman Alamodi  
> **Institution:** Albukhary International University (AIU)  
> **Focus:** Statistical Programming, Data Wrangling, & Machine Learning  

Welcome to my **R Programming & Data Analysis** repository. This collection contains practical lab exercises and applied data analysis scripts developed as part of my coursework in Statistical Programming and Data Science. 

Each R script is designed to be highly modular, heavily commented, and focused on demonstrating practical implementations of core R concepts—from foundational data structures to advanced predictive simulations.

---

## 🛠️ Technology Stack

![R](https://img.shields.io/badge/R_Programming-276DC3?style=for-the-badge&logo=r&logoColor=white) 
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
![Data Analysis](https://img.shields.io/badge/Data_Analysis-FF6F00?style=for-the-badge&logo=google-analytics&logoColor=white)

**Key Libraries & Packages:**
`dplyr` 🧹 | `ggplot2` 📊 | `caret` 🤖 | `rpart` 🌲 | `e1071` 🧠 | `zoo` 📈 | `psych` 🧠 | `epiDisplay` 🏥 | `mc2d` 🎲 | `plotrix` 📉

---

## 📂 Detailed Script Breakdown

Here is a comprehensive overview of the scripts included in this repository, organized by their core analytical focus:

| 🗂️ Analytical Focus | 📄 Script Name | 🎯 Key Concepts & Description | 📦 Core Libraries |
| :--- | :--- | :--- | :--- |
| **🧱 Foundational R** | `R_Data_Structures_and_Basic_Operations.R` | **Data Types:** Vectors, matrices, data frames, lists, arrays, and advanced indexing. | `base` |
| **🧱 Foundational R** | `R_Control_Structures_and_Data_Visualization.R` | **Logic & Viz:** Loops, conditionals, functions, and basic plots (bar, pie, hist, boxplot). | `base`, `plotrix` |
| **📈 Statistics** | `R_Descriptive_Statistics.R` | **Metrics:** Mean, median, variance, covariance, and correlation matrices. | `stats` |
| **📈 Statistics** | `R_Normal_Distribution_and_Probability.R` | **Probability:** Distribution analysis and properties of the normal distribution. | `stats` |
| **🧹 Data Wrangling** | `R_Data_Cleaning_and_Preprocessing.R` | **Preprocessing:** Missing values, normalization, and outlier detection (`airquality` data). | `dplyr` |
| **🧹 Data Wrangling** | `R_Data_Summarization_and_Manipulation.R` | **Wrangling:** Advanced data manipulation and descriptive summarizations. | `dplyr`, `psych`, `epiDisplay` |
| **💼 Real-World Analytics**| `R_Stock_Data_Analysis_and_Visualization.R` | **Finance:** Maybank stock analysis, trend visualizations, and interpreting market behavior. | `ggplot2`, `zoo` |
| **💼 Real-World Analytics**| `R_Retail_Data_Analysis_and_Business_Insights.R` | **Business:** Retail dataset insights, sales velocity, and inventory trends. | `dplyr`, `ggplot2` |
| **💼 Real-World Analytics**| `R_Obesity_Data_Analysis_and_Statistical_Measures.R` | **Health:** End-to-end Exploratory Data Analysis (EDA) and robust statistical testing. | `dplyr`, `psych` |
| **🤖 Machine Learning** | `R_Simulation_and_Machine_Learning.R` | **AI & Models:** Distribution simulation, Linear Regression, Naive Bayes, KNN, and Decision Trees. | `caret`, `rpart`, `e1071`, `mc2d` |

---

## 📘 Analytical Workflow Overview

This repository demonstrates a comprehensive mastery of the end-to-end data lifecycle in R, covering:
* **Data Engineering:** Managing structures, handling missing values, and preprocessing.
* **Exploratory Data Analysis (EDA):** Descriptive statistics, transformations, and hypothesis testing.
* **Visual Analytics:** Generating insights through highly customized charts and graphs.
* **Statistical Modeling:** Probability distributions, covariance, and linear regression simulations.
* **Machine Learning:** Implementing classical classification algorithms on structured data.

---

## ⚙️ Quick Start

To run these scripts locally on your machine, clone the repository and open the files in **RStudio**:

```bash
# 1. Clone the repository
git clone [https://github.com/Alamodi123/R-Labs-Data-Analysis.git](https://github.com/Alamodi123/R-Labs-Data-Analysis.git)
cd R-Labs-Data-Analysis

```

*💡 Tip: Ensure you run install.packages(c("dplyr", "ggplot2", "caret", "e1071", "psych", "epiDisplay", "rpart", "zoo", "mc2d", "plotrix")) in your R console before executing the advanced scripts to ensure all dependencies are met.*

```r
install.packages(c("dplyr", "ggplot2", "caret", "e1071", "psych", "epiDisplay", "rpart", "zoo", "mc2d", "plotrix"))
