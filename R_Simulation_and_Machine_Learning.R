set.seed(12)
expense<- rnorm(n=10000, mean = 2000, sd=500)
hist(expense, col='orange',xlab='Expense ($)', main='Expense')
mtext(paste('mean:', round(mean(expense))), at=c(500), line = -1)
mtext(paste('stdev:', round(sd(expense))), at=c(500), line = -2)
mtext(paste('median:',round(median(expense))), at=c(500), line = -3)


unexpected<- rnorm(n=10000, mean = 20, sd=300)
hist(expense, col='coral2',xlab='unexpected Income/Expense ($)',
     main='Unexpected Income or Expense')
mtext(paste('mean:', round(mean(unexpected))), at=c(700), line = -1)
mtext(paste('stdev:', round(sd(unexpected))), at=c(700), line = -2)
mtext(paste('median:',round(median(unexpected))), at=c(700), line = -
        3)


shape_rate <- function(mean,sd){
  scale <- sd^2/mean
  shape <- mean/scale
  rate <- 1/scale
  return(c(scale,shape,rate))
}

set.seed(12)
income<-rgamma(n=10000, shape=shape_rate(mean=3200,
                                         sd=2000)[2],rate=shape_rate(mean=3200,sd=2000)[3])
hist(income, col='cornflowerblue',xlab='Income ($)', main='Income')
mtext(paste('mean:', round(mean(income))), at=c(10000), line = -1)
mtext(paste('stdev:', round(sd(income))), at=c(10000), line = -2)
mtext(paste('median:',round(median(income))), at=c(10000), line = -3)



install.packages("mc2d")
library("mc2d")
survey <- c(30, 60, 90, 110, 120, 150, 300, 320, 350, 400)
credit <- rempiricalD(n=10000, survey)
hist(credit, col='darkolivegreen',xlab='credit ($)',
     main='credit',breaks=10)
mtext(paste('mean:', round(mean(credit))), at=c(200), line = -1)
mtext(paste('stdev:', round(sd(credit))), at=c(200), line = -2)
mtext(paste('median:',round(median(credit))), at=c(200), line = -3)



#interest
interest <- runif(10000)*(1.5-0.3)+0.3
hist(interest, col='violet',xlab='Interest Rate (%)', main='Interest
Rate')
mtext(paste('mean:', round(mean(interest),2)), at=c(0.6), line = -1)
mtext(paste('stdev:', round(sd(credit),2)), at=c(0.6), line = -2)
mtext(paste('median:',round(median(credit),2)), at=c(0.6), line = -3)



#saving
saving <- (income-expense-credit+unexpected) * (1+(interest/100))
hist(saving, xlab='saving ($)', main='saving', prob=TRUE, xlim=c(-
                                                                   3000,10000), breaks=24)
mtext(paste('mean:', round(mean(saving))), at=c(6000), line = -1)
mtext(paste('stdev:', round(sd(saving))), at=c(6000), line = -2)
mtext(paste('median:',round(median(saving))), at=c(6000), line = -3)


# ---------------------------------------------------------------------------------


# Simulation for Linear Regression
set.seed(123) # Ensures the same random numbers each time
library(ggplot2)
simulate_linear <- function(condition = "linear", n = 100) {
  x <- runif(n, 0, 10) # Simulate study hours from 0 to 10
  if (condition == "linear") {
    y <- 2 * x + 5 + rnorm(n, 0, 2) # Perfect linear + small noise
  } else if (condition == "nonlinear") {
    y <- 2 * x^2 + rnorm(n, 0, 10) # Quadratic relationship
  } else if (condition == "noise") {
    y <- 2 * x + 5 + rnorm(n, 0, 10) # Linear with high noise
  } else if (condition == "outliers") {
    y <- 2 * x + 5 + rnorm(n, 0, 2)
    y[c(5,10)] <- y[c(5,10)] + 50 # Add large outliers
  }
  data <- data.frame(x = x, y = y)
  return(data)
}


# Linear Case
data1 <- simulate_linear("linear")
model1 <- lm(y ~ x, data = data1)
summary(model1)
cor(data1$x, data1$y)


# Non-linear Case
data2 <- simulate_linear("nonlinear")
model2 <- lm(y ~ x, data = data2)
summary(model2)
cor(data2$x, data2$y)


# Noisy Case
data3 <- simulate_linear("noise")
model3 <- lm(y ~ x, data = data3)
summary(model3)
cor(data3$x, data3$y)


# With Outliers
data4 <- simulate_linear("outliers")
model4 <- lm(y ~ x, data = data4)
summary(model4)
cor(data4$x, data4$y)


par(mfrow = c(2,2), mar = c(4, 4, 2, 1)) # smaller margins
plot(data1$x, data1$y, main = "Linear", pch=19)
abline(model1, col="red")
mtext(paste("r =", round(cor(data1$x, data1$y), 2)), side=3, line = -
        1)
plot(data2$x, data2$y, main = "Non-Linear", pch=19)
abline(model2, col="red")
mtext(paste("r =", round(cor(data2$x, data2$y), 2)), side=3, line = -
        1)
plot(data3$x, data3$y, main = "High Noise", pch=19)
abline(model3, col="red")
mtext(paste("r =", round(cor(data3$x, data3$y), 2)), side=3, line = -
        1)
plot(data4$x, data4$y, main = "With Outliers", pch=19)
abline(model4, col="red")
mtext(paste("r =", round(cor(data4$x, data4$y), 2)), side=3, line = -
        1)


# ---------------------------------------------------------------------------------


# VI. Exploring ML Algorithms through Simulation on the Iris dataset

data(iris)
str(iris)
# Optional: encode Species as factor if needed
iris$Species <- as.factor(iris$Species)
# Visualize basic structure
pairs(iris[1:4], col=iris$Species)

library(e1071) # for naiveBayes
library(class) # for knn
library(rpart) # for decision tree
library(caret) # for confusionMatrix
set.seed(123)
n_sim <- 50
acc_nb <- c()
acc_knn <- c()
acc_tree <- c()

for (i in 1:n_sim) {
  idx <- sample(1:nrow(iris), 0.7 * nrow(iris))
  train <- iris[idx, ]
  test <- iris[-idx, ]
  
  # Naive Bayes
  model_nb <- naiveBayes(Species ~ ., data = train)
  pred_nb <- predict(model_nb, test)
  acc_nb[i] <- mean(pred_nb == test$Species)
  # KNN (k=3)
  pred_knn <- knn(train[,-5], test[,-5], cl = train$Species, k = 3)
  acc_knn[i] <- mean(pred_knn == test$Species)
  
  # Decision Tree
  model_tree <- rpart(Species ~ ., data = train)
  pred_tree <- predict(model_tree, test, type = "class")
  acc_tree[i] <- mean(pred_tree == test$Species)
}


library(ggplot2)
results <- data.frame(
  Accuracy = c(acc_nb, acc_knn, acc_tree),
  Method = rep(c("Naive Bayes", "KNN", "Decision Tree"), each =
                 n_sim)
)

ggplot(results, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_boxplot() +
  labs(title = "Simulated Accuracy of ML Models on Iris Dataset",
       y = "Accuracy", x = "Algorithm") +
  theme_minimal()
  
# Add noise to Iris dataset
iris_noisy <- iris
iris_noisy[,1:4] <- iris_noisy[,1:4] + matrix(rnorm(nrow(iris)*4, 0,
                                                    0.5), ncol=4)

# Run one of the models again and compare accuracy

set.seed(123)
idx <- sample(1:nrow(iris_noisy), 0.7 * nrow(iris_noisy))
train <- iris_noisy[idx, ]
test <- iris_noisy[-idx, ]

model_nb_noisy <- naiveBayes(Species ~ ., data = train)
pred_nb_noisy <- predict(model_nb_noisy, test)
mean(pred_nb_noisy == test$Species)

# -----------------------------------------------------------------

# ------------------------
# V. DISCUSSION
# ------------------------

# 1. What is simulation in the context of machine learning?

# Simulation means generating or repeating data scenarios (like random train/test splits)
# to evaluate how a machine learning model performs under different conditions.
# It's useful when real-world testing is expensive or not feasible.


# 2. Why do we simulate multiple runs instead of doing it once?

# Running a model once may give a biased or unrepresentative result due to randomness.
# By repeating the training/testing process (e.g., 50 times), we get a more reliable average accuracy,
# and we can see how much the model performance varies across different splits.


# 3. How did each model perform under different settings?

# - Naive Bayes showed high and consistent accuracy with low variability across runs.
# - KNN had good performance, but accuracy varied slightly more between splits.
# - Decision Tree had more fluctuation and was slightly less stable than the others.
# This can be seen clearly in the boxplot visualization.


# 4. Which model would you choose if you had noisy data?

# To test robustness, we added noise to the numerical features of the Iris dataset:
# iris_noisy[,1:4] <- iris_noisy[,1:4] + matrix(rnorm(nrow(iris)*4, 0, 0.5), ncol=4)
# Then we re-ran Naive Bayes and observed the accuracy.
# Example result: accuracy dropped slightly from ~96% to ~93%.
# This means Naive Bayes still performed well despite the noise, making it a robust choice.
# In contrast, other models (like KNN or Decision Tree) might suffer more from added noise.
# Therefore, Naive Bayes would be the preferred model in a noisy data environment.











                                                                                                                                                                                                                                                                                                                                                                                                                                                    