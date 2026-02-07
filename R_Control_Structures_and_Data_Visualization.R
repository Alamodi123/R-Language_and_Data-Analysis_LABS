for (i in 1:10) {
  print (i)
}

x <- c("a", "b", "c", "d")
for (i in 1:4) {
  print (x[i]) # print out each element of x
} 

n<-100
result<-1
for (i in 1:n) {
  result<-result * i
}
result 

x <- 3
if (x > 2) {
  y <- 2 * x
} else {
  y <- 3 * x
}

x <- runif(1, 0, 10)
if(x > 3) {
  y <- 10
} else {
  y<- 0
}
y

count <- 0
while (count < 10) {
  print(count)
  count <- count + 1
}

z <- 5
set.seed(1)
while (z >= 3 && z <= 10) {
  coin <- rbinom(1, 1, 0.05)        #rbinom(n, size, probability)
  if (coin == 1) { #random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}

x = 1
repeat { # Repeat loop
  print(x)
  if (x > 4) { # Break statement to terminate if x > 4
    break
  }
  x = x + 1 # Increment x by 1
}

for (i in 1:100) {
  print(i)
  if(i <= 20) {
    next # skip the first 20 iterations
  }
}

for (i in 1:100) {
  if(i <= 20) {
    next # skip the first 20 iterations
  }
  print(i)
}


for (i in 1:100) {
  print(i)
  if (i > 20) {
    break # stop loop after 20 iterations
  }
}

# Creating sequence of numbers from 32 to 46.
print(seq(32,46))

# Finding the mean of numbers from 22 to 80.
print(mean(22:80))

# Finding the sum of numbers from 41 to 70.
print(sum(41:70))

# Creating a function without an argument.
new.function <- function() {
  for(i in 1:5) {
    print(i^2)
  }
}
new.function()

# Creating a function to print squares of numbers in sequence.
new.function <- function() {
  for(i in 1:5) {
    b <- i^2
    print(b)
  }
}
# Calling the function new.function with no argument.
new.function()

# Creating a function to print squares of numbers in sequence.
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
# Calling the function new.function supplying 10 as an argument.
new.function(10)
new.function(25)

# Creating a function with argument
new.function <- function(x, y, z) {
  result <- x * y + z
  print(result)
}

# Calling the function by position of argument.
new.function(11,13,9)

# Calling the function by names of the argument.
new.function(x=2,y=5,z=3)

# Creating a function with arguments
new.function <- function(x=11, y=24) {
  result <- x * y
  print(result)
}

# Calling the function without giving any argument.
new.function()

# Calling the function with giving new values of the argument.
new.function(4,6)

#2D pie chart :
dat <- c(10, 12, 4, 16, 8)
category <- c("Chicken", "Meat", "Vegetable", "Fish", "Fruit")
percent <- round(dat/sum(dat)*100)
category <- paste(category, percent) # add percents to labels
category <- paste(category,"%",sep="") # ad % to labels
windowsFonts(A = windowsFont("Times New Roman")) #change font
pie(dat,labels = category, col=rainbow(length(category)), family="A",
main="Pie Chart of Favorite Food")

#3D pie chart
library(plotrix)
pie3D(dat,labels=category,explode=0.2, family="A", main="Pie Chart of Favorite Food")


#Simple Bar chart
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears",family="A",col = c("blue","navy blue","turquoise"))


#Horizontal simple bar chart
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))


counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, family="A", main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        names.arg=c("3 Gears", "4 Gears", "5 Gears"),
        legend = c(0,1))


#Cluster bar chart
barplot(counts, family="A", main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


#Simple Histogram
x <- mtcars$mpg
h<-hist(x, breaks=10, col="red", family="A", xlab="Miles Per Gallon",
        main="Histogram with Normal Curve")


#Histogram with density curve
dh<-hist(x, breaks=10, col="red", family="A", xlab="Miles Per Gallon",
         main="Histogram with Normal Curve",probability = T)
lines(density(x),col="blue",lwd=2)



#Simple dot plot
dotchart(mtcars$mpg,family="A", labels=row.names(mtcars),cex=.7,
         main="Gas Milage for Car Models",
         xlab="Miles Per Gallon")

#Boxplot
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data",
        xlab="Number of Cylinders", ylab="Miles Per Gallon")






