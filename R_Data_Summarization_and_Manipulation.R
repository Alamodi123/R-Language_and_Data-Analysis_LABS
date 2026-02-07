# 4.1 Install Packages

install.packages("psych")
library(psych)
describe(ChickWeight)
colnames(mtcars)
pairs.panels(mtcars[, c("mpg", "disp", "hp", "wt")])


# Create a frequency table with graph by install epiDisplay package.
install.packages("epiDisplay")
library(epiDisplay)
tab1(mtcars$gear, sort.group = "decreasing", cum.percent = TRUE)


# 4.2.1 filter() Method

library(dplyr)
stats <- data.frame(player=c('A', 'B', 'C', 'D'), runs=c(100, 200, 408, 
                                                         19), wickets=c(17, 20, NA, 5))
filter(stats, runs>100)


# 4.2.2 distinct() Method

library(dplyr)
stats <- data.frame(player=c('A', 'B', 'C', 'D', 'A', 'A'), runs=c(100,
                                                                   200, 408, 19, 56, 100), wickets=c(17, 20, NA, 5, 2, 17))
distinct(stats)
distinct(stats, player, .keep_all = TRUE)


# 4.2.3 arrange() Method

arrange(stats, runs)

# 4.2.4 select() Method

library(dplyr)
stats <- data.frame(player=c('A', 'B', 'C', 'D'), runs=c(100, 200, 408,
                                                         19), wickets=c(17, 20, NA, 5))
select(stats, player,wickets)

# 4.2.5 rename() Method
rename(stats, runs_scored=runs)


# 4.2.6 mutate() and transmute() Methods

mutate(stats, avg=runs/4)
transmute(stats, avg=runs/4)


# 4.2.7 summarize() Method
summarize(stats, sum(runs), mean(runs))

data()





