# VECTOR ============
# assign a vector in a form of numerical data
age<-c(40,23,24)
age

# assign a vector in a form of character data
color<-c("blue", "red", "green")
color

exercise<-c(TRUE, TRUE, FALSE, NA) # logical vector
exercise

# elements with different data type cannot be mixed in a vector
cats<-c(1, "one", TRUE)
cats

# if numeric vector mix with logical vector, TRUE will be assigned as 1 and FALSE as 0
a<-c(80, TRUE)
a

# sequence from 1 to 10
sequence<-1:10
sequence


# sequence from 1 to 10 with increment of 2
sequence2<-seq(1, 10, 2)
sequence2

#page 2


# generate word female 5 times
female<-rep("Female", 5)
female

# 2.3.1 Extracting the Elements from Vector
data<-c(12,10,15,18,9,21)
data[3] # select the third element of the vector
data[1:3] # select first three element
data[c(1,4,6)] # select elements at position 1,4 and 6

# 2.3.2 Removing the Elements from Vector
data[-2] # remove the second element
data[-(4:6)] # remove the last three element
data[-c(1,3,6)] # remove elements at position 1,3 and 6
# determine the number of elements in a vector
length(data)

# 2.3.3 Operation in a Vector
data>10
data<10
data==10
data!=10
data>10|data<18
data>10&data<18
data[data>15]
data[data!=10]
sum(data<15)

#page 3
 
which(data>15)
data[which(data>15)]
min(data)
which(data==min(data))
which.max(data)
sort(data) # sort data ascending order

# consider two vectors:
x<-c(2,5,7,11,14)
y<-c(1,4,5,7,6)
x+2
3*x-1
x+y

# DATA FRAME ============
Height<-c(156, 160, 154, 148, 170, 178, 188, 163, 165, 157)
Weight<-c(48, 56, 45, 50, 68, 75, 81, 65, 70, 49)

# combine 2 vectors as a data frame
Mydata<-data.frame(Height,Weight)
Mydata

# rename the observations
Namesresp<-c("Susan", "Mary" ,"Lily" ,"Hillary ","Mike", "John", "William", "Cathy", "Jeff", "Julie")
Variables<-c("Height", "Weight")
dimnames(Mydata)<-list(Namesresp, Variables)
Mydata

# add more variables

Gender<-c("Female", "Female", "Female", "Female", "Male", "Male", "Male", "Female", "Male",
          "Female")
Age<-c(30, 36, 26, 47, 31, 49, 28, 50, 42, 47)
Mydata<-data.frame(Mydata, Gender) # by using data.frame()
Mydata
Mydata<-cbind(Mydata, Age) # by using cbind()
Mydata

# add more observations
a<-c(177,60,"Female",33)
Mydata<-rbind(Mydata,a)
Mydata


# 2.4.1 Extract elements from Data Frame
Mydata[,1] # select the first column
Mydata$Height # select Height column
Mydata[,2:3] # select column 2 until 3
Mydata[,c(1,3)] # select column 1 and 3
Mydata[4,] # select observation in row 4 only
Mydata[3:6,] # select observations in row 3 to 6
Mydata[c(2,5,7),] # select observations from specific rows, row 2, 5 and 7


# 2.4.2 Remove Elements from Data frame
Mydata[,-1] # remove first column
Mydata[,-(3:4)] # remove column 3 and 4
Mydata[,-c(2,4)] # remove column 2 and 4
Mydata[-3,] # remove row 3
Mydata[-(1:6),] # remove row 1 to 6
Mydata[-c(3,7,10),] # remove row 3, 7 and 10

# arrange data according to names in alphabetical order
Mydata<-Mydata[with(Mydata, order(Namesresp)), ]

# change column name
colnames(Mydata)[which(names(Mydata) == "Gender")] = "Sex"

# to save and reload data frame in r
save(Mydata,file="Mydata.rda")
load("Mydata.rda")


# MATRICES ============
# convert a vector to matrices
x<-c(20, 30, 27, 25, 28, 21, 23, 24, 29, 21, 33, 19, 22, 24, 22, 20)

m<-matrix(x,nrow=4,ncol = 4) # by row
m

m<-matrix(x,nrow=4,ncol = 4,byrow = T) # by column
m


# assign the name of column and row
group<-c("A","B","C","D") # column name
obs<-c("sample 1", "sample 2", "sample 3", "sample 4") # row name
dimnames(m)<-list(obs,group)
m

m[2,] # select second row
m[2:3,] # select row 2 and 3
m[,-3] # remove third column
m[,c(2,4)] # select column 2 and 4


# add column and row
sample_5 <- c(3,7,14,18)
rbind(m,sample_5) # add vector to a row matrix
E <- c(13,17,20,21)
cbind(m,E) # add vector to a column matrix

# create a matrix with the value of same elements, for example all elements are value 1
z <- matrix(1,2,3)
z

# LIST ============
a<-c(T,F,F,T,T) # vector
b<-matrix(c(1,3,5,8),ncol=2) # matrix

c<-data.frame(id=101:103,age=c(40,38,33)) #data frame

abc<-list(a,b,c) # combine as a list
abc

abc[1] # select the first element from the list
abc[c(1,3)] # select the first and third element from the list


# ARRAY ============


# creates an array of two 3x3 matrices each with 3 rows and 3 columns
a <- c(5,9,3)
b <- c(10,11,12,13,14,15)
ab <- array(c(a,b),dim = c(3,3,2))
ab

# give names to the rows, columns and matrices in the array
column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")
ab <- array(c(a,b),dim = c(3,3,2),dimnames = list(row.names,column.names, matrix.names))
ab
ab[3,,2] # select elements from the third row of Matrix2
ab[1,3,1] # select elements from the first row third column of Matrix1


# SLIDE 26
gender <- factor(c("female", "female", "male", "male"));
gender[-3]

# seq() function for creating creating a sequence of continuous values.
# length.out defines the length of vector.
Y <- seq(1, 10, length.out = 5)
cat('using seq() function', Y, '\n')





