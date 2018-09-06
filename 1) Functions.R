#1) Objects
a <- 1
b <- 1
c <- -1
print(a)
ls()           #Names of all objects saved in the workspace
rm(list=ls())  #Remove all from the Global Environment

#Functions
#Usually functions are in (), if no () it shows us the code of the functions
ls
log(a)

#Nested Functions
log(exp(1))

#To get help on a function
?log
help("log")

#For a quick reminder of arguments
args(log)

log(x = 8, base = 2)
log(8,2)   #These two are equivalent since R assumes you are inputting arguments in order

?"+" #Shows all arithmetic operators

data()  #shows all pre-built data in R

pi
Inf

n <- 1000
x <- seq(1,n)
sum(x)

#2) Data Types

#Class defines type of object
class(2)
class(ls)

library(dslabs)
data("murders")
class(murders)

#str shows structure of an object
str(murders)
head(murders)  #shows first six

#  $ called accessor is used to access different variabes
murders$population

names(murders)  #shows the names of the variables

length(murders$population)    #tells you how many objects in the vector population

#if you want to work with strings, use quotes

class(murders$state)

#Logical vector either TRUE or FALSE   == is a relational operator
z <- 3==2
z


#factors
class(murders$region)
#best for categorical data
murders$region
levels(murders$region)  #shows the levels/categories

#factors are used to save the categories as integers to save space
# [[]] is th same as $ murders$state = murders[["state"]]

identical (a,b)  #checks if two vectors are identical

table()   #Takes in a vector and outputs the frequency of each unique element

#3) Vectors

#Vectors creatd by using c - concatenate

codes <- c(380,124,818)
country <- c("italy","canada","egypt")
codes <- c(italy=380,canada=124,egypt=818)
names(codes) <- country   #this is equivalent to line 87

#sequence
seq(1,9, by=2)
1:10  #shorthand for seq(1,10)
seq(0,100,length.out = 5)  #increases by same amount but specified length

#using L after whoe number defines integes
3L-3

#Subsetting

#we use []  to access elements of a vector
codes[2]
codes[c(1,3)]
codes["canada"]

#coercion
#R tries to guess the type of vector
x <- c(1,"canada",3)   #R converted the whole vector to characters

as.character()           #converts number to characters
as.numeric()            #converts characters to numbers

#Missing Data defined as NA
#When R fails to coerce it creates NA

#4) Sorting

library(dslabs)
data(murders)
sort(murders$total)

index <- order(murders$total)  #returns the indices that sort the vector parameter
murders$abb[index]    #returns in order the state abbreviation in order of total murders

max(murders$total)   #max number of murders
i_max <- which.max(murders$total)   #index of the max number of murders
murders$state[i_max]
#similar with min

#rank gives the rank of each entry
rank(murders$total)

#data frame
# Store temperatures in an object 
temp <- c(35, 88, 42, 84, 81, 30)

# Store city names in an object 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Create data frame with city names and temperature 
city_temps <- data.frame(name = city, temperature = temp)

is.na()  #returns logical vector of where NA is 

#Vector Arithmetic

murders$state[which.max(murders$population)]
max(murders$population)

murder_rate <- murders$total/murders$population*100000

murders$state[order(murder_rate, decreasing = TRUE)]

#5) Indexing

index <- murder_rate < 0.71
murders$state[index]

sum(index)

#Logical operators:  
# == exactly equal to   != not equal to  ! not  | or   & and

west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]

#Indexing Functions

# which gives entries of a logical vector that are true
x <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
which(x)

index <- which(murders$state == "Massachusetts")
murder_rate[index]

# match looks for entries in a vector and returns the index needed to access them
index <- match(c("New York","Florida","Texas"),murders$state)
murder_rate[index]

# %in% if we want to know if each element of a first vector is in a second vector
x <- c("a","b","c","d","e")
y <- c("a","d","f")
y %in% x

c("Boston","Dakota","Texas") %in% murders$state

#6) Basic Data Wrangling
library(dplyr)    #for data manipulation on data frame
data(murders)

#mutate takes data frame as first argument and name and value as second argument
murders <- mutate(murders,rate=total/population*100000)   #we have redefined murders object

#filters to filter data takes data frame as first argument and condition as second argument
filter(murders, rate <= 0.71)

#select to select just the columns you want to work with
new_table <- select(murders, state, region, rate)

#dplyr helps you the argument, in our case murder, it assumes what is being piped is what
#should be operated on
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)
my_states <- murders %>% mutate(rate=total/population*100000,rank=rank(-rate)) %>% filter(region %in% c("Northeast",'West') & rate<1) %>% select(state,rate,rank)
#Creating data frames
#using data.frame function, automatically turns characters into factors
grades <- data.frame(names=c("Josh","Juan","Jean","Yao"),
                     exam_1=c(95,80,90,85),
                     exam_2=c(90,85,85,90),
                     stringsAsFactors = FALSE)

#     %in% to filter in dplyr

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks
#rank(-x)

#7) Basic Plots
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions,total_gun_murders)

hist(murders$rate)

boxplot(rate~region, data=murders)

#8) Programming Basics

#Conditional Expressions
#if
a <- 1/25
if(a!=0){
  print(1/a)
} else{
  print('No reciprocal for 0')
}

ind <- which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
} else{
  print('No state has murder rate that low')
}

#if-else    works on vectors
a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

#replacing NA with 0
data(na_example)
sum(is.na(na_example))

no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

#any and all
z <- c(TRUE, TRUE, FALSE)
any(z)   #checks if any entries in vector TRUE
all(z)   #checks if all entries in vector are TRUE

#9) Basic Functions
#average of x sum(x)/length(x)
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}      #s,n are not saved in workspace, so creating a s,n in workspace will not interfere
x <- 1:100
avg(x)
identical(mean(x),avg(x))

avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

#For Loops
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)

m <- 25
#create an empty vector
s_n <- vector("numeric", 25)
s_n <- vector (length=m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

n <- 1:m
plot(n,s_n)

plot(n, s_n)
lines(n, n*(n+1)/2)  #creates a line through discrete points

#for (i in range of values) {operations that use i that changes across the range of values}
for(i in 1:5){
  print(i)
}
i

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

#Other Functions
#Instead of loops we usually use apply, sapply, tapply, mapply
#Other: split, cut, quantile, reduce, identical, unique

#nchar  how long a character length is
nchar("massachusetts")
