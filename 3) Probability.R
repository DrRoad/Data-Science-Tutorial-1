######################## Probability ########################

rm(list=ls())

## 1) Discrete Probability

# Related to categorical data

# Probability of an event: Proportion of times the event occurs when we repeat the experiment
# over and over, independently and under the same conditions.

# Pr(A)   A - event


## 2) Monte Carlo Simulations

# Random number generators permit us to mimic the process of picking at random

# Simulating urn with 2 red and 3 blue balls

beads <- rep(c("red","blue"), times=c(2,3))
beads

# Sample lets you repeat picks, but by default without replacement

sample(beads,1)   # produces one random outcome


# We repeat experiment a large enough number of times, to make the result practically equivalent to doing over and over
# This is a Monte Carlo Simulation

B <- 10000
events <- replicate(B, sample(beads,1))

tab <- table(events)
tab

prop.table(tab)


# Sample lets you repeat picks, but by default without replacement
# So we don't neet to use the replicate function here

events <- sample(beads, B, replace=TRUE)
prop.table(table(events))


## 3) Probability Distributions

# To define a distribution for categorical outcomes, we simply assign a probability to each category.


## 4) Independence

# Two events are independent if the outcome of one does not affect the other.

# example) Coin tosses, picking beads from urn with replacement.

# If proability of getting king is 4/52, and you have a king, probability of king again now is 3/51
# These events are not independent since first outcome affects the second.
# Probabilities change once you see the other outcomes

x <- sample(beads, 5)
x[2:5]

# Conditional probabilities for non-independent events.

# Pr(Card 2 is a king | Card 1 is a king) = 3/51

# if A, B are independent Pr(A|B) = Pr(A)

# Multiplication Rule: Pr(A and B) = Pr(A)*Pr(B|A)

# Prbability of Blackjack : First card Ace, next cards face cards: 1/13*16/51 = 0.02

# Pr(A and B and C) = Pr(A)*Pr(B\A)*Pr(C|A and B)


## 5) Combinations and Permutations

paste(letters[1:5], as.character(1:5))    # create strings using smaller strings

expand.grid(pants=c("blue","black"), shirt=c("white","grey","plaid"))  # performs operation element wise



library(gtools)

# Permutations computes, for any list of size "n', all the different ways we can select "r" items, order matters
permutations(5,2)

# To see 5 random 7-digit phone numbers out of all possible numbers

all_phone_numbers <- permutations(10, 7, v=0:9)   
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

# Combinations computes, for any list of size "n', all the different ways we can select "r" items, order does not matters
combinations(5,2)

# Constructing a deck of cards using R

suits <- c("Diamonds","Clubs","Hearts","Spades")
numbers <- c("Ace","Deuce",'Three',"Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number,deck$suit)


# Probability that you draw a king is 1/13

kings <- paste("King",suits)   # Vector containing 4 ways we can get a king
mean(deck %in% kings)       # proportion of the deck is one of these cards


# Second card being king, given that first card is a king

hands <- permutations(52,2, v=deck)   # selecting 52 choose 2 out of deck

first_card <- hands[,1]     # first column
second_card <- hands[,2]    # second column

sum(first_card %in% kings & second_card %in% kings)/sum(first_card %in% kings)
mean(first_card %in% kings & second_card %in% kings)/mean(first_card %in% kings)


# Probability of a natural 21 in blackjack

aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2,v=deck)  # selecting 52 choose 2 out of deck
mean(hands[,1]  %in% aces & hands[,2] %in% facecard)


# We can use Monte Carlo simulation 

B <- 10000
hand <- sample(deck,2)

results <- replicate(B, {
  hand <- sample(deck,2)
  hand[1] %in% aces & hand[2] %in% facecard |
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)


## 6) The Birthday Problem

# Probability of 50 random people in the same room at least 2 sharing birthday

n <- 50
bdays <- sample(1:365, n, replace=TRUE)
any(duplicated(bdays))  # checks if any duplicates exist

B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)


## 7) Sapply

compute_prob <- function(n, B=1000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
})
  mean(same_day)
}

n <- seq(1,60)
compute_prob(n)     # We can not apply the sequence to the function we created since it expects a scalar


prob <- sapply(n, compute_prob)   # applies to each element of prob n

plot(n,prob)

# Exact calculation instead of using Monte Carlo Simulation

# Pr(Person 1 has unique birthday) = 1
# Pr(Person 2 has unique birthday | Person 1 has unique birthday) = 364/365
# Pr(Person 3 has unique birthday | Person 1 and Person 2 have unique Birthday) = 363/365
# 1x(364/365)x(363/365)x...x((365-n+1)/365)

n <- seq(1,60)

exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1-prod(prob_unique)
}

eprob <- sapply(n, exact_prob)

plot(n,prob)
lines(n, eprob, col="red")


## 8) How many Monte Carlo simulations are enough?

# Complicated question that requires advanced probability theory

# We can check the stability of the estimate

B <- 10^seq(1,5, len=100)

compute_prob <- function(B,n=22){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)
plot(log10(B), prob,type="l")



simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))


B <- 10000


set.seed(1)


celtic_wins <- replicate(B, {(simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))); 
  any( simulated_games=="win")})

mean(celtic_wins)


p <- seq(0.5, 0.95, 0.025)
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
Pr <- sapply(p,prob_win)

plot(p,Pr)



N <- seq(1,25,2)
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
Pr <- sapply(N, prob_win)
plot(N,Pr)


# 6 games remaining, first was a loss, need to win 4 or more games, prob of win = 0.5

n <- 6
l <- list(c(0,1))

possibilities <- expand.grid(rep(sample(l),n))
results <- rowSums(possibilities) >=4
mean(results==1)

# As Monte Carlo

B <- 10000
set.seed(1)

results <- replicate(B,{simulated_series <-sample(c(0,1), 6, replace = TRUE)
sum(simulated_series)>=4
})
mean(results)


## 9) The addition rule

# Pr(A or B) = Pr(A) + Pr(B) - Pr(A and B)

# Pr(Ace followed by Face Card) = (4/52)*(16/51)
# Pr(Face Card followed by Ace) = (16/52)*(4/51)
# Intersection is empty, probability of natural blackjack sum of top 2


## 10) The Monty Hall Problem

# Sticking to same door

B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

# Switching door

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors%in%c(my_pick, show)]
  switch == prize_door
})
mean(switch)


## 11) Continuous Probability

# Define a function that operates on intervals, CDF

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

F <- function(a) mean(x<=a)   # ECDF  gives proportion of values smaller than a
1-F(70.5)     # proportion of students taller than 70 inches


## 12) Theoretical Distribution

# F(a) = pnorm(a, avg, s)  Quantity normally distributed with average avg and standard deviation s

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

1-pnorm(70.5, mean(x), sd(x))     # We assume the heights are normally distributed

# As a data scientist we work with data that is technically discreet

# We can consider our adult data categorical with each specific height a unique category
# We run into issues however when one reports 70 inches and one reports 69.789876 inches
# Better to treat data as continuous, rounding to nearest inch.

# With continuous distributions, the probability of a singular values is not defined, we define probabilities for intervals

plot(prop.table(table(x)), xlab= "a = Height in inches", ylab= "Pr(X=a)")

# In cases like height, in which data is rounded, the normal distribution is particularly useful if we deal
# with intervals that include exactly one round number. i.e for 70 inches Pr(69.99<x<70.01)

# actual data, not the approximation

mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# With approximation

pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# not as useful for intervals that do not include an integer, this is called discretization
# Although true heights distribution is continuous, the reported heights tend to be more common at discrete values
# in this case due to rounding

mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))    


## 13) Probability Density  f(x)

# F(a) = Pr(X <= a) = integral(f(x)dx, -inf, a)

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

dnorm(76, mean(x), sd(x))     # pdf of 76 for heights data


## 14) Monte Carlo Simulations

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

n <- length(x)
avg <- mean(x)
s <- sd(x)

simulated_heights <- rnorm(n, avg, s)    # simulating a list that looks like our reported heights

ds_theme_set()
data.frame(simulated_heights=simulated_heights) %>% ggplot(aes(simulated_heights)) + 
  geom_histogram(color="black", binwidth=2)

B <- 1000
tallest <- replicate(B, {
  simulated_data <- rnorm(800,avg,s)   # 10000 simulations where we generate 800 normally distributed values
  max(simulated_data)
})

mean(tallest >= 7*12)  # proportion of people exceeding 7 footers


## 14) Other Continuous Distributions

# student-t, chi-squared, the exponential, gamma, beta

# r uses  d - density, q - quantile, p - Cdf, r - random ; putting these letter in the front of a shorthand distribution

# t - student t

library(tidyverse)

x <- seq(-4,4, length.out = 100)
data.frame(x, f = dnorm(x)) %>% ggplot(aes(x,f)) + geom_line()


male_avg <- 69

male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.
qnorm(.99, male_avg, male_sd)


## 15)  Random Variables

# Generating random variables

beads <- rep( c("red", "blue"), times = c(2,3))
X <- ifelse(sample(beads,1) == "blue" , 1, 0)  # Random Variable


## 16) Sampling Models

# Assume casino asks you to see if it is profitable to set up a roulette wheel
# 1000 people play, you can only bet on red or black
# Roulette wheel: 18 red, 18 black, 2 green
# If gambler wins, casino loses $1, so we draw -$1, otherwise +$1

color <- rep(c("Black", "Red", "Green"), c(18,18,2))

n <- 1000
X <- sample(ifelse(color=="Red",-1,1), n, replace=TRUE)

# Because we know the prpoportion of 1's and -1's inside the urn
# we can generate the draws with one line of code without defining color

X <- sample(c(-1,1), n, replace = TRUE, prob=c(18/36,20/36))   # sampling model

S <- sum(X)
S   # random variable

# Probability Distribution of a random variable: probability of the observed value falling in any given interval
# F(a) = Pr(S <= a)

# Simulate Distribution Function using Monte Carlo
library(tidyverse)

n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(18/36,20/36))   # sampling model
  S <- sum(X) 
})

mean( S < 0)   # probability of losing


# To define distribution we need average and standard deviation, since it is approx. normal

s <- seq(min(S), max(S), length=100)
normal_density <- data.frame(s=s, f=dnorm(s, mean(S), sd(S)))  # mean(S) - expected value   sd(S) - standard error
data.frame(S=S) %>% ggplot(aes(S,..density..)) +
  geom_histogram(color="black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data=normal_density, mapping=aes(s,f), color="blue")


## 17) Distributions vs. Probability Distributions

# Any list of numbers has a distribution F(a): what proportion of the list is less than or equal to a

# A random variable x has a distribution function 
# To define it we do not need a list of numbers, it is a theoretical concept
# F(a): what is the probability that X is less than or equal to a?

# In the case there is a list of numbers, like drawing balls from an urn
# The probability distribution of x and mean and sd, are expected value and standard errors of the r.v.


## 18) Notation for Random Variables

# Capital letters are used to denote random variables
# Lower case letters are used for observed values


## 19) Central Limit Theorem

# CLT - when the number of independent draws is large, the sum of the independent draws is approx. normal

# All we need to describe the list is the average and the standard deviation.


# Expected value - E(x) = mu   taking average of many draws, the average of the draws will approx. the expected value

# For the casino where we have +1 or -1, E[X] = (20+-18)/38 = 0.0526   a*p + b(1-p)  when there are only 2 outcomes

B <- 10^6
X <- sample(c(-1,1), B, replace = TRUE, prob=c(18/36, 20/36))
mean(X)     # just like the point above about 0.05


n <- 10000
B <- 10000

set.seed(1)

S <- replicate(B, {
  X <- sample(c(17,-1), n, replace = TRUE, prob=c(2/38,36/38))   
  S <- mean(X) 
})


mean(S)

sd(S)


# Expected value of the sum of the draws is the number of draws times the average of the numbers in the urn
# i.e. if 1000 play roulette - casino wins $50 on average


# Standard Error (SE) - size of the variation around the expected value

# If our draws are independent, then the SE of the sum = square root(number of draws)*Standard Deviation of numbers in urn

# abs(b-a)*square root((p*(1-p)))

# Roulette abs((1-(-1)))sqrt((20/36)*(18/36)))

library(tidyverse)

n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob=c(18/36,20/36))   # sampling model
  S <- sum(X) 
})

2*sqrt((20/38)*(18/38))

n <- 1000

sqrt(n)*2*sqrt((20/38)*(18/38))   # 1000 people playing, standard error $32

# For 1000 people playing

mean(S)
n*(20-18)/38   # expected value

sd(S)
sqrt(n)*2*sqrt((20/38)*(18/38))    # standard error

# We can skip Monte Carlo simulation

mu <- n*(20-18)/38
se <- sqrt(n)*2*sqrt((20/38)*(18/38))
pnorm(0, mu, se)

mean(S<0)

# Average Value for 2 outcomes: n*(outcome_1)*p+n*(outcome_2)*(1-p)

# Standard Error for 2 outcomes: sqrt(n)*abs(outcome_1-outcome_2)*sqrt(p*(1-p))


## 20) Average and Proportions

# Expected value of the sum of r.v. is the sum of the expected values of the individual r.v.
# If we are drawing from the same urn, they all have same expected value so E[X_1+X_2+...+X_n] = n*mu

# E[a*X] = a*E[X]  a-scalar constant

# E[(X_1+X_2+...+X_n)/n] = E[X_1+X_2+...+X_n]/n = n*mu/n = mu

# The square of the S.E. of the sum of independent r.v. is the sum of the square of the S.E. of each random variable

# SE[X_1+X_2+...+X_n] = sqrt(SE[X_1]^2 + SE[X_2]^2 +...+ SE[X_n]^2)

# SE[a*X] = a*SE[X]

# For same urn
# SE[(X_1+X_2+...+X_n)/n] = SE[X_1+X_2+...+X_n]/n = sqrt(SE[X_1]^2 + SE[X_2]^2 +...+ SE[X_n]^2)/n =
# sqrt(sigma^2 + sigma^2 + ... + sigma^2)/n = sqrt(n*sigma^2)/n = sigma/sqrt(n)

# If X - random variable, a*X+b - random variable


## 21) Law of Large Numbers (a.k.a Law of Averages)

# when n is very large, the average of the draws coverges to the average of the urn

# Applies only to a very large number of draws, not small samples


## 21) How Large is Large in CLT?

# In general, when probability of success is very small, we need larger sample sizes

# When the probability of success is really small, Poisson distribution is more appropriate


## 22) The Big Short: Interest Rates Explained

# Trying to predict what interest rate we should charge

# Suppose bank gives out 1000 loans for $180,000
# Suppose bank loses $200,000 per foreclosure (includes operationl costs)
# about 2% of customers default

library(tidyverse)

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02

defaults <- sample(c(0,1),n, prob=c(1-p,p), replace=TRUE)
sum(defaults*loss_per_foreclosure)


B <- 10000

losses <- replicate(B, {
  defaults <- sample(c(0,1),n, prob=c(1-p,p), replace=TRUE)
  sum(defaults*loss_per_foreclosure)
})

# Distribution of how much money we are going to lose in millions

data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth = 0.6, col="black")


# Using CLT

n*(p*loss_per_foreclosure+(1-p)*0)   # expected value

sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))  # standard error


# Interest Rate so that we break even

# l - loss per foreclosure, we need to calculate l*p+x(1-p) = 0

loss_per_foreclosure*p/(1-p) # = x  # how much we should charge interest for each mortgage


# we need to pick an interest rate that makes it more sure that we do not default, instead of break even

# Assume chance of losing money that we are willing to take is 1/100    Pr(S < 0) = 0.01

# expected value of S   {lp + x*(1-p)}*n        n - number of loans
# standard error of S   abs(x-l)*sqrt(n*p*(1-p))

# Pr(S < 0) = 0.01

# Pr((S-E[S])/SE[S] < -E[S]/SE[S])    # standardized   Pr(Z < -E[S]/SE[S] )

# Pr(Z < -{lp + x*(1-p)}*n/(abs(x-l)*sqrt(n*p*(1-p)))) = 0.01

qnorm(0.01) # =z    quantity on right

# -{lp + x*(1-p)}*n/(abs(x-l)*sqrt(n*p*(1-p)))) = z

x <- (loss_per_foreclosure*(z*sqrt(n*p_default*(1-p_default))-n*p_default))/
  (n*(1-p_default)+z*sqrt(n*p_default*(1-p_default)))


x <- 6249    #using algebra

loss_per_foreclosure*p+x*(1-p)


n <- 1000
B <- 10000
profit  <- replicate(B, {
  draws <- sample(c(x,loss_per_foreclosure),n, prob=c(1-p,p), replace=TRUE)
  sum(draws)
})

mean(profit)
mean(profit < 0)


## 23) The Big Short


library(tidyverse)

n <- 1000
loss_per_foreclosure <- -200000
l <- -200000
p <- 0.04   # probability of foreclosure
r <-0.05    # interest rate we charge
x <- r*180000

loss_per_foreclosure*p+x*(1-p) 

# E[S] = n*mu
# SE[S] = sqrt(n)*sigma

# z = -n*mu/(sqrt(n)*sigma)  from Pr(Z < 0) and standardizing

# if n >= z^2*sigma^2/mu^2   we need to find n, a number of loans that minimizes probability of a loss

z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)

n*(loss_per_foreclosure*p+x*(1-p))


p <- 0.04
r <- 0.05
x <- r*180000


B <- 10000

profit <- replicate(B,{
  draws <- sample( c(x,loss_per_foreclosure),n,
                   prob=c(1-p,p), replace=TRUE)
  sum(draws)
})

mean(profit<0)

# This assumes that one person defaulting is independent of other people defaulting


# More realistic simulation

# A global event affects everybody

# We will assume with 50-50 chance probability goes up slightly, between 0.03, 0.05 but it affects everybody
# No longer independent

n <- 1000
loss_per_foreclosure <- -200000
l <- -200000
p <- 0.04   # probability of foreclosure
r <-0.05    # interest rate we charge
x <- r*180000

loss_per_foreclosure*p+x*(1-p) 

B <- 10000

z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)


profit <- replicate(B, {
  new_p <- 0.04+ sample(seq(-0.01, 0.01, length=100),1)
  draws <- sample( c(x,loss_per_foreclosure),n,
                   prob=c(1-new_p,new_p), replace=TRUE)
  sum(draws) 
})

mean(profit)
sd(profit)   # sd shot up a lot due to nonindependence
mean(profit < 0)  # 35%   
mean(profit < -10000000)

hist(profit)  # Not normal anymore due no nonindependence




