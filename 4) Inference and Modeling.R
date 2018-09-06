######################## Inference and Modeling ########################

rm(list=ls())


## 1) Sampling Model Parameters and Estimates

# Scenario polsters face: We will use urn instead of voters
# Assume urn, with $25 prize
# Challenge: Guess the spread between the proportion of blue and red balls in urn
# Before we make prediction, we can take a sample with replacement from the urn
# Simulating that running pole is expensive: cost $0.10 per bead sampled - i.e if you sample 250 beads you break even
# Entry into competition can be an interval
# If the interval contains the true proportion, you get half what you paid and pass to the second phase
# In second phase, the entry with the smallest interval wins

# Assume blue - democrat, red - republican, and no other parties
# proportion of blue beads: p,  proportion of red beads: 1-p,   spread: p-(1-p) = 2p-1

# beads in the urn - population
# p - proportion of blue beads - parameter
# the sample that me take - sample

# Task of statistical inference: predict the parameter p using the observed data in the sample

library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)   # function that samples blue and red beads from an urn - in this case 25 beads

# proportion of blue beads: p,  proportion of red beads: 1-p,   spread: p-(1-p) = 2p-1
# Given that we see 13 red and 12 blue in the sample, it is unlikely that p is 0.9 or 0.1

# We want to construct an estimate of p using only the information we observe
# Estimate - summary of the observed data that we think is informative about the parameter of interest

# Assume 12 blue and 13 red,  sample proportion = 0.48
# Rerunning take_poll(25) gives us a different sample proportion
# Sample proportion is a Random Variable, we need to describe its distribution


## 2) The Sample Average

# We are proposing the use of the proportion of the blue beads in our sample as an estimate of the parameter p

# Random Variable X: 1 if blue bed, 0 if red bead
# If we sample N beads, then the average of the drawn X_1 through X_N is equivalent to the proportion of blue beads in our sample
# Assume draws are independent and with replacement

# We do not know what is in the urn, how many red and blue beads


## 3) Polling versus Forecasting

# if a poll is conducted four months before the election it is estimating the p for that moment, not for election day
# Polls provided the night before the election tend to be the most accurate
# Forecasters try to build tools that model how opinions vary across time and try to predict the election day result,
# taking into conseideration the fact that opinions fluctutate


## 4) Properties of Our Estimate

# E(N*X_bar) = N*p,   E(X_bar) = p
# SE(N*X_bar)  (1-0)*sqrt(p*(1-p)) = sqrt(p*(1-p)), SE(X_bar) = sqrt(p*(1-p)/N)
# In our case we do not know p or N since we do not know how many beads there are, and what actual p is

N <- seq(10,10000)
p <- 0.51               # assume this were the case
SE <- sqrt(p*(1-p)/N)
df <- data.frame(N=N, SE=SE)
df %>% ggplot(aes(N,SE)) + geom_line() + geom_hline(yintercept = 0.01, color="red")   # we want a SE of 0.01

# Usually sample size in opinion polls is 500 to 3500
# if p = 51% and N = 1,000,  SE is 1.5%


p <- seq(0,1,length=100)   # how many subpoints

sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

for(i in sample_sizes){
  se <- sqrt(p*(1-p)/i) 
  plot (p, se, ylim=c(0,0.11)) 
}


## 4) The Central Limit Theorem in Practice

# if X normal (mu, sigma),  X/a normal (mu/a, sigma/a)  a - non zero constant
# This implies the distribution of X_bar is approx. normal

# E[X_bar] = p   SE[X_bar] = sqrt(p*(1-p)/N)


# What is the probability that we are within 1 percentage point of p?   Pr(abs(X_bar-p)) <= 0.01
# Pr(X_bar <= p + 0.01) - Pr(X_bar <= p- 0.01)

# Pr(Z <= (p+0.01-E(X_bar))/SE(X_bar)) - Pr(Z <= (p-0.01-E(X_bar))/SE(X_bar))
# Pr(Z <= (0.01/sqrt(p*(1-p)/N)) - Pr(Z <= (-0.01/sqrt(p*(1-p)/N))

# We don't know p but we use a plug_in estimator  SE_hat(X_bar) = sqrt(X_bar*(1-X_bar)/N))     hat denotes estimates

# for the sample of 12 blue balls and 13 red balls    X_bar = 0.48

N_sample <- 25
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/N_sample)
se

# since we wanted to be within 1%

pnorm(0.01/se)-pnorm(-0.01/se)    # really small chance we will as close as this to the actual proportion


## 5) Margin of Error

# Margin of error = 2*SE

N_sample <- 25
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/N_sample)

2*se   # Margin of error

# Traditionally we want 95%

N_sample <- 2000
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/N_sample)
se   # Margin of error 2*SE   2%


## 6) A Monte Carlo Simulation for the CLT

library(tidyverse)
library(gridExtra)

B <- 10000
N <- 1000
p <- 0.45 #issues we do not know p so we choose 1
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  mean(X)
})

mean(X_hat)
sd(X_hat)

p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) + geom_histogram(bindwidth = 0.005, color="black")

p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) + 
  stat_qq(dparams = list(mean=mean(X_hat), sd=sd(X_hat)))+
  geom_abline() + ylab("X_hat") + xlab('Theoretical normal')
grid.arrange(p1, p2, nrow=1)

# In real life we do not know p, so we could not run this, but we could run it for different values of p


## 7) The Spread

# The competition is to predict the spread not the proportion p
# Due to there being only 2 parties we know 2p-1, we did everything to estimate 2p-1 ( p-(1-p))
# Spread: 2*X_bar-1  with SE 2*SE(X_bar)

# Our estimate of p was 0.48, margin of error 0.2
# Estimate of spread 0.04, margin of error 0.4 (40%)


## 8) Why Not Run a Very Large Poll?

# Assuming 

N <- 100000
p <- seq(0.35, 0.65, length=100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p, SE)) + geom_line()

# Polling is much more complicated than pulling beads from an urn
# People are more complicated and might lie
# We actually don't know for sure who is in our population
# How do we know they are going to vote
# Are we reaching all possible voters?

# This is bias
# Typical bias is 1%-2%


# errors p-X_bar which is a random variable

p <- 0.45
N <- 100
B <- 10000

set.seed(1)


take_sample <- function(p,N){X <-sample(c(0,1),size=N, replace=TRUE, prob=c(1-p,p)) 
mean(X)}

errors <- replicate(B, p - take_sample(p, N))

mean(errors)
sqrt(mean(errors^2))


# p - unknown

N <-100
X_hat <- 0.51

se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger

1-(pnorm(0.01, mean=0, sd=se_hat)-pnorm(-0.01,mean=0, sd=se_hat))   # probability in the 2 tails, symmetric
2*pnorm(-0.01, mean=0, sd=se_hat)      # same as above


## 9) Confidence Intervals

library(tidyverse)

data("nhtemp")
data.frame(year=as.numeric(time(nhtemp)), temperature=as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# [X_bar - 2*SE_hat(X_bar) ; X_bar + 2*SE_hat(X_bar)] probability that interval includes p

# The end-points of above interval are a random variable

p <- 0.45
N <- 1000

X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    
c(X_hat-2*SE_hat, X_hat+2*SE_hat)

# If we repeat above code we get different confidence intervals

# Pr(X_bar - 2*SE_hat(X_bar) <= p <= X_bar + 2*SE_hat(X_bar)) what we want
# Pr(-2 <= (X_bar-p)/SE_hat <=2 )   approx normal mean 0 sd 1
# Pr(-2 <= Z <=2 )    about 95%

# For other CI  z <- qnorm(1-(1-a)/2)   a - confidence level


## 10) Monte Carlo Simulation for Confidence Intervals

# 95% CI means it includes p 95% of the time

library(tidyverse)

B <- 10000

p <- 0.45
N <- 1000

inside <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)    
  between(p, X_hat-2*SE_hat, X_hat+2*SE_hat)
})

mean(inside)


## 11) The Correct Language

# Intervals are random, not p

# 95% refers to the probability that the random interval falls on top of p


## 12) Power

library(tidyverse)

N <- 25
X_hat <- 0.48     # spread example   X_hat-(1-X_hat)
(2*X_hat-1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/N)   # includes 0 which includes tie

# it includes 0 we have a small sample size, a lack of power

# in context of polls: power = Pr(detecting a spread different from 0)
# We need to increase sample size to lower SE


## 13) p-values

# Question on beads could be are there more blue than red, i.e. 2p-1 > 0

# Assume we got 52 blue beads when picking 100 beads, i.e spread = 4%, 2p-1=0.04
# Null hypothesis is the skeptics hypothesis: the spread is 0
# p- value - how likely is it to see a value this large when the null hypothesis is true?

# Pr(abs(2p-1)>0.04)   Pr(abs(p-0.5)>0.02)

# Pr(abs(X_bar-0.5)/sqrt(0.5*(1-0.5)/N)) > 0.02/sqrt(0.5*(1-0.5)/N)))
# Pr(abs(z) > 0.02/sqrt(0.5*(1-0.5)/N))))

N = 100
z <- 0.02/(0.5/sqrt(N))

1-(pnorm(z)-pnorm(-z))

# if a (1-a) confidence interval does not include 0, then we know the p-value must be smaller than a

# In general we prefer confidence intervals over p values since it gives us an idea of size of estimate
# p-value reports a probability, says nothing about significance of finding in context of problem


library(dslabs)
# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate >= '2016-10-31' & state=='U.S.')

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
print(N)
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
print(X_hat)
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
print(se_hat)
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
qnorm(1-0.05/2)
ci <- c(X_hat-qnorm(1-0.05/2)*se_hat,X_hat+qnorm(1-0.05/2)*se_hat)

pollster_results <- polls %>% 
  mutate(X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize), 
         lower=X_hat-qnorm(1-0.05/2)*se_hat, upper=X_hat+qnorm(1-0.05/2)*se_hat) %>% 
  select(pollster, enddate,X_hat, se_hat, lower, upper)

p <- 0.482 # actual result
avg_hit <- pollster_results %>% mutate(hit = p >=lower & p <= upper) %>% summarize(mean(hit))


polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% 
  mutate(d_hat=rawpoll_clinton/100-rawpoll_trump/100)

N <- polls$samplesize[1]
d_hat <- polls$d_hat[1]
X_hat <- (d_hat+1)/2 # from d_hat = 2p-1
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
ci <- 2*X_hat-1+c(-qnorm(1-0.05/2),qnorm(1-0.05/2))*se_hat


## 14) Poll Aggregatots

# Simulate results for 12 polls taken the week before the election

library(tidyverse)

d <- 0.039   # Obama's winning margin
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516) # actual poll nr.
p <- (d+1)/2      # proportion of people voting for Obama

confidence_intervals <- sapply(Ns, function(N) {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat-2*SE_hat, X_hat+2*SE_hat ) -1
})

polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),    # transpose
                    sample_size=Ns)
names(polls) <- c("poll", "estimate","low","high","sample_size")
polls

# we are going to combine all the polls in one large aggregator

sum(polls$sample_size)

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size)/sum(sample_size)) %>% .$avg  # weighted average

p_hat <- (d_hat+1)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))  # margin of error
moe

round(d_hat*100,1)
round(moe*100,1)

# Actual polling is more complicated and uses statistica models


## 15) Poll Data and Pollster Bias

library(tidyverse)
library(dslabs)

data("polls_us_election_2016")
names(polls_us_election_2016)

polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c('A+','A',"A-","B+") | is.na(grade)))

polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)  #spread estimate

# we assume only 2 parties ad we call p - proportion voting for Clinton

# We have different spreads from different polls, this is a r.v.

d_hat <- polls %>%
  summarize(d_hat=sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat

p_hat <-(d_hat+1)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# actual spread was 2.1% way outside the 95% confidence interval

polls %>% ggplot(aes(spread)) + geom_histogram(color="black",binwidth= .01)  # data not normally distributed

polls %>% group_by(pollster) %>%
  filter(n()>=6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

polls %>% group_by(pollster) %>%
  filter(n()>=6) %>%
  summarize(se= 2*sqrt(p_hat*(1-p_hat)/median(samplesize)))
  
# The theory we learned says nothing about polls with different expected values

# This is pollster bias or house effects



## 16) Data-Driven Models

library(tidyverse)
library(dslabs)

data("polls_us_election_2016")

# For each polster we are going to use their last reported result before the election

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate==max(enddate)) %>%
  ungroup()

one_poll_per_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth=0.01)

# Rather than having beads with 0 and 1 in the urn, now we have poll results

# Expected value of urn is the actual spread d = 2*p-1

# Our urn now contains continuous numbers between -1 and 1

# Standard error now includes pollster to pollster variability

# Two unknowns now, expected value d and our variability sigma

# We don't know sigma, we can estimate it with the sample standard deviation

# sample standard deviation: s = 1/(N-1)*sum((X_i-X_bar)^2, i in (1...N))

sd(one_poll_per_pollster$spread)    # sd computes sample standard deviation

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se=sd(spread)/sqrt(length(spread))) %>%
  mutate(start=avg-1.96*se, end= avg+1.96*se)
round(results*100,1)


library(dslabs)
data(heights)

x <- heights %>% filter(sex == "Male") %>%
  .$height

mean(x)
sd(x)

N <- 50   # number of people measured

X <- sample(x, size=N, replace=TRUE)

mean(X)
sd(X)

se <- sd(X)/sqrt(N)
ci <- mean(X) + c(-1,1)*qnorm(1-0.05/2)*se

mu <- mean(x)  # population mean

B <- 10000

res <- replicate(B, {
  X <- sample(x, size=N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(1-0.05/2)*sd(X)/sqrt(N)
  between(mu, mean(X)-qnorm(1-0.05/2)*sd(X)/sqrt(N), mean(X)+qnorm(1-0.05/2)*sd(X)/sqrt(N))
})
mean(res)


library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster,spread))+geom_boxplot() + geom_point()

sigma <- polls %>% group_by(pollster) %>% summarize(s=sd(spread))

res <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread), N = n()) 

estimate <- max(res$avg)-min(res$avg)

se_hat <- sqrt(res$s[2]^2/res$N[2]+res$s[1]^2/res$N[1])

ci <- estimate + c(-1,1)*qnorm(1-0.05/2)*se_hat

# Calculate the p-value
2*(1-pnorm(estimate/se_hat))


polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

var <- polls  %>% group_by(pollster) %>% summarize(avg=mean(spread), s= sd(spread))


## 17) Bayes' Theorem

# Suppose test for cystic-fibrosis has  a 99% accuracy
# Prob( + | D=1 ) = 0.99 ,   Prob( - | D=0 ) = 0.99

# Suppose we select one random person and they test positive, Pr that they have the disease
# Pr( D=1 | + )

# Cystic fibrosis rate = 1/3900
# Pr( D=1 ) = 0.00025

# Bayes's Theorem   Pr(A|B) = Pr(A & B)/ Pr(B) = Pr( B|A )*Pr(A)/ Pr(B)

# Pr( D=1 | + ) = Pr(+|D=1)*Pr(D=1)/Pr(+)
# Pr( D=1 | + ) = Pr(+|D=1)*Pr(D=1)/(Pr(+|D=1)*Pr(D=1) + Pr(+|D=0)*Pr(D=0))

# Plugging in we get 0.02   2%

# This says that despite the test having 99% accuracy, the probability of having the disease
# given a positive test is 2%


prev <- 1/3900
N <- 100000

outcome  <- sample(c("Disease","Healthy"), size=N,replace=TRUE, prob=c(prev, 1-prev))

N_D <- sum(outcome == "Disease")
N_H <- sum(outcome == "Healthy")

# since N_H so high, Pr of false positives it quite high

accuracy <- 0.99
test <- vector("character",N)
test[outcome=="Disease"] <- sample(c("+","-"),N_D, replace=TRUE,prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"] <- sample(c("-","+"),N_H, replace=TRUE,prob=c(accuracy,1-accuracy))

table(outcome, test)

# Disease+/(Disease+ + Healthy +) is about 2%


## 18) Bayes in Practice

# Bayesian models = Hierarchical models

# Baseball example:

# Start: April, At Bats: 20, H: 9, Avg: 0.450

# We will try to predict batting average at end of season

# We can think of hit as a binomial

# se: sqrt(0.45(1-0.45)/20) = 0.111

# 95% confidence interval is huge, 0228, 0.662

# Issues: we imply that someone will break the record of 0.45, and large interval


# Better way:

# We look at distribution of batting averages for all players with more than 500 at bats
# for 2010, 2011, 2012 season

# Average player had average of 0.275   sd: 0.027


## 19) The Hierarchical Model

# we pick a player and record p proportion of being successful
# we see 20 random outcomes with success probability p

# Bayesian approach incorporates data from the past

# Model has 2 levels of variability:
# Each player is assigned natural ability to hit at birth we will use p
# p has normal distribution

# we know expected value is 0.270, sd 0.027

# Second level of variability is luck

# p ~ N(mu, tau) describes randomness of picking player   (sstandard error tau)

# Y | P ~ N(p, sigman)  describes randomness if the perforance of this particular player

# in our case p = 0.275, tau = 0.027, sigma=sqrt(p*(1-p)/N)

# First level: prior distribution
# Second level: sampling distribution

# For Jose's data: wantto predict his innate ability in the form of his true batting average p
# the 0.45 batting average he had can also be due to luck

# p ~ N (0.275, 0.027)
# Y | P ~ N (p, .111)

# Compute posterior distribution  Pr(p | Observed data Y)

# We need to use continuous version of Bayes, since normal distribution is continuous

# E(p | Y) = B*mu + (1-B)*Y = mu + (1-B)*(Y-mu)     B = sigma^2/(sigma^2+tau^2)
# SE(p | Y)^2 = 1/(1/sigma^2+1/tau^2)

# weighted aerage of mu (average of all baseball players) and what we observed Y
# this weighted average aso referred to as shrinking

# E(p | Y=0.45) = 0.285
# SE(p | Y) = 0.0263

# 95% credible interval      p is random

# E(p | Y) + c(-1,1)*qnorm(1-0.05/2)*SE(p | Y)


# Case sudden infant death syndrome (SIDS)
# Mother had 2 babies die of SIDS    (not independent biologically speaking)
# Pr (Baby dying of SIDS) = 1/8500
# Pr(mother is a murderer???two children found dead with no evidence of harm) = 

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS (given first one died of SIDS)
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2   

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_BA*Pr_A/(Pr_B)


library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(nrow(polls)))

mu <- 0   # because both parties were close
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results
sigma=results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <-(sigma^2)/(sigma^2 +tau^2)
print(B)

# Calculate the expected value of the posterior distribution
exp_value <- B*mu+(1-B)*Y

# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/(1/tau^2+1/sigma^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- exp_value + c(-1,1)*qnorm(1-0.05/2)*se

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0

p_calc <- function(taus){
  B <- sigma^2/(sigma^2+taus^2)
  exp_value <- B*mu+(1-B)*Y
  se <- sqrt( 1/ (1/sigma^2 + 1/taus^2))
  pnorm(0, exp_value, se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- sapply(taus,p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)


## 20) Election Forecasting

# We use Hierarchical Model

# d ~ N(mu, tau) describes our best guess had we not seen any polling data    d - spread
# X_bar | d ~ (d, sigma) describes the randomness due to sampling and the pollster effect

# mu = 0 interpreted as model that does not provide any info on who will win
# tau = 0.035  using historical data winner of popular vote has average spread of 3.5%

library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  #filter(enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(nrow(polls)))

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2+1/tau^2))

# Posterior distribution is also normal

posterior_mean+c(-1.96,1.96)*posterior_se

1-pnorm(0, posterior_mean,posterior_se)   # very high

# in general these models do not account for general bias

# in 2016, polls were biased in favor of Democrats by 1-2%, we ony find this out after election


## 21) Mathematical Represenation of Models

# X_j = d + epsilon_j  ( d + error_term )   j - different polls
# epsilon_j ~ N(0, 2*sqrt(p*(1-p)/N))

# assuming d=2.1 samplesize=2000 we can simulate 6 data points

J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d + rnorm(J, mean=0, sd=2*sqrt(p*(1-p)/N))

# X_ij  i - pollster   j - jth poll from a given pollster

# X_ij = d + epsilon_ij 

I <- 5    # pollsters
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, mean=0, sd=2*sqrt(p*(1-p)/N))
})

# model does not show pollster to pollster variability

# h_i - house effect for ith pollster

# X_ij = d + h_i +epsilon_ij 

I <- 5    # pollsters
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, mean=0, sd=2*sqrt(p*(1-p)/N))
})

# b - general bias variability   taken from historical data  0.025

# X_ij = d + b + h_i +epsilon_ij 

# X_bar = d + b + 1/N*sum(X_i, i from 1 to N)
# se = sqrt(sigma^2/N + sigma_b^2)

# It doesn't matter how many polls you take, this bias does not get reduced by takig averages

library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state="U.S" & enddate >= "2016-10-31" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(nrow(polls)))

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2+1/tau^2))
1-pnorm(0, posterior_mean,posterior_se) 


## 22) Predicting the Electoral College

library(tidyverse)
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

results <- polls_us_election_2016 %>% 
  filter(state != "U.S." & !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg=mean(spread), sd=sd(spread), n=n()) %>%
              mutate(state = as.character(state))

results %>% arrange(abs(avg))

results <- left_join(results, results_us_election_2016, by= "state")  

results_us_election_2016 %>% filter(!state %in% results$state)


results  <- results %>% 
  mutate(sd=ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd))  # for states with 1 poll

# To keep is simple we assign the same prior to each state
# We assume we know nothing about election and results from year to year don't change much in each state

mu <- 0
tau <- 0.02

results %>% mutate(sigma=sd/sqrt(n),
                   B=sigma^2/(sigma^2+tau^2),
                   posterior_mean = B*mu+(1-B)*avg,
                   posterior_se = sqrt(1/(1/sigma^2+1/tau^2))) %>%
  arrange(abs(posterior_mean))

mu <- 0
tau <- 0.02

clinton_EV <- replicate(1000, {
  results %>% mutate(sigma=sd/sqrt(n),
                     B=sigma^2/(sigma^2+tau^2),
                     posterior_mean = B*mu+(1-B)*avg,
                     posterior_se = sqrt(1/(1/sigma^2+1/tau^2)),
                     simulated_result=rnorm(length(posterior_mean), posterior_mean,posterior_se),
                     clinton = ifelse(simulated_result >0, electoral_votes,0 )) %>%
    summarize(clinton=sum(clinton)) %>%
    .$clinton + 7  # 7 for Rhode Island and D.C.
})
mean(clinton_EV > 269)

data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=269)

# this model ignores general bias

mu <- 0
tau <- 0.02
bias_sd <- 0.03

clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma=sqrt(sd^2/n+bias_sd^2),
                     B=sigma^2/(sigma^2+tau^2),
                     posterior_mean = B*mu+(1-B)*avg,
                     posterior_se = sqrt(1/(1/sigma^2+1/tau^2)),
                     simulated_result=rnorm(length(posterior_mean), posterior_mean,posterior_se),
                     clinton = ifelse(simulated_result >0, electoral_votes,0 )) %>%
    summarize(clinton=sum(clinton)) %>%
    .$clinton + 7  # 7 for Rhode Island and D.C.
})
mean(clinton_EV_2 > 269)

data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +   # more variability
  geom_histogram(binwidth=1) +
  geom_vline(xintercept=269)

results %>% mutate(sigma = sd/sqrt(n), 
                   B = sigma^2 / (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1/ (1/sigma^2 + 1/tau^2))) %>%
  ggplot(aes(avg, posterior_mean, size = n)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)

## 23) Forecasting

# Election forecasters musk ask how informative are polls taken several weeks before election

library(tidyverse)
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

one_pollster <- polls_us_election_2016 %>%
  filter(pollster=="Ipsos" & state=="U.S.") %>%  # one pollster so there is no pollster effect
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)

se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se


one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth=0.01, color='black')

one_pollster %>% ggplot(aes(enddate,spread)) +
  geom_line()   # variability due to time which is not accounted for when we assumed p constant


polls_us_election_2016 %>%
  filter(state=="U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n()>10) %>%
  ungroup() %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100) %>%
  ggplot(aes(enddate,spread)) + geom_smooth(method = "loess", span=0.1)+
  geom_point(aes(color=pollster), show.legend = FALSE, alpha=0.6)

# this implies we need to add a term to the model for the time bias effect

# Y_ijt = d + b + h_j + b_t + epsilon_ijt  s.d. of b_t dependent on time, the closer to election the smaller

# Pollsters also estimate trends f(t)
# Blue line in above graph is estimate of f(t)

# Y_ijt = d + b + h_j + b_t + f(t) + epsilon_ijt 
  
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>% 
  mutate(candidate = factor(candidate, levels = c("Trump","Clinton")))%>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +  
  geom_point(show.legend = FALSE, alpha=0.4)  + 
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))



library(tidyverse)
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that columns for the lower and upper confidence intervals. Select the columns indicated in the instructions.
cis <- polls %>% mutate(X_hat=(spread+1)/2, se= 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower=spread-qnorm(1-0.05/2)*se, upper=spread+qnorm(1-0.05/2)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits <- cis %>% mutate(hit=lower<=actual_spread & actual_spread<=upper) %>% 
  summarize(proportion_hits = mean(hit))

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has more than 5 polls.

p_hits <- cis %>% mutate(hit=lower<=actual_spread & actual_spread<=upper) %>% 
  group_by(pollster) %>% filter(n()>=5) %>% 
  summarize(proportion_hits = mean(hit), n = n(), grade=grade[1]) %>% 
  arrange(desc(proportion_hits))

ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits <- ci_data %>% mutate(hit=lower<=actual_spread & actual_spread<=upper) %>% 
  group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits = mean(hit), n = n()) %>% 
  arrange(desc(proportion_hits))

p_hits$state <- factor(p_hits$state, levels = p_hits$state)

p_hits %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat="identity") + coord_flip()

errors <- cis %>% mutate(error=spread-actual_spread, hit=sign(spread)==sign(actual_spread))

errors <- cis %>% 
  mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls

p_hits <- errors %>% group_by(state) %>% filter(n()>5) %>% 
  summarize(proportion_hits=mean(hit),n=n()) %>% arrange(desc(proportion_hits)) 

cis <- p_hits

cis$state <- factor(cis$state, levels = cis$state)

cis %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat="identity") + coord_flip()

hist(errors$error)    # shows bias since not centered around 0
median(errors$error)

errors %>% filter(grade %in% c("A+","A","A-","B+")) %>% 
  mutate(state=reorder(state, error))  %>% 
  ggplot(aes(state, error))+geom_boxplot()+geom_point() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

errors %>% filter(grade %in% c("A+","A","A-","B+")) %>% group_by(state) %>% 
  filter(n()>=5) %>% ungroup() %>%  mutate(state=reorder(state, error)) %>% 
  ggplot(aes(state, error))+geom_boxplot()+geom_point() + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


## 24) The t-Distribution

# For values smaller than 30 we need to be cautious about using the CLT

# If we know data to follow normal distribution we can create confidence intervals for any n

# Z = (X_bar-d)/(sigma/sqrt(N))

# Usually we do not know sigma so we use s, but s adds vaiability to data

# This makes it that Z follows t-distribution with N-1 degrees of freedom

z <- qt(1-0.05/2, 15-1)   # equivalent 95%

# Calculate the probability of seeing t-distributed random variables 
# being more than 2 in absolute value when 'df = 3'.

pt(-2,3)+1-pt(2,3)

df <- seq(3,50,1)

pt_func <- function(df){
  pt(-2,df)+1-pt(2,df)
}
probs <- sapply(df,pt_func)
plot(df,probs)


library(dslabs)
library(dplyr)
data(heights)


x <- heights %>% filter(sex == "Male") %>%
  .$height
mu <- mean(x)
N <- 15
B <- 10000

set.seed(1)

res <- replicate(B, {
  sam <- sample(x, size=N, replace=TRUE)
  interval <- mean(sam)+c(-qnorm(1-0.05/2),qnorm(1-0.05/2))*sd(sam)/sqrt(N)
  between(mu,mean(sam)-qnorm(1-0.05/2)*sd(sam)/sqrt(N),mean(sam)+qnorm(1-0.05/2)*sd(sam)/sqrt(N))
})
mean(res)

res <- replicate(B, {
  sam <- sample(x, size=N, replace=TRUE)
  interval <- mean(sam)+c(-pt(1-0.05/2,N-1),pt(1-0.05/2,N-1))*sd(sam)/sqrt(N)
  between(mu,mean(sam)-qt(1-0.05/2,N-1)*sd(sam)/sqrt(N),mean(sam)+qt(1-0.05/2,N-1)*sd(sam)/sqrt(N))
})
mean(res)
  

## 25) Association Tests

# We have not discussed inference for binary, categorical or ordinal data

library(tidyverse)
library(dslabs)
data("research_funding_rates")   # gender bias favoring male over female

totals <- research_funding_rates %>% 
  select(-discipline) %>%
  summarise_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men-awards_men,
            yes_women = awards_women,
            no_women = applications_women-awards_women)

totals %>% summarize(percent_men=yes_men/(yes_men+no_men),
                     percent_women=yes_women/(yes_women+no_women))


tab <- matrix(c(3,1,1,3),2,2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab     # two-by-two table

fisher.test(tab, alternative = "greater")


## 26) Chi Squared Test

# 2823 individuals, some are men, some are women, some get funded some don't
# success rates   18% men, 15% women

library(tidyverse)
library(dslabs)
data("research_funding_rates")   # gender bias favoring male over female

totals <- research_funding_rates %>% 
  select(-discipline) %>%
  summarise_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men-awards_men,
            yes_women = awards_women,
            no_women = applications_women-awards_women)

totals %>% summarize(percent_men=yes_men/(yes_men+no_men),
                     percent_women=yes_women/(yes_women+no_women))

funding_rate <- totals %>%
  summarize(percent_total = 
              (yes_men+yes_women)/(yes_men+no_men +yes_women+no_women)) %>%
  .$percent_total
funding_rate

# Null hypothesis, difference in funding the same if funding assigned at random

# Compare observed 2 by 2 table to what you expect to see at the overall funding rate

# Chi-Squared test tells us how likely it is to see a deviation this large or larger by chance

two_by_two <- tibble(awarded=c("no","yes"),
                     men=c(totals$no_men,totals$yes_men),
                     women=c(totals$no_women,totals$yes_women))
two_by_two

tibble(awarded=c("no","yes"),    # overall funding rate
       men=c(totals$no_men+totals$yes_men)*c(1-funding_rate, funding_rate),
       women=c(totals$no_women+totals$yes_women)*c(1-funding_rate, funding_rate)) 

two_by_two %>% select(-awarded) %>% chisq.test()     # 5.1% to be at random


# Summary Statistics

# For two-by-two tables is the odds ratio

# X = 1 if male or 0 otherwise,  Y = 1 if funded or 0 otherwise

# Odds of getting funded if male  Pr(Y = 1 | X = 1) / Pr(Y = 0 | X = 1)

odds_men <- (two_by_two$men[2]/sum(two_by_two$men)) / (two_by_two$men[1]/sum(two_by_two$men))
odds_women <- (two_by_two$women[2]/sum(two_by_two$women)) / (two_by_two$women[1]/sum(two_by_two$women))

# Odds_ratio : ratio of 2

odds_men/odds_women   # 1.23 more    may not be practically significant

# Can not just base reults of p-value, small p-value does not mean large odds ratio!!!


library(tidyverse)
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

cis <- polls %>% mutate(X_hat=(spread+1)/2, se= 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower=spread-qnorm(1-0.05/2)*se, upper=spread+qnorm(1-0.05/2)*se) %>% 
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
cis <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits <- cis %>% mutate(hit=lower<=actual_spread & actual_spread<=upper) %>% 
  summarize(proportion_hits = mean(hit))
errors <- cis %>% mutate(error=spread-actual_spread, hit=sign(spread)==sign(actual_spread))

totals <- errors %>% filter(grade %in% c("A-","C-")) %>% 
  group_by(grade,hit) %>% summarize(n=n()) %>% spread(grade,n)

totals <- totals[c(2,1),c(1,3,2)]

totals[[1,2]]/sum(totals[,2])
totals[[1,3]]/sum(totals[,3])

chisq_test <- totals %>% select(-hit) %>% chisq.test
print(chisq_test$p.value)

odds_C <- totals$"C-"[1]/totals$"C-"[2]
odds_A <- totals$"A-"[1]/totals$"A-"[2]

odds_A/odds_C



