#### Linear Regression ######

# Sabermetrics: The application of stats and data science to baseball

## 1) Baseball Basics 

# Goal: score more runs than the other team
# Each team has 9 batters that bat in a predetermined order
# After 9th batter hits we start with the first again
# Each time they come to bat: Plate Appearance (PA)
# At each plate appearance the other team's pitcher throws the ball and you try to hit it
# Each plate appearance ends in either: you make an out (failure)
# or not (success) which lets player run around bases and potentially score a run
# Each team gets 9 tries, called innings, to score runs
# Each inning ends after 3 outs
# Home run, when you hit it hard enough, gets you at least 1 run.

# There are several ways to succeed
# When you hit the ball you want to pass as many bases as possible
# There are 4 bases, 4th one is called home plate
# Start from home plate, if you get home you score a run

# 5 ways to succeed
# Base on balls (BB) - when pitcher does not pitch well and you get to first base
# Single is when you hit ball and get to first base
# Double (X2B) - when you hit ball and go past first base to second
# Triple (X3B) - when you hot ball and get to third (from home)
# Home Run - when you hit ball and go all the way home

# If you get to a base you still have a chance of getting home and scoring a run if
# the next batter hits successfully

# While you are on base you can also try to steal a base 
# if you run fast enough you can go from first to second, second to third, etc.
# without the other team tagging you

# Historically batting average has been considered the most important offensive statistic

# To define this average we define Hit (H) and At Bat (AB)
# Singles, Doubles, and Triples are Hits
# Base on Balls is not a hit

# At bat: nr. of times you make a hit or get an out
# Base on balls are exluded

# Battling Average: Hits/At Bats    H/AB    main measure of success rate

# Today ranges from 20% to 38%
# We refer to success rate in thousands: i.e if battling average is 25% it is 250

# We can examine team-level statistics


## 2) Bases on Balls or Stolen Bases

# Do teams that hit more home runs score more runs

# Visualization for exploring relationship between two variables: Scatterplot

library(Lahman)
library(tidyverse)
library(dslabs)

ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game= R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha=0.5)                               # shows strong association


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game= R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha=0.5)                               # relationship is not clear


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game= R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha=0.5)                               # pretty strong relationship

# It might appear base on balls is causing runs, when it is in fact home runs causing runs
# This is called confounding 


## 3) Correlation and Correlation Coefficient

install.packages("HistData")

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father,son)) + geom_point(alpha=0.5)


# p = (1/n)*sum(((x_i-mu_x)/sigma_x)*(y_i-mu_y)/sigma_y))  from 1 to n     p = rho

# Unrelated variables have a correlation of about 0

galton_heights %>% summarize(cor(father,son))


## 4) Sample Correlation Coefficient is a Random Variable

set.seed(0)

R <- sample_n(galton_heights, 25, replace = TRUE) %>%      # a random variable
  summarize(cor(father,son))

B <- 1000
N <- 25
R <- replicate(B , {
  sample_n(galton_heights, 25, replace = TRUE) %>%
    summarize(r=cor(father,son))   %>% .$r
})

data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color="black")

mean(R)
sd(R)     # pretty large standard error

# For Large N the CLT applies and the distribution is approx. normal

# R ~ N(p, sqrt((1-r^2)/(N-2)))


## 5) Anscombe's Quartet/Stratification

# Correlation is only meaningful in a particular context

# Creating strata of fathers with similar heights

# Trying to predict height of son given father is 72 inches tall

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

conditional_avg <- galton_heights %>% filter(round(father) == 72) %>% 
  summarize(avg=mean(son)) %>% .$avg

conditional_avg

# Boxplots

galton_heights %>% mutate(father_strata=factor(round(father))) %>%
  ggplot(aes(father_strata,son)) +
  geom_boxplot() + geom_point()

# Plot means

galton_heights %>% mutate(father=round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father,son_conditional_avg)) + geom_point()

# plotting regression line

r <- galton_heights %>% summarize(r=cor(father,son)) %>% .$r
galton_heights %>% mutate(father=round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father=scale(father),z_son=scale(son)) %>%
  ggplot(aes(z_father,z_son)) +
  geom_point() +
  geom_abline(intercept=0, slope=r)

# for every sd sigma_x above mu_x, y grows rho*sigma_y above mu_y

# ((y_i-mu_y)/sigma_y) = rho*((x_i-mu_x)/sigma_x)

# regression line: slope = rho*sigma_y/sigma_x  intercept = mu_y-m*mu_x

# using original data

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father,galton_heights$son)
m <- r*s_y/s_x
b <- mu_y-m*mu_x

galton_heights %>% ggplot(aes(father,son)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = b, slope = m)

# if we scale, intercept is 0


## 6) Bivariate Normal Distribution

# Two r.v. approximated by Bivariate Normal Distribution, scatterplot looks like ovals
# Distribution is defined for pairs

# if X, Y normally distributed r.v., and for any group of X, X=x, Y is approx. normal
# in that group, then the pair is approx. bivariate normal, i.e. conditional distribution
# of Y given X

# F_y|X=x is conditional distribution
# E(Y!X=x) is conditional expected value

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  mutate(z_father = round((father-mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() + stat_qq(aes(sample=son)) + facet_wrap(~z_father)

# If X,Y follow bivariate distribution

# E(Y|X=x) = mu_Y + rho*((X - mu_X)/sigma_X)*sigma_Y    (this is the regression line)


## 7) Variance Explained

# Var(Y | X=x) = sigma_Y*sqrt(1-rho^2)

# When 2 variables follow a bivariate normal distribution, the variation explained: rho^2*100


## 8) There are Two Regression Lines

# The lines are calculated from computing expectations, if you know E( X | Y=y) you can't get E( Y | X=x) by calculating inverse


## 9) Confounding: Are BBs More Predictive?

# Association is not causation

library(Lahman)
library(tidyverse)
library(dslabs)

ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB,HR), cor(Singles, HR), cor(BB, Singles))

# It may appear BB cause runs, but it is homeruns that cause runs

# BB are confounded with HR


## 10) Stratification and Multivariate Regression

# Stratifying Home Runs to the closest 10th

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G,1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>%
  filter(HR_strata >=0.4 & HR_strata <= 1.2)

dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~HR_strata)

# to see slopes

dat %>%
  group_by(HR_strata) %>%
  summarize(slope=cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))



dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G,1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha=0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~BB_strata)

dat %>%
  group_by(BB_strata) %>%
  summarize(slope=cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))


# Complicated to do by strata

# E[R | BB=x_1, HR = x_2 ] = beta_0 + beta_1*x_1 = beta_2*x_2


## 11) Linear Models

# Regression allows us to find relationships between two varibles while adjusting for others

# Y_i = beta_0 = beta_1*x_i + epsilon_i, i = 1,...,N

# Assume errors are independent, have expected value 0 and s.d. does not depend on i, i.e. constant

# In practice linear models are just assumed without necessarily assuming normality

# To make the intercept parameter more interpretable we write  Y_i = beta_0 + beta_1(x_i - x_bar)
# in this case beta_0 is the predicted height for the son of the average father

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

lm(son~father, data = galton_heights)


galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galton_heights)


## 12) Least Square Estimate (LSE)

# Standard approach, minimize distance of the fitted model to the data

# RSS = sum(Y_i - (beta_0 + beta_1*x_i))^2  for i = 1 .... n     Residual Sum of Squares

# beta_o_hat , beta_1_hat   -   Least Squares Estimate

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0 + beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0,1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss,beta0 = 36))   # keeping beta0 at 25 so that it is not a 3d plot

results %>% ggplot(aes(beta1, rss)) + geom_line() + geom_line(aes(beta1, rss), col=2)


## 13) the lm Function

fit <- lm(son~father, data = galton_heights)
summary(fit)


Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game= R/G, HR_per_game = HR/G)

# Baseball linear model

lm(R_per_game~BB_per_game + HR_per_game, data = Teams)


## 13) LSE are Random Variables

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# We run Monte Carlo, assuming the father son data we have defines the entire population

B <- 1000
N <- 50

lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son~father, data = .) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])

library(gridExtra)

p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth =5, color= "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth =0.1, color= "black")
grid.arrange(p1,p2,ncol=2)

sample_n(galton_heights, N, replace=TRUE) %>%
  lm(son~father, data = .) %>% summary

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# t-statistic follows assumptions that epsilon follow normal distribution

# beta_0_hat / SE(beta_0_hat)_hat , beta_1_hat / SE(beta_1_hat)_hat  follow t distribution with N-p degrees of freedom
# p - number of parameters

# If either you assume errors are normal and use the t distribution or
# you assume that N is large enough to use the CLT
# You can construct Confidence Intervals


# Although interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated

lse %>% summarize(cor(beta_0, beta_1))

# However, the correlation depends on how the predictors are defined or transformed

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%     # standardized father heights
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 


## 14) Predicted Variables are random variables

library(HistData)
library(tidyverse)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=='male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# If we plot y_hat versus x we see the regression line

galton_heights %>% ggplot(aes(son,father)) +
  geom_point() + 
  geom_smooth(method = 'lm')  # plots confidence intervals

# predict() takes an lm object as input and returns these predictions

galton_heights %>%
  mutate(Y_hat = predict(lm(son~father, data= .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()

fit <- galton_heights %>% lm(son~father, data= .)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# anoter way to plot predictions and confidence intervals

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


## 15) Advanced dplyr: Tibbles

library(Lahman)
library(tidyverse)
library(dslabs)

ds_theme_set()

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB=BB/G,
         R=R/G) %>%
  select(HR,BB,R) %>%
  filter(HR >= 0.4 & HR<=1.2)

dat %>% group_by(HR) %>%
  summarize(slope=cor(BB,R)*sd(R)/sd(BB))

# lm gives CI but is not part of tidyverse so ignores group by

dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class() 

# Tibble - special kind of data frame, default data frame for tidyverse

# select, filter, mutate, arrange return what you input, i.e. input data frame you get a data frame

# tibbles display better

Teams   # looks bad shows a lot of columns

as_tibble(Teams) # output as Tibble

# subsets of tibbles are tibbles, subsets of data frame might not be a data frame

# Tibbles can have more complex objects

# Tibbles can be grouped

# do() function - is a bridge between R functions such as lm() and the tidyverse, always returns a data.frame

dat %>% group_by(HR) %>%
  do(fit = lm(R~BB,data= .))   # not useful

get_slope <- function(data){
  fit <- lm(R~BB, data=data)
  data.frame(slope=fit$coefficients[2],
             se=summary(fit)$coefficient[2,2])
}

dat %>% group_by(HR) %>%
  do(get_slope(.))


get_lse <- function(data){
  fit <- lm(R~BB, data=data)
  data.frame(term=names(fit$coefficients),
    slope=fit$coefficients,
             se=summary(fit)$coefficient[,2])
}

dat %>%
  group_by(HR) %>%
  do(get_lse(.))


get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))


# to simplify things we will use broom

library(broom)

# tidy() - returns estimates and related information as a data frame

fit <- lm(R~BB, data=dat)
tidy(fit,conf.int=TRUE)    # outcome is a data frame

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R~BB, data = .),conf.int = TRUE)) %>%   # outcome is a data frame so we can filter etc.
  filter(term == "BB") %>%
  select(HR,estimate,conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin= conf.low, ymax=conf.high)) +
  geom_errorbar() +
  geom_point()

# glance() : model-specific outcomes

glance(fit)

# augment(): observation-specific outcomes


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 


## 16) Building a Better Offensive Metric for Baseball

# How well do BB predict runs

# Runs per game predicted by base on bals per game and home runs per game

library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
  lm(R ~ BB + HR, data=.)

tidy(fit, conf.int = TRUE)

# If we wan to construct a metric to pick players, we need to consider singles, doubles, and triples as well
# We will assume these 5 variables are jointly normal

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples=X3B/G,
         HR=HR/G,
         R=R/G) %>%
  lm(R~BB +singles + doubles + triples + HR, data=.)

tidy(fit, conf.int = TRUE)

# We can predict the number of runs for each team in 2002 (we did not use that data before)


Teams %>% filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata=.)) %>%
  ggplot(aes(R_hat, R)) + 
  geom_point() +
  geom_text(aes(label=teamID), nudge_x = .05) +
  geom_smooth(method="lm", se=FALSE, color="black")

# We have deived the metrics for teams based on team-level summary statistics

# Predict for individual players
# Per-plate appearance rate

# To make the per-game team rate comparable to the number of plate appearances per team per game averaged across all teams

pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB + BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

# We are going to computer the per-plate-appearance rates for players available in 2002, using data from 1999:2001

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit,newdata=.))

# Interpreted as number of runs we predict a team would score if all batters are exactly like that player

players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, color='black')

# To build the teams we need to know players' salary since we have a limited budget, $40mil
# We also need the players' position, we are going to need one shortstop, one second baseman, one thirdbaseman etc.

players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# We pick the one positions most played by each player using top_n, in case of ties, we take the first row
# pitchers don't bat

players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by= "playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by = "playerID")

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

players %>% ggplot(aes(salary, R_hat, color=POS)) +
  geom_point() + scale_x_log10()


# Building a Better Offensive Metric for Baseball: Linear Programming (code given not covered)

library(reshape2)
library(lpSolve)
library(tidyverse)

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

# This algorithm chooses these 9 players:
  
  our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)



# We note that these players all have above average BB and HR rates while the same is not true for singles.

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))


# On Base Plus Slugging (OPS)

# Doubles, Triples, home runs should be weighted much more than singles

# OPS =  BB/PA + (Singles + 2*Doubles + 3*Triples + 4*HR)/AB


## 17) Regression Fallacy

# Sophomore Slump - instance in which a second effort fails to live up to the standard of the first effort
# Rookie of the year - best firstyear Baseball player usually does not perform as well during the second year

library(Lahman)
library(tidyverse)

playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(Master, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# we will focus on batting average

ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by = 'playerID') %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# remove rookies that did not play a sophomore season

ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie","sophomore")) %>%
  filter(n()==2) %>%
  ungroup %>%
  select(playerID,rookie_year, rookie, nameFirst, nameLast,AVG)

ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))    # spread creates column for different type of rookie


two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%               # minimum needed to be considered
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup() %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%        # tilde key without shift
  left_join(playerInfo, by = 'playerID') %>%
  filter(POS != "P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%        # tilde key without shift
  select(-playerID)

two_years

# Top performers go down from 2013, to 2014    (these are not rookies)

arrange(two_years, `2013`)

# Bottom performers go up from 2013, to 2014

# There is no such thing as a sophomore slump

# Correlation for performance in two separate years is high but not perfect

two_years %>% ggplot(aes(`2013`,`2014`)) + geom_point()

summarize(two_years, cor(`2013`,`2014`))    # r is 0.46


## 18) Measurement Error Models

# Common to have nonrandom covariates such as time
# Randomness is introduced from measurement error rather tan sampling or natural variability

library(tidyverse)
library(dslabs)
library(broom)

falling_object <- rfalling_object()

falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

# Looks like a parabola   Y_i = beta_0 + beta_1*x_i + beta_2*x^2_i + epsilon_i , i = 1,...,n    x_i is time, y_i is meters drop
# E[epsilon] = 0 i.e. we assume no bias

fit <- falling_object %>%
  mutate(y = observed_distance,time_sq = time^2) %>%
  lm(y~time+time_sq, data=.)
tidy(fit)

augment(fit) %>%              # shows graphically the fit
  ggplot() +
  geom_point(aes(time,y)) +
  geom_line(aes(time,.fitted))

tidy(fit, conf.int = TRUE)


## 19) Correlation is not Causation: Spurious Correlation


# http://tylervigen.com/spurious-correlations
# Examples of data dredging, data phishing, data snooping

# data dredging - look though many results produced by a random process and pick the one that shows
# a rlationship that supports the theory you want to defend

library(tidyverse)
library(broom)

N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))  # sim 1mil groups each with 25 obs
# Since we constructed this simulation x and y are not correlated

res <- sim_data %>% group_by(group) %>% summarize(r = cor(X,Y)) %>% arrange(desc(r))
head(res)

sim_data %>% filter(group == res$group[which.max(res$r)]) %>% 
  ggplot(aes(X,Y)) + geom_point() + geom_smooth(method = 'lm')

res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = 'black')   # shows distribution of cor which is a rv

sim_data %>%
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y~X, data = .)))       # we are incorrectly claiming that this is statistically significant

# p-hacking:
# Looking for association between an outcome and several exposures and only reporting the one that is significant
# Trying several different models and selecting the one that yields the smallest p-value.
# Repeating an experiment multiple times and only reporting the one with the smallest p-value


## 20) Correlation is not Causation: Outliers

library(tidyverse)
library(broom)

# Another way we can see high correlation when there is no causation is outliers

set.seed(1)

x <- rnorm(100,100,1)
y <- rnorm(100,84,1)     # currently r = 0

x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

cor(x,y)  # very high due to outlier

cor(x[-23],y[-23])   # almost 0

# Spearman Correlation - robust to outliers
# Compute the correlation on the ranks of values rather than the values themselves

tibble(x,y) %>% ggplot(aes(rank(x),rank(y))) + geom_point(alpha = 0.5)

cor(rank(x),rank(y))

cor(x,y, method = "spearman")


## 20) Correlation is not Causation: Reversing Cause and Effect

# ex: claiming that tutoring makes students perform worse because they test lower than peers that are not tutored

# Using father son data, fitting the reverse model X_i = beta_0 + beta_1*y_i + epsilon_i,  i = 1,...,N
# X - father height,   Y - Son height

library(HistData)
library(tidyverse)
library(broom)

data('GaltonFamilies')

GaltonFamilies %>%
  filter(childNum == 1 & gender == 'male') %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father~son, data = .)))

# This model fits the data well, but it could be incorrectly interpreted i.e. son being tall caused father to be tall

# Reversed cause and effect examples

# Past smokers who have quit smoking may be more likely to die from lung cancer.
# it is possible that a smoker would quit after being diagnosed, but is at a higher risk of dying. 
# It does not mean that quitting smoking increases your risk of dying from lung cancer

# People with high blood pressure tend to have a healthier diet.
# This could be reverse causation. Individuals with high blood pressure may have a healthier diet 
# because they are trying to improve their blood pressure, not because a healthy diet causes high blood pressure.

# Individuals in a low social status have a higher risk of schizophrenia.
# This could be reverse causation. It is possible that individuals with schizophrenia are more likely 
# to have a lower social status due to the challenges of managing their disease.


## 21) Correlation is not Causation: Confounders

# If X and Y are correlated, we call Z a confounder if changes in Z cause changes in both X and Y

# Home Runs was a confounder that resulted in higher correlation than expected when studying
# relationship between bases on balls and runs

# In some cases we can use linear models to account for confounders, but it is not always possible

# UC Berkley admission data 1973,  44% of men admitted compared to 30% women admitted

library(dslabs)
library(tidyverse)
library(broom)


data(admissions)

admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted/100*applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))                 # rejects the hypothesis that gender and admissions are independent

# Closer inspection shows paradoxical result

admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)      # 4 out of the 6 majors favor women

# It appears that men have higher a higher admission rate than women, however, 
# after we stratify by major, we see that on average women have a higher admission rate than men


# Analyzing the totals suggests a depenence between admisions and gender

# When data is grouped by major, this dependence seems to disappear

# This can hapen if an uncounted confounder is driving most of the variability

# define: X : 1 for men, 0 for women,   Y : 1 for admitted, 0 for not admitted,  Z - how selective the major is

# Gender Bias Claim  Pr(Y = 1 | X = x)  is higher when x = 1 rather than when x = 0

# Z is associated with Y

# Is major selectivity Z associated with gender?

admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender=="women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# Plot suggests women were much more likely to apply to the two hard majors

admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")         # % of applicats accepted by gender


admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()       # Major by major there is not much difference

# easiest majors A, B more men applied to those so they got accepted more




## 21) Simpson's Paradox

# We see sign of correlation flip when computed on the entire population and when we computed on specific strata

















