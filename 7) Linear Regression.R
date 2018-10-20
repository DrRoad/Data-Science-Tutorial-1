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



