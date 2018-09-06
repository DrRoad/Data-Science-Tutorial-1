######################## Data Visualization ########################

## 1) Introduction to Distributions


## 2) Data Types

# Categorical: ordinals vs. non-ordinals
# Variales defined by a small number of groups: sex, male or female, regions of the state
# Some categorical data can be ordered: spiciness: mild, medium, hot    a.k.a. Ordinal Data (better used in different context, see below)
# Numerical: discrete vs. continuous
# Discrete numeric data can be considered ordinal: heights rouned to the nearest inch

# Ordinal Data - variables beloging to a small number of different groups with each group having many members
# Discrete numerical variables - Many group with few cases in each group

# ex.  Number of packs of cigarettes a person smokes a day rounded to nearest pack 0,1,2 - ordinal
# number of cigarettes that we spoke 0,1,2,3,36 - numerical

rm(list=ls())

library(dslabs)
data(heights)
names(heights)
x <- heights$height
length(unique(x))
tab <- table(x)


## 3) Describing Heights to ET

# The most basic statistical summary of a list of objects or numbers is its distribution
#Categorical data:  distribution describes proportions of each unique category

prop.table(table(heights$sex))

#If more than 2 categories we use a bar plot for categorical data

# For distribution of numerical data: define a function that reports proportion of the data below a value A
# for all possible values of A : Cumulative Distribution Function (CDF)  F(a) = Pr(x <= a)
# We can get proportion of values between any 2 value a and b by doing F(b)-F(a)

# More popular plot is ECDF (empirical disribution function)
# Histograms are more preferred
# To create histogram divide a span of our data into non-overlapping bins of the same size.  For each bin we count number of values in that interval


## 4) Smooth Density Plots

# No sharp edges, many local peaks have been removed
# Scale of y-axis changed from counts to density/frequency scale
# Concept of sample data
# No big jumps from bin to bin like in Histogram
# Create histogram with frequency on y-axis, keep points on heights of bin and draw smooth curve
# Smooth is a relative term, we can control the smoothness
# Histogram is assumption free, smooth density plots rely on assumptions and choices
# y-axis is scaled so that the area under the density curve = 1

# If we choose a bin of base=1, the y-value is the proportion of values in that bin
# For other base values compute proportion of area in that interval
# Advantage of smooth densities over histograms, it makes it easier to compare to distribution


## 5) Noraml Distribution

# Only defined by (m,s)  (mean, standard deviation)
# 95% values between 2 standard deviations

aveerage <- sum(x)/length(x)
SD <- sqrt(sum(x-average)^2)/length(x)

library(dslabs)
data(heights)
index <- heights$sex == "Male"
x <- heights$height[index]
average <- mean(x)
SD <- sd(x)
c(average=average, SD=SD)

# Standard Unit
z <- (x-average)/SD
z <- scale(x)

# To see how many men within 2 standard deviations
mean(abs(z)<2)


## 6) Quantile-Quantile plots (Q-Q Plots)

# Define a value p=0,1 ; 0.2 ; 0.95 etc. 
# Then we find a value q such that proportion of values below q is p

library(dslabs)
data(heights)
x <- heights$height[index]
mean(x <= 69.5)    # if p = 0.5147, q = 69.5
mean(x >=69 & x <= 72)
pnorm(72, mean = avg, sd = stdev, lower.tail = TRUE, log.p = FALSE)-pnorm(69, mean = avg, sd = stdev, lower.tail = TRUE, log.p = FALSE)
pnorm(72, 69, 3, FALSE)

p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm( p, mean = mean(x), sd = sd(x))
plot(theoretical_quantiles, observed_quantiles)    #since values almost fall on the line, data approx. normal
abline(0,1)   # intercept, slope

# Simpler if we use standard units
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


## 7) Percentiles

# p=0.25  25th percentile, 25% of data fall below this point
# p=0.5   Median
# quartiles  p=0.25, p=0.50, p=0.75
library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
female_percentiles <- quantile(female, seq(0.1,0.9,0.2))
male_percentiles <- quantile(male, seq(0.1,0.9,0.2))
df <- data.frame(females = female_percentiles, males = male_percentiles)
quantile(heights$height, seq(.01, 0.99, 0.01))

## 8) Boxplots

# Provide a 5 number summary: range, quartiles (25, 50, 75)
# Ignore outliers when computing range, plot as independent points
# Boxplot with whiskers, distance between buttom and top of box is the interquartile range
# Line in between is median
# Great to compare two distributions

## 9) Distribution of Female Heights

library(dslabs)
data(heights)
index <- heights$sex == "Female"
x <- heights$height[index]
z <- scale(x)
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)



## 10) GGPLOT

#aes in argument to call columns

library(tidyverse)   #contains dplyr and ggplot2 and other packages
# ggplot works exclusively with data tables
library(ggthemes)
library(ggrepel)

library(dslabs)
data(murders)
# 3 components: Data component
# geometry component: scatterplot, barplot, qqplot 
# Aesthetic mapping component
ggplot(data=murders)       # the two lines of code ar equivalent
murders %>% ggplot()       # blank, no geometry is defined
p <- ggplot(data=murders)

# we use a + to add layers

# Scatterplot
murders %>% ggplot() + 
  geom_point(aes(x=population/10^6,y=total))

# Labels
#geom_label and geom_text
murders %>% ggplot() + 
  geom_point(aes(x=population/10^6,y=total)) + 
  geom_text(aes(population/10^6, total, label=abb))

# Tinkering
murders %>% ggplot() + 
  geom_point(aes(x=population/10^6,y=total),size=3) + 
  geom_text(aes(population/10^6, total, label=abb),nudge_x=1)  #nudge_x moves labels a bit

# To make code more simplified to not call x and y all the time, defined labels globally
p <- murders %>% ggplot(aes(population/10^6,total, label=abb))
p + geom_point(size=3) + geom_text(nudge_x=1)   #local mappings override global ones

# Scales Labels and Colors
p + geom_point(size=3) + geom_text(nudge_x=0.075) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2009")

# More efficient
p + geom_point(size=3) + geom_text(nudge_x=0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2009")

# Colors, Legend, optional style changes
p <- murders %>% ggplot(aes(population/10^6,total, label=abb)) +
  geom_text(nudge_x=0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2009")

# Assigning categorical variable to color adds color argument and legend
p + geom_point(aes(color=region),size=3)

# To add average line
r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate
p + geom_point(aes(color=region),size=3) + geom_abline(slope=1,intercept = log10(r))

p <- p + 
  geom_abline(slope=1,intercept = log10(r),lty=2,color = "darkgrey")+
  geom_point(aes(color=region),size=3) 

# To capitalize region
p <- p + 
  geom_abline(slope=1,intercept = log10(r),lty=2,color = "darkgrey")+
  geom_point(aes(color=region),size=3) +
  scale_color_discrete(name = "Region")

# To change theme
library(ggthemes)   # adds themes
library(ggrepel)    # adds customization for labels

p+theme_economist()+geom_text_repel()   # geom_text replaced with # geom_repel

################# Putting it all together

library(tidyverse)   
library(ggthemes)
library(ggrepel)
library(dslabs)
data(murders)

# Define slope of line
r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% .$rate

# Make plot       # geom_label(aes(label=abb)) optional
murders %>% ggplot(aes(population/10^6,total, label=abb)) +
  geom_abline(slope=1,intercept = log10(r),lty=2,color = "darkgrey")+
  geom_point(aes(color=region),size=3) + 
  geom_text_repel() +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in US 2009") +
  scale_color_discrete(name = "Region") +
  theme_economist()

## 11) Other examples

library(tidyverse) 
library(dslabs)
library(dplyr)
data(heights)
heights %>% filter(sex=="Male")

# Histograms

p <- heights %>% 
  filter(sex=="Male") %>%
  ggplot(aes(x=height))

p + geom_histogram(binwidth=1, fill="blue", col="black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

# Densities

p + geom_density(fill="Blue")

heights %>% 
  ggplot(aes(height)) + geom_density(aes(group=sex))     # group puts the two sexes on one set of axis

heights %>% 
  ggplot(aes(height))+geom_density(aes(color=sex))       # color automatically puts the two sexes on one set of axis and different color

heights %>% 
  ggplot(aes(height, fill = sex)) +                      # fill fills the two densities, alpha sets a transparency so the densitities do not overlap
  geom_density(alpha=0.2) 

# QQ-Plot    needs a sample argument not x

p <- heights %>% 
  filter(sex=="Male") %>%
  ggplot(aes(sample=height))

p + geom_qq()   # assumes mean 0 sd 1

params <- heights %>% 
  filter(sex=="Male") %>%
  summarize(mean=mean(height), sd=sd(height))

p + geom_qq(dparams = params) + geom_abline()

# If we want mean 0 sd 1
heights %>% filter(sex=="Male") %>%
  ggplot(aes(sample=scale(height))) +
  geom_qq() +
  geom_abline()

# Putting plots next to each other
library(gridExtra)

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p + geom_histogram(binwidth = 1, fill = 'Blue', col="Black")
p2 <- p + geom_histogram(binwidth = 2, fill = 'Blue', col="Black")
p3 <- p + geom_histogram(binwidth = 3, fill = 'Blue', col="Black")

grid.arrange(p1,p2,p3, ncol=3)


## 12) Summarizing data with dplyr

# TO make summaries easier, summarize and group_by
# Summary will return NA for any missing values
# Access resulting values using dot placeholder
# Use range whic helps us examine data after sorting

library(tidyverse)
library(dslabs)
data(heights)

s <- heights %>% filter(sex == "Male") %>% summarize(average=mean(height), standard_deviation = sd(height))
s$average
s$standard_deviation

heights %>% filter(sex == "Male") %>% summarize(median = median(height), minimum=min(height), maximum=max(height))


## 13) Dot Placeholder

# To make dplyr function return vectors as opposed to data

data(murders)

murders <- murders %>% mutate(murder_rate=total/population*100000)
murders_d <- murders %>% mutate(murder_rate=total/population*100000) %>% summarize(mean(murder_rate))
murders_d               # counting small states just the same as the large states (doing a simple straight average of murder_rate) not correct methodology
# The US murder rate is proportional to the total US murders/total US population

us_murder_rate <- murders %>% summarize(rate= sum(total)/sum(population)*100000)
us_murder_rate     # The US murder rate is proportional to the total US murders/total US population

# dplyr does not take data frames

# Dot

us_murder_rate %>% .$rate     #makes it equivalent to US murders rate$rate      . is like placeholder for data being passed through the pipe

us_murder_rate <- murders %>% summarize(rate= sum(total)/sum(population)*100000) %>% .$rate


## 14) Group the summarize

# We may want to compute average snd sd. of heights for men and women separately.
heights %>% group_by(sex)       # new data frame, same columns as original but not necessarily same rows, stacked into 1 object

heights %>% group_by(sex) %>% summarize(average=mean(height), standard_deviation=sd(height))

murders %>% group_by(region) %>% summarize(median_rate=median(murder_rate))


## 14) Sorting data tables

# arrange

murders %>% arrange(population) %>% head()    #sorts states by population in ascending order
murders %>% arrange(murder_rate) %>% head()    #sorts states by murder rate in ascending order

murders %>% arrange(desc(population)) %>% head()    #sorts states by population in descending order
murders %>% arrange(desc(murder_rate)) %>% head()    #sorts states by murder rate in descending order

# NEsted sorting, if there are ties, to use a second column

murders %>% arrange(region, murder_rate) %>% head()      # order by region, then within each region by murder_rate

# top_n  instead of head() top_n gives n number of entries you want to see

murders %>% top_n(10, murder_rate)   # gives top 10 by murder_rate but not ordered

murders %>% arrange(desc(murder_rate)) %>% top_n(10)   # to also arrange the top 10 by murder


## 15) Other Examples

library(NHANES)
library(dslabs)
data(NHANES)
data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE)  # to ignore NA
sd(na_example, na.rm = TRUE)    # to ignore NA

tab <- NHANES %>% filter(Gender== "female",AgeDecade == " 20-29")  # filter on female, and age group 20-29

ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average=mean(BPSysAve, na.rm=TRUE),standard_deviation=sd(BPSysAve, na.rm=TRUE))

rev_ave <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average=mean(BPSysAve, na.rm=TRUE),standard_deviation=sd(BPSysAve, na.rm=TRUE)) %>% .$average  # only calls the average

NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% 
  summarize(min=min(BPSysAve, na.rm=TRUE),max=max(BPSysAve,na.rm=TRUE))         # calls min and max

NHANES %>%
  filter(Gender == "female") %>% group_by(AgeDecade) %>%
  summarize(average=mean(BPSysAve, na.rm=TRUE),standard_deviation=sd(BPSysAve, na.rm=TRUE))

NHANES %>% group_by(AgeDecade, Gender) %>% 
  summarize(average=mean(BPSysAve, na.rm=TRUE),standard_deviation=sd(BPSysAve, na.rm=TRUE))

NHANES %>% filter(AgeDecade==" 40-49", Gender=="male") %>% group_by(Race1) %>% 
  summarize(average=mean(BPSysAve, na.rm=TRUE),standard_deviation=sd(BPSysAve, na.rm=TRUE)) %>%
  arrange(average) 


## 16) Gapminder Dataset

# Created to inform people on misinformation about GPD, mortality by country, etc.

library(tidyverse)
library(dslabs)
data(gapminder)
head(gapminder)

gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)


ds_theme_set()
filter(gapminder, year==1962) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()


## 17) Faceting

# Side by side plots  stratifying variable

# facet_grid()  expects rows and column variables separated by ~

filter(gapminder, year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(continent~year)       # compares 1962 and 2012 data by continent

filter(gapminder, year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_grid(.~year)         # compares 1962 and 2012 data, no row, just 2 columns


# facet_wrap() allows to have graphs across different rows and columns

years <- c(1962,1980,1990,2000,2012)
continents <- c('Europe','Asia')

gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point() +
  facet_wrap(~year)    # better for comparing across plots  also keeps scale the same


## 18) Time Series Plots


# Time in x-axis, outcome in y-axis

gapminder %>% filter (country == "United States") %>%
  ggplot(aes(year,fertility)) +
  geom_line()


countries <- c("South Korea", "Germany")

gapminder %>% filter (country %in% countries) %>%
  ggplot(aes(year,fertility, group=country)) +         # group in order for the line not to just go through both points
  geom_line()

gapminder %>% filter (country %in% countries) %>%
  ggplot(aes(year,fertility, color=country)) +         # color automatically groups data
  geom_line()

# For time series plots labeling is preferred to legends

countries <- c("South Korea", "Germany")
labels <- data.frame(country= countries, x = c(1975,1965), y= c(60,72))

gapminder %>% filter (country %in% countries) %>%
  ggplot(aes(year,life_expectancy, color=country)) +         
  geom_line() +
  geom_text(data=labels, aes(x,y,label=country), size=5)+
  theme(legend.position = "none")     # no legend



## 19) Transformations


library(tidyverse)
library(dslabs)
data(gapminder)

# Calculating GDP dollars per day per person, GDP values adjusted for inflation already, so comparable across the years
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% filter( year == past_year & !is.na(gdp)) %>%    # per_day income distribution in 1970, ignoring NA
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black")

#Log_transformations
gapminder %>% filter( year == past_year & !is.na(gdp)) %>%    # per_day income distribution in 1970, ignoring NA
  ggplot(aes(log2(dollars_per_day))) +                       # we use base 2 not base 10 because easier to interpret
  geom_histogram(binwidth=1, color="black")      # The two bumps in plot are modes in stats, $2 per day amd $32 per day


# Transormation in plots:
# 2 ways: log values before plotting, or log scales in the axis

# If we log the data, we can more easily interpret intermediate values in the scale
# If we use log sclaes, we see the original values on the axis

gapminder %>%              
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = 'black') +
  scale_x_continuous(trans="log2")        # Instead of seeing the log values, We see original values in a log scale


## 20) Stratify in Boxplot

library(tidyverse)
library(dslabs)
data(gapminder)

length(levels(gapminder$region))      # number of regions

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))

# Reordering example

fac <- factor(c("Asia","Asia","West","West","West"))
levels(fac)
values <- c(10,11,12,6,4)
fac <- reorder(fac, values, Fun=mean)       #  reorders fac by the mean of values for each fac
levels(fac)  



p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%   # reordering by median
  ggplot(aes(region, dollars_per_day, fill = continent)) +           # to add color to define the different continents
  geom_boxplot() +                      # puts labels in alphabetic order, writes labels horizontally
  theme(axis.text.x = element_text(angle=90, hjust=1)) +        # rotates labels 90 degrees
  scale_y_continuous(trans="log2")+         # to to see data a bit better
  geom_point(show.legend = FALSE)  # Since we don't have many data points we can show data
  xlab("")
p


## 21) Comparing Distributions

library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
past_year <- 1970
present_year <- 2010

gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans= "log2") +
  facet_grid(year ~ group)     # makes histograms for each year and region

# more 2010 countries than 1970

# Redoing graphs with the same countries in both

country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1,country_list_2)    # common countries in both lists


gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans= "log2") +
  facet_grid(year ~ group)


library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
past_year <- 1970
present_year <- 2010

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1,country_list_2)    # common countries in both lists

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + 
  xlab("") +
  scale_y_continuous(trans="log2")
p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) + facet_grid(year~.)

# to ease comparisons instead of faceting we keep data from each year together

p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) 


## 22) Density Plots

gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n=n()) %>% knitr::kable()      # size of each group

# We want areas of the densities to be proportional to the size of the groups
# We multiply y-axis values by the size of the group
# geom_density computes a variable count that does multiply y-values by size of group
#  We can access these variable (i.e. count) using ..

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group=ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill=group)) +
  scale_x_continuous(trans = "log2") 
 
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~.)     # bw is binwidth



gapminder <- gapminder %>%
  mutate(group = case_when(                   # assigning groups dependent on region
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America","South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others")) 
  

gapminder <- gapminder %>%
  mutate(group= factor(group, levels=c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))    # transforming into factors
  

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight= population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill=group, weight=weight))+
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position= "stack") + facet_grid(year ~.) 


## 23) Other functions


library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
past_year <- 1970
present_year <- 2010

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country

country_list <- intersect(country_list_1,country_list_2)    # common countries in both lists


gapminder <- gapminder %>%
  mutate(group = case_when(                   # assigning groups dependent on region
    .$region %in% west ~ "West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Caribbean", "Central America","South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia","Polynesia") ~ "Pacific Islands"))


surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate= 1- sum(infant_mortality/1000*population)/sum(population))

surv_income %>% arrange(income)

# logit f(p)=log(p/(1-p))  p/(1-p) - odds - how many more people are expected to survive than to die
# Useful to highlight differences near 0 or 1

surv_income %>% ggplot(aes(income, infant_survival_rate, label=group, color=group)) +
  scale_x_continuous(trans="log2", limit=c(0.25,150))+
  scale_y_continuous(trans="logit", limit=c(0.875,.9981), breaks=c(.85,.90,.95,.99,.995,.998)) +   # breaks lets us set location of axis labels
  geom_label(size=3, show.legend = FALSE)
    
# we can not just judge off the plot, since it is on an average basis, some countries from 1 could be higher than sme countries from the other.
#Jumping to this conclusion is an ecological fallacy, since this perfect relationship from graph is observed for the averages at the regional level

# between function

tab <- gapminder %>% filter(between(year, 1960, 2010))


gapminder %>% mutate(dollars_per_day=gdp/population/365) %>% 
  filter(continent=="Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day, fill=region))+geom_density(bw=0.5, alpha=0.2, position="stack")+ 
  scale_x_continuous(trans ="log2") + facet_grid(year~.)

gapminder_Africa_2010  <- gapminder %>% mutate(dollars_per_day=gdp/population/365) %>%   
  filter(continent=="Africa" & year == 2010 & !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color=region)) + 
  geom_point() + scale_x_continuous(trans="log2")+geom_text(aes(label=country), size=4)

gapminder_Africa_comp  <- gapminder %>% mutate(dollars_per_day=gdp/population/365) %>%   
  filter(continent=="Africa" & year %in% c(1970,2010) & !is.na(dollars_per_day) & !is.na(infant_mortality)) %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color=region)) + 
  geom_point() + scale_x_continuous(trans="log2")+geom_text(aes(label=country)) + facet_grid(year~.)



######## Data Visualization Principles 

# Humans are much better at judging lengths and linear positions than areas and angles in charts

# When using bar-plots it is dishonest not to start the bars at 0

# When using position rather than length it is not necessary to include 0

# Do not distort quantities

# Order by a meaningful value, not alphabetically (like ggplot defaults to)

# Show meaningful data in plots. 
# You can add jitter so that points aren't falling on top of each other, and alpha blending

# Sometimes it is better to show distributions rather than points

# Keep the axes the same when comparing data across plots

# Align plots vertical to see horizontal changes and horizontally to see vertical changes

# Consider transfrmasions

# Ease Comparisons: Compared Visual Cues Should Be Adjacent and using color

# Usually scatterplots are best to describe relatioship between 2 numeric variables
# When comparing variables of the same type but at different time points and for relatively small number of comparisons
# We use a slope chart.  Only in geom_lines

# Bland Altman plot (Two Key Mean difference plot) shows difference versus mean

# Avoid Pseudo 3D plots. Use color to distinguish 3 lines

# Avoid too many significant digits.  Default in R for tables is to show 7 significant digits

signif()
round()




## 24) Case Study: Vaccines


# Sequential color palettes are suited for data that goes from high to low

library(RColorBrewer)
display.brewer.all(type='seq')

# Divergent colors are used to represent values that verge from a center
# We put equal value on both ends of the data range

library(RColorBrewer)
display.brewer.all(type='div')


library(tidyverse)
library(dslabs)
data("us_contagious_diseases")

the_disease <- "Measles"
dat <- us_contagious_diseases %>% 
  filter(!state %in% c("Hawaii","Alaska") & disease == the_disease) %>%  # !state removes the defined states
  mutate(rate=count/population*10000) %>%
  mutate(state=(reorder(state, rate)))

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color= "grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9, "Reds"), trans = "sqrt") + # transforming due to high values
  geom_vline(xintercept = 1963, col="blue") +  # adds a blue vertical line
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("")+
  xlab("")






the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >=10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")


avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")



us_contagious_diseases %>% filter(state=="California" & weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()

us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()


## 25) Other

library(tidyverse)

color_blind_friendly_cols <- c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
p1 <- data.frame(x=1:8, y=1:8, col = as.character(1:8)) %>% ggplot(aes(x,y,color=col))+
  geom_point(size=5)
p1+scale_color_manual(values=color_blind_friendly_cols)


library(dslabs)

data(us_contagious_diseases)
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
dat %>% mutate(state= reorder(state,rate)) %>%ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()


data("murders")
murders %>% mutate(rate = total/population*100000) %>% 
  mutate(region= reorder(region, rate, FUN=median)) %>% 
  ggplot(aes(region, rate))+geom_boxplot() + geom_point()

