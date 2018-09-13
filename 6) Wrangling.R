######  Wrangling   ########


## 1) Importing Spreadsheets

# A spreadsheet is a file version of a Data Frame

# For a simple text file, a new row is definedwith a return
# A new column is defined with a space, comma, tab, semicolon

# It is important to know if a file has a header or not

# Generally it is not recommended to use Excel, but text file, google spreadsheets, etc.


## 2) Paths and the Working Directory

# Important to know your working directory

getwd()   # to see the working directory
setwd()   # to set the working directory

# Unless a full path is provided , they search for files in the working directory

system.file("extdata", package="dslabs")   # looks for the directory extdata of the package dslabs

path <- system.file("extdata", package="dslabs")
filename <- "murders.csv"
fullpath <- file.path(path, filename)
list.files(path)   # shows files in the specified directory
file.copy(fullpath, getwd())    # copies files into wd

file.exists(filename)   # checks if the file exists in your wd


## 3) The readr and readxl Package

library(tidyverse)
library(readxl)

# readr tidyverse package that reads data in txt files into R

read_table()    # white space separated values
read_csv()      # comma separated values
read_csv2()     # semicolon separated values
read_tsv()      # tab delimited separated values 
read_delim()    # general text file format, must define delimiter

read_excel()    # auto-detects formal .xls or .xlsx
read_xls()      # reads xls
read_xlsx()     # reads xlsx

excel_sheets()  # gives us the names of the sheets in an excel file

read_lines()    # shows us the first few lines of a file in R

read_lines(
  "C:/Users/wiers/Desktop/Data Science/Data-Science-Tutorial/murders/data/murders.csv", 
  n_max=3)      # max number of rows, also shows if header

# since murders has headers and has , we use read_csv

# the imported data is a tibble


## 4) Importing Data using R-base Functions

# R- base has similar functions, but they create dataframe, not a tibble
# Characters are converted to factors, can be avoided by setting stringsAsFactors=FALSE

read.table("murders.csv", stringsAsFactors = FALSE)
read.csv()
read.delim()


## 5) Downloading Files from the Internet

# We can read files in directly from the web

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

dat <- read_csv(url)

download.file(url, "murders.csv")   # lets you download the file to have a local copy

tempdir()   # creates a directory with a name that is very unlikely not to be unique
tempfile()  # creates a file that is likely to be a unique file name

file.remove()   # removes a file


## 6) Tidy Data

library(dslabs)
data("gapminder")

tidy_data <- gapminder %>%
  filter(country %in% c("South Korea","Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

tidy_data %>% ggplot(aes(year, fertility, color=country)) + geom_point()

# Code works seemlessly because the data is tidy

# Each row represents one observation
# Each column represents the different variables that we have data on for those observations

# Original file, wide data

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

select(wide_data, country, '1960' : '1967')

# Wide format

# Each row contains several observations

# We need to wrangle the wide data into tidy format

# One of the variables is stored in the header


## 7) Reshaping Data

library(tidyverse)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

select(wide_data, country, '1960' : '1967')

gather()   # converts y data into tidy data, assumes column names are characters

# third argument specifies columns that will be gathered, default is all
# first argument sets the name of the column that will hold the variables
# that is currently kept in the wide data column name
# the second argument sets the column name for the column that will hold 
# the values in the column cells

new_tidy_data <- wide_data %>% 
  gather (year, fertility, '1960':'2015')

head(new_tidy_data)

# only columns not gathered is the countries one, so we can do that faster

new_tidy_data_fast <- wide_data %>% 
  gather (year, fertility, -country)

head(new_tidy_data_fast)

class(new_tidy_data$year)   # it becomes character from integer


new_tidy_data_fast <- wide_data %>% 
  gather (year, fertility, -country, convert=TRUE)  # converts to integer

class(new_tidy_data_fast$year)

new_tidy_data_fast %>% ggplot(aes(year, fertility, color=country)) + geom_point()

# sometimes it is useful to convert tidy into wide

spread()  # inverse of gather()

# first argument: which variable will be used as the column names
# second argument: which variable to use to fill out the cells

new_wide_data <- new_tidy_data_fast %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1967')


tidy_ex <- read_csv("experiment/Spread.csv")

dat_tidy <- tidy_ex %>% spread(key=var, value=people)
dat_tidy


## 8) Separate and Unite

library(tidyverse)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read.csv(filename)

select(raw_dat, 1:5)

dat <- raw_dat %>% gather(key, value, -country)  # no column name for key since it contains var type

head(dat)   # each observation is associated with two not one row

# We want the values in fertility and life expectancy as two separate columns

separate()

# argument 1: name of column to be separated
# argument 2: names to be used for the two columns
# argument 3: character that separates the variables

dat %>% separate(key, c("year", "variable_name"),"_")

# problem, since _ is used to separate life and expectancy

dat %>% separate(key, c("year", "first_variable_name","second_variable_name"),
                 fill="right")
# we need to merge the two columns

dat %>% separate(key, c("year", "variable_name"), sep="_", extra="merge")

dat %>% separate(key, c("year", "variable_name"), sep="_", extra="merge") %>%
  spread(variable_name, value)


separate_ex <- read_csv("experiment/separate.csv")

tidy_data <- separate_ex %>%
  gather(key = 'key', value = 'value', -age_group) %>%
  separate(col = key, into = c('year', 'variable_name'), sep = '_') %>% 
  spread(key = variable_name, value = value)


separate_unite_ex <- read_csv("experiment/separate_unite.csv")

tidy_data <- separate_unite_ex %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)

tidy_data <- separate_unite_ex %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), 
           sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)


## 9) Combining Tables



                 

