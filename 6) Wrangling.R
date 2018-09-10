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


## 3) 




