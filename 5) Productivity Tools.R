### Productivity Tools


## 1) R and R-Studio

installed.packages()    # shows all installed packages

library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

# In Global options uncheck Restore .RData into workspace at startup, and pick never for Save sorkspace to .RData

# Keep your file system organized, to increase productivity

# File -> New Project -> New Directory -> New Project   decide location
# Use lower case letters, no spaces, and hyphens to separate words
# Generates a .Rproj file
# Top right shows you to which project this RStudio session belongs to
# When working on a project, all files will be saved and searched for in the folder associated with the project


## 2) Git

# Git is a version control system
# Useful for tracking changes to files and coordinating the editing of code by collaborators

# For windows we need to install Git and Git bash   https://git-scm.com/download/win
# Select Nano as default editor for Git -> and Git and optional Unix Tools
# Global options -> Open new terminal with Git


## 3) Git-Hub

# Create a Git-Hub account
# Connect R to Git-Hub  Global Options -> Git-SVN -> directory where git is installed -> bin
# git.exe, create SSH RSA key


## 4) Git-Hub Repositories

# Two copies of code, one on computer, one on Git-Hub

# Create new repository

# Clone or download link


## 5) R-Studio, Git, and Git-Hub

# open a new terminal
# git config --global user.name "your name"
# git config --global user.email "your@mail.com"

# start nw project -> version control

# not updated automatically.  Will upload when you are ready

# click on check box on Git tab to upload -> commit -> write text -> push

# git-scm.com/docs/gitignore


## 6) Unix

# The terminal is our window to the Linux world
# Using it to organize files in our system without using mouse

# On windows : Gitbash

# on GitBash
# echo "Hello World" will type back hello world.


## 7) The Filesystem

# Think of it as nested folders containing files, folders and executables

# folders-subfolders  in UNIX:  directories-subdirectories
# Working directory - current location

# pwd - prints full path of the working directory

# ~ shortname of home directory

# mkdir projects  - creates directory project
# cd projects - moves into that directory    cd - change directory


## 8) Unix Commands

# Unix autocompletes by hitting tab
# pwd - prints full path of the working directory
# ls - list directory content
# mkdir projects  - makes directory called projects,  mkdir projects  teaching - creates both
# rmdir projects - removes directory called projects only if it is empty
# cd projects - moves into that directory    cd - change directory

# cd / or ~ is a full path
# cd projects is a relative path - it will look for the directory in the current working directory

# cd .. - go back to parent directory
# cd ~ - will bring us back to the home directory from anywhere

# cd ~/projects - equivalent to typing the full path out
# cd ../.. to move to the parent directory of the parent directory
# cd -  - moves you back to the directory you just left
# cd - will return you to home directory


## 9) Moving and removing files

# mv move command will not ask to overwrite files, it will simply do it
# mv path_to_the_file path_to_the_destination_directory
# mv also renames i.e.  mv cv.tex resume.tex     renames cv to resume
# mv and renaming:  mv cv.tex ../reports/resume.tex

# We can also move entire directories

# mv ~/docs/resumes ~/docs/reports/    (last slash is important so that it doesn't think you want to rename)


# cp copy commann, copies file instead of moving
# we can not copy entire directories without learning about arguments


# rm - deletes (it is permanent, no warning or questions)
# to remove directories we need to learn about arguments


## 10) less: looking at a file

# To look at a text file, move to the directory it is using cd
# less cv.tex  -  which opens the less viewer
# to exit less viewer type q


## 11) Preparing for Data Science Project

# mkdir projects
# cs projects
# mkdir murders
# cd murders 
# mkdir data rda     - rda a folder for r data

# if you reopen Git-Bash cd projects/murder

# in R under New Projects you can use existing directory and type full path

getwd()  # shows working directory in R

# In R always use relative paths, if you use full paths it might not work on other systems

# Code will not be used in above directory for simplicity

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

dest_file <- "murders/data/murders.csv"    # we used a relative path

download.file(url, destfile=dest_file)


library(tidyverse)

murders <- read_csv("murders/data/murders.csv")

murders <- murders %>% mutate(region=factor(region), rate= total/population*10^5)

save(murders, file = "murders/rda/murders.rda")   # saves into R data file

load("murders/rda/murders.rda")

murders %>% mutate(abb=reorder(abb,rate)) %>%
  ggplot(aes(abb, rate)) +
  geom_bar(width = 0.5, stat="identity", color="black") + coord_flip()

# to be organized we create new directory called figs   mkdir figs from the parent dir murders

ggsave("murders/figs/barplot.png")

# As a data scientist it is important to create a README.txt text file describing what each file does  


## 12) Reproducible Reports with R Markdown

# The final product on an analysis project is the report

# Using R Markdown and knitr, code and text can be combined in the same document
# Figues and tables are automatically added

# Great for if you make a mistake, get other data etc.


## 13) R Markdown

# It is a format for literate programming documents

# more info on www.markdowntutorial.com   https://rmarkdown.rstudio.com/

# knitr compiles R Markdown codes

# R Markdown document looks different than final project

# File -> New File -> R Markdown -> Title and author, then choose HTML for debugging
# Type can be changed later

# Rmd suffix for these files

# Changing output on top to pdf_document we can make to pdf

# CTRL+ALT+I will automatically create empty code chunk

# Whatever code you do in the position of the R chunk, that is where it will appear in final

# writing code, and then using argument echo=false will not show code in presentation

# To add labels ```{r pressure-summary, echo=FALSE}


## 14) knitr

# Website: https://yihui.name/knitr/

# Used to convert R Markdown to document

# R Studo provides button in R Markdown knit

# output: github_document  that renders on GitHub, with suffix .md


## 15) 











