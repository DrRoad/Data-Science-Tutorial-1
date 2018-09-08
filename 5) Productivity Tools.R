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


## 8) Unic Commands



