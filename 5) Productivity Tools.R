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


## 15) Git and GitHub

# Git and Git-Hub is great for version control
# It tracks changes and you can revert back to an old version
# Git lets you create branches in which we can test out ideas then decide if we can merge
# the new branch with the original
# Also colaboration
# To share

# Files move around 4 different areas:
# Working Directory, Staging Area, Local Repository, Upstream Repository

# We can clone an existing Repo, or initialize a new one


## 16) Using Git and the Command Line

# We are going to clone the upstream repository github.com/rairizarry/murders

# Go to link, click clone, and copy repo URL

# Cloning: instead of downloading files to computer we are going to copy the entire 
# git structure i.e. we will add the files and directories to each of the 3 stages
# Working Directory, Staging Area, Local Repository

# In Git-Bash

# mkdir git-example
# cd git-example
# git-clone https://github.com/rairizarry/murders.git

# ~/Desktop/Data Science/Data-Science-Tutorial/murders/git-example  on my laptop

# When you change files using R-Studio you only edit in the working directory

# git status - can tell you how files relate to version

# We want to sync with upstream repo once we are sure they are final enough to share

# We only want edits that we think are important, not every small one

# Edits in the staging area are not kept by the version control system

# git add - adds file to staging area

# create new file
# echo "test" >> new-file.txt        creates a file with the word test in it
# echo "temporary" >> tmp.txt
# git add new-file.txt           which will state the file
# any stages we add to new-file.txt will get added to the repository next time we commit
# git commit -m "adding a new file" -  commits the file with that message
# We have now changed the local repository

# to show changes

# echo "adding a line" >> new-file.txt
# git add new-file.txt 
# git commit -m "adding a new file" 

# we don't have to do git add, we can go to git commit


# git log - shows all files

# git push - pushes all the files to the repository
# won't work with this example since it is rafael's repository and we don't have permission

# git fetch - to make our local repository like the upstream repository (in case of a collaborator)

# git merge - to make this copies to the staging and working directory

# git pull - to make it all in one shot, fetch and merge


## 17) Creating a GitHub Repository

# Initializing our own repository rather than cloning


# Create a project on your ow computer, independent of Git
# We do not have a Git local repo, or Git upstream repo

# Create a new repo on our GitHub page, this is the upstream Repo
# we will call it murders

# copy repo URL

# Go on Git-bash and go into the murders folder

# We need to make it a local Git directory

# git init - initializes

# Next we connect files to local repo, and connect the local repo to Git Hub repo

# create a text file

# git add README.txt - add a file 

# git commit -m "First commit"


# git remote add origin https://github.com/dwierschen/murders.git - to connect to upstream repo

# git push will push the files in the local repository to upstream


## 18) Advanced Unix Arguments

# Arguments - most Unix commands can be run with arguments

# - or -- followed by a letter or a word

# rm -r directory-name  removes files and directories recursively from the directory called
# Equivalent to throwing folder in the trash but it can not be recovered

# Sometimes files or directories are protected

# we use the -f argument to force removal

# rm -rf directory-name

# ls -a   -  show all files in a directory, including hidden

# In Unix all files starting with a . are hidden

# ls -l - show more information about file

# ls -t  - to show in chronological order, most recently modified on top

# ls -r  - reverser order 

# ls -lart  - combines all these


## 19) Getting help and pipes

# man command-name  - to get help on a command  (does not work on Git Bash)

# For Git Bash:    command --help

# Pipe  |

# ls --help | less   - shows less or more compact

# less command works with other things too

# ls -- help | head -6      - shows only the first 6 lines


## 20) Wildcard

# Imagine you want to remove all files ending in .HTML
# It would be painful to do 1 by 1

# uses *
# ls *.html     lists all files ending in html
# rm *.html     removes all HTML files

# ? means any character

# If we know all files are of the form file-001.html
# We can type rm file-???.html   to remove all files of the form file-???.html

# We can combine wildcards
# rm file-???.*


## 21) Environment Variables and Shells

# Unix has settings that affect your command line environment
# These are called environent variables

# Distinguished by $

# Home directory is stored in $HOME
# echo $HOME shows home directory

# env shows all environmental variables

# Most of these commands were Run on the Unix Shell
# echo $SHELL shows current shell

# export val value


## 22) Executables, Permissions, and File Types

# In Unix all programms are files
# Programms that run are called executables

# ls, mv, git, they are all executable files
# which git will show the directory in which these files reside

# ls /usr/git    will show executables

# echo $PATH  where UNIX looks for command executables

# You can create your own commands     ./my-ls to define it

# ls -l  . regular file: -    directory: d    executable: x

# To create something that works like a keyboard shortcut
# alias seetop=' ls -lt | head'   seetop would show top 10 in chronological order and with detail

# Careful when editing your .files, that can screw things up


## 23) Commands you should learn

# start filename  -  will try to open a file with the right application   open for Mac

# nano   - bare bones editor

# ln  - creates symbolic links

# tar lets you create of extract archives

# Ssh  - lets you conncect to other computers

# grep  - powerful command that lets you search for patterns in a file

# awk & sed  - two commands that permit you to find specific strings and files and change them


# You can perform file management within R

# use ?files














