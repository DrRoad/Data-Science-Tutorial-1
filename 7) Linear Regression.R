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

