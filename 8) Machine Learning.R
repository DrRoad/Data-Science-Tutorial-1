########   Machine Learning   #############


## 1) Notation

# Outcome we want to predict (output) & features we use to predict the outcome (input)

# Y - outcome     X_1 through X_p - features (predictors, covariates)

# Prediction outcomes: categorical Y can be any of K classes  k = 1,...,K, or continuous

# Y_hat to denote prediction
# actual outcome - what we ended up observing

# When outcome is categorical - classification   - right or wrong
 
# when outcome is continuous - prediction   - error

# First step: what are the outcomes, what are the features?


## 2) Caret package, training and test sets, and overall accuracy

library(caret)
library(dslabs)
library(tidyverse)

data("heights")

y <- heights$sex              # predictor
x <- heights$height           # outcome

# We split data into 2
# Training set - we know the outcomes we use it to develop algorithm
# Test Set - we pretend we do not know the outcomes

# Standard is to randomly split the data

# createDataPartition

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)  

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

# Since outcome categorical report proportion when outcome correctly predicted
# Referred to as overall accuracy

# Guessing the outcome

y_hat <- sample(c("Male","Female"), length(test_index),replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

# Since men are usually taller, we will predict male if height within 2 sd of avg male

heights %>% group_by(sex) %>%
  summarize(mean(height),sd(height))

y_hat <- ifelse(x > 62 , "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

mean(y == y_hat)

# We use different cutoffs and take the best one

cutoff <- seq(61,70)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

plot(cutoff,accuracy, type = 'l')

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Now we can use test_set

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


# exercise

mnist <- read_mnist()
y <- mnist$train$labels


## 3) Confusion Matrix

