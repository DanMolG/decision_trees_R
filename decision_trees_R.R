##################################################################
# Goal: Use iris.uci to build a decision tree
# Problem: classification
##################################################################

# import libraries
library(rpart)
library(rpart.plot)
library(dplyr)

# attach or read in dataset
attach(iris)

# attach dataset
df = iris

# clean the work space
rm(list = ls(all = TRUE))

# mean
df %>%
  group_by(Species) %>%
  summarise(mean(Sepal.Length),
            mean(Sepal.Width),
            mean(Petal.Length),
            mean(Petal.Width)
            )

# Split the data into training and test sets
set.seed(123)
train_index <- sample(1:nrow(iris), size = 0.7 * nrow(iris))
train <- iris[train_index, ]
test <- iris[-train_index, ]

# Train a decision tree model
tree <- rpart(Species ~ ., data = train, method = "class")         

# Plot the decision tree 
par(mar=c(1,1,1,1))
rpart.plot(tree, main = "Decision Tree for the Iris Dataset")



 