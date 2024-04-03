##################################################################
# Goal: Use iris.uci to build a decision tree
# Problem: classification
##################################################################


# At the root, split the data by petal length. 
# If petal length > 2.45 cm then follow the right branch down to the next node.
# At the next node, the feature 
# that is used to split the data is sepal length. Since the sepal length of the new
# flower is greater than 5.0 cm, you would follow the right branch down to the leaf 
# node. The leaf node that you reach is labeled “versicolor”, so the predicted class 
# for the new flower is versicolor.


# import libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(rattle)
 
library(RColorBrewer)

# plot mytree
fancyRpartPlot(mytree, caption = NULL)

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

fancyRpartPlot(tree, caption = NULL)

 