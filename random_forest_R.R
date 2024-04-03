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

# clean the work space
rm(list = ls(all = TRUE))


# import libraries
library(dplyr)
library(randomForest)

#view structure of airquality dataset
str(airquality)

# attach dataset
attach(airquality)
df = airquality

# sum complete cases
print(paste0('with complete cases: ', sum(complete.cases(df)) ) )

# sum incomplete cases
print(paste0('with incomplete cases: ', sum(!complete.cases(df)) ) )

# top 6
head(df)

#replace NAs with column medians
for(i in 1:ncol(df)) {
  # fill NA
  df[ , i][is.na(df[ , i])] <- median(df[ , i], na.rm=TRUE)
}

# top 6
head(df) 

cor_df=round( cor(df), 2)

library(reshape2)
melted_cor <- melt(cor_df)

par(mar=c(1,1,1,1))
library(ggplot2)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red", limit = c(-1, 1), name = "Correlation") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank()) +
  labs(title = "Correlation Heatmap")


# 5 number summaries of all columns
sapply(df[c(colnames(df))], fivenum)


# Split the data into training and test sets
set.seed(123)
train_index <- sample(1:ceiling(nrow(df)), size = ceiling( nrow(df)*0.7)  )
train <- df[train_index, ]
test <- df[-train_index, ]


#make this example reproducible
set.seed(1)

#fit the random forest model
model <- randomForest(formula = Ozone ~ ., data = train)

#display fitted model
model

#find number of trees that produce lowest test MSE
which.min(model$mse)
 
#find RMSE of best model
sqrt(model$mse[which.min(model$mse)]) 

#plot the test MSE by number of trees
plot(model)

#produce variable importance plot
varImpPlot(model) 

# tune the rf model
model_tuned <- tuneRF(
  x=train[,-1], #define predictor variables
  y=train$Ozone, #define response variable
  ntreeTry=500,
  mtryStart=4, 
  stepFactor=1.5,
  improve=0.01,
  trace=FALSE #don't show real-time progress
)


# Make predictions on the testing data
predictions <- predict(model, newdata = test)
 

#define new observation#definas.factor()e new observation
new <- data.frame(Solar.R=150, Wind=8, Temp=70, Month=5, Day=5)

#use fitted bagged model to predict Ozone value of new observation
predict(model, newdata=new)

 




 