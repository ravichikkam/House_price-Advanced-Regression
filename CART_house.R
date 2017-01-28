# Importing training and testing data into R

Train = read.csv("train.csv")
Test = read.csv("test.csv")

# Identitying the important independent variables for linear regression

newcol = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54, 81)

Train2 = Train[,newcol]

newcol1 = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54)

Test2 = Test[,newcol1]

# Using mice function to impute data to fill NA's

library(mice)

temptrain = mice(Train2, seed = 123)
temptest = mice(Test2 , seed = 123)

Trainnew = complete(temptrain)
Testnew = complete(temptest)

#Creating cross-validation sets

library(caTools)

set.seed(123)

splittrain = sample.split(Trainnew , SplitRatio = 0.75)

Train1 = subset(Trainnew , splittrain == T)
Test1 = subset(Trainnew , splittrain == F)

# Creating a Regression Tree model

library(rpart)
library(rpart.plot)

model_house = rpart(SalePrice ~ . , data = Train1 , minbucket = 10)

prp(model_house)

# model accuracy check

Test1_predict = predict(model_house , newdata = Test1)

Test1$Predict = Test1_predict

RMSE = sqrt(sum((Test1$SalePrice - Test1$Predict)^2))

RMSE

# MinBucket = 20 , RMSE = 984542.7
# MinBucket = 10 , RMSE = 959849.3
# MinBucket = 5 , RMSE = 964964.9

# Testnew$Id = Test$Id
# Testnew$SalePrice = Test_predict
# 
# Final = Testnew[,c(21,22)]
