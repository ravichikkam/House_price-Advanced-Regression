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

# Creating a Regression forest Tree model

library(randomForest)

model_house = randomForest(SalePrice ~ . , data = Train1 , nodesize = 5 , ntree = 1000)

# model accuracy check

Test1_predict = predict(model_house , newdata = Test1)

Test1$Predict = Test1_predict

RMSE = sqrt(sum((Test1$SalePrice - Test1$Predict)^2))

RMSE

#nzise = 25, ntree = 200 , RMSE = 689240.3
#nsize = 20, ntree = 500 , RMSE = 673123.5
#nsize = 10, ntree = 750, RMSE = 642207.2
#nsize = 5, ntree = 1000, RMSE = 640750.9

Test_predict = predict(model_house , newdata = Testnew)

Testnew$Id = Test$Id
Testnew$SalePrice = Test_predict

Final = Testnew[,c(21,22)]
