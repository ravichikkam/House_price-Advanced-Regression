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

# Creating a Linear Regression model

model_house = lm(log(SalePrice) ~ . , data = Trainnew)

# model accuracy check

summary(model_house)

# R2 = 0.8976 , R2 adj = 0.8926

Test_predict = predict(model_house , newdata = Testnew)


Testnew$Id = Test$Id
Testnew$SalePrice = Test_predict

Final = Testnew[,c(21,22)]
