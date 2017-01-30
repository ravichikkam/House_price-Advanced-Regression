# Importing training and testing data into R

Train = read.csv("train.csv")
Test = read.csv("test.csv")

# Identitying the important independent variables for linear regression

newcol = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54, 81)

Train2 = Train[,newcol]

Train2$SalePrice = log(Train2$SalePrice)

newcol1 = c(4,5,6,9,11,13,15,18,19,23,27,28,31,33,35,37,38,44,45,54)

Test2 = Test[,newcol1]

# Using mice function to impute data to fill NA's

library(mice)

temptrain = mice(Train2, seed = 123)
temptest = mice(Test2 , seed = 123)

Trainnew = complete(temptrain)
Testnew = complete(temptest)

Testnew$SalePrice = NULL

#Creating cross-validation sets

library(caTools)

set.seed(123)

splittrain = sample.split(Trainnew , SplitRatio = 0.75)

Train1 = subset(Trainnew , splittrain == T)
Test1 = subset(Trainnew , splittrain == F)

# Creating a XGboost model

library(xgboost)
library(Matrix)

# Creating a sparse matrix

Sparse_Train = sparse.model.matrix(SalePrice ~ . -1, data = Trainnew)
Sparse_Train1 = sparse.model.matrix(SalePrice ~ . -1, data = Train1)
Sparse_Test1 = sparse.model.matrix(SalePrice ~ . -1, data = Test1)
Sparse_Test = sparse.model.matrix( ~ . -1 , data = Testnew)

Label_Train = Train$SalePrice
Label_Train1 = Train1$SalePrice
Label_Test1 = Test1$SalePrice

# Creating xgb.DMatrix 

Xgmat_Train = xgb.DMatrix(data = Sparse_Train , label = Label_Train)
Xgmat_Train1 = xgb.DMatrix(data = Sparse_Train1 , label = Label_Train1)
Xgmat_Test1 = xgb.DMatrix(data = Sparse_Test1 , label = Label_Test1)
Xgmat_Test = xgb.DMatrix(data = Sparse_Test)

# Creating a simple Xgboost model without tunning parameters

model_house = xgboost(data = Xgmat_Train1 ,
                      nrounds = 1000 , objective = "reg:linear" , 
                      eval_metric = "rmse" , verbose = TRUE , eta = 0.1)


Test1_Predict = predict(final_model , newdata = Xgmat_Test1)

#To check RMSE value

RMSE = sqrt(sum((Test1$SalePrice - Test1_Predict)^2))

RMSE

# Setting up a grid to fine tune the parameters over cross vaildation

param = expand.grid(max_depth = c(10,11,12,13),
                    eta = c(0.005,0.01,0.05),
                    nrounds = c(300),
                    subsample = c(0.7,0.8,0.9),
                    min_child_weight = c(1,2),
                    colsample_bytree = c(0.8,0.9),
                    gamma = c(0,1,2))

# Xgboost cross-validation with folds = 5

df_new = data.frame()

for(i in 1: nrow(param)){
  
  cv = xgb.cv(params = param[i,] , 
              data = Xgmat_Train , objective = "reg:linear",
              nfold = 5 , eval_metric = "rmse" ,
              nrounds = param[i,3] , verbose = FALSE)
  
  df_new = rbind(df_new ,data.frame("Iter" = i , "Error" = mean(cv$evaluation_log[,test_rmse_mean])))
  
}

# To find the right parameters using the cross validation model

df_new[which(df_new$Error == min(df_new$Error)) ,]

final_param = param[df_new[which(df_new$Error == min(df_new$Error)) ,1],]

#   max_depth   eta nrounds subsample min_child_weight colsample_bytree gamma
#       11    0.005     150       0.7                2              0.8     0

# Creating the final model 

final_model = xgboost(data = Xgmat_Train , nrounds = 1000,
                      objective = "reg:linear" , max_depth = 13,
                      eta = 0.05 , subsample = 0.8,
                      min_child_weight = 2 , colsample_bytree = 0.9,
                      gamma = 2 , eval_metric = "rmse",
                      verbose = T)



# RMSE = 50530.58

# Using the model to make predictions on the test set

Test_Predict = predict(final_model , newdata = Xgmat_Test)

Testnew$Id = Test$Id

Testnew$SalePrice = Test_Predict

Final = Testnew[,c(21,22)]
