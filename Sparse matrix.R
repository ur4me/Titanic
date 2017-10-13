library(plyr)
library(dplyr)
library(mice)
library(xgboost)
library(caret)
library(randomForest)
library(Matrix) 


setwd('c:/Kaggle/titanic')

#Combine train.csv and test.csv files
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
total <- bind_rows(train, test)


#make Title column
total$Title <- gsub('(.*, )|(\\..*)', '', total$Name)
total$Title[total$Title == 'Mlle'] <- 'Miss'
total$Title[total$Title == 'Ms'] <- 'Miss'
total$Title[total$Title == 'Mme'] <- 'Mrs'


table(total$Sex, total$Title)

#make family size column that contains the passenger themselves
total$FamilySize <- total$SibSp + total$Parch + 1
#make Deck column
total$Deck <- factor(sapply(total$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


total1 <- total[,-c(1,2,4,9,11)]
total1 <- as.data.frame(unclass(total1))

miceMod <- mice(total1, method="rf")
miceOutput <- complete(miceMod)
#Check whether there is missing values
colnames(miceOutput)[colSums(is.na(miceOutput)) > 0]

miceOutput$Pclass <- as.factor(miceOutput$Pclass)

#separate

train1 <- miceOutput[1:891,]
test1 <- miceOutput[892:1309,]
#add SalePrice column to train1
Survived <- train$Survived
train1 <- cbind(train1,Survived)



#xgboost
#split train
set.seed(54321)
outcome <- train1$Survived

partition <- createDataPartition(y=outcome,
                                 p=.7,
                                 list=F)
training <- train1[partition,]
testing <- train1[-partition,]

testing1 <- testing[, -11]

#sparse
sparse_matrix <- sparse.model.matrix(Survived~.-1, data = training)
output_vector <- training[,'Survived']
dtrain <- xgb.DMatrix(sparse_matrix,label = output_vector)
sparse_matrix1 <- sparse.model.matrix(~.-1, data = testing1)
dtest <- xgb.DMatrix(sparse_matrix1)



#tuning
train.control <- trainControl(method = "repeatedcv", repeats = 2,number = 3, search = "grid")

tune.grid <- expand.grid(nrounds = c(100,150),
                         max_depth = c(5,6,7),
                         eta = c(0.05, 0.10, 0.2),
                         gamma = c(0.0, 0.2),
                         colsample_bytree = c(0.5,0.7,1),
                         min_child_weight= c(5,7), 
                         subsample =c(0.5,0.7,1))



caret.cv <-caret::train(Survived ~.,
                        data=training,
                        method="xgbTree",
                        metric = "auc",
                        tuneGrid=tune.grid,
                        trControl=train.control)


#xgboost parameters
xgb_params <- list(colsample_bytree = 0.5, #variables per tree 
                   subsample = 0.7, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 7, #tree levels
                   eta = 0.1, #shrinkage
                   eval_metric = "auc", 
                   objective = "binary:logistic",
                   min_child_weight = 5,
                   gamma=0)

#cross-validation and checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 4, print_every_n = 5, nrounds=1000, nthread=6)

#check with confusionmatrix
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   verbose = 1, maximize =F,
                   nrounds = 10, nthread=6)

prediction <- predict(gb_dt, dtest)
prediction <- ifelse(prediction > 0.5, 1 , 0)
confusionMatrix(prediction, testing$Survived)


#predict with real test data
sparse_matrix <- sparse.model.matrix(Survived~.-1, data = train1)
output_vector <- train1[,'Survived']
dtrain <- xgb.DMatrix(sparse_matrix,label = output_vector)
sparse_matrix1 <- sparse.model.matrix(~.-1, data = test1)
dtest <- xgb.DMatrix(sparse_matrix1)


#xgboost parameters
xgb_params <- list(colsample_bytree = 0.5, #variables per tree 
                   subsample = 0.7, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 7, #tree levels
                   eta = 0.05, #shrinkage
                   eval_metric = "auc", 
                   objective = "binary:logistic",
                   min_child_weight = 5,
                   gamma=0)

#cross-validation and checking iterations
set.seed(4321)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 4, print_every_n = 5, nrounds=1000, nthread=6)


#prediction
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   verbose = 1, maximize =F,
                   nrounds = 30, nthread=6)

prediction <- predict(gb_dt, dtest)
prediction <- ifelse(prediction > 0.5, 1 , 0)


solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'sparse1.csv', row.names = F)
