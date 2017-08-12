# Using RandomForest

## Table of contents

- [Preparation](#preparation)
- [Prediction](#prediction)


## Preparation
```
#Load packages
library(plyr)
library(dplyr)
library(mice)
library(xgboost)
library(randomForest)
library(hydroGOF)
```
```
#Set up Working directory
setwd('c:/Kaggle/titanic') #Location where I saved train.csv and test.csv files
```

We need to Combine train.csv and test.csv files in order to fill up the missing values. I will change brank cells to NAs as it is easier to fill up the missing values. 
```
#Combine train.csv and test.csv files
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
total <- bind_rows(train, test)
```
#### Adding new column
It would be really good if I can make as many new variables as possible. However, in order to use tree function, factor predictors must have at most 32 levels. In other words, actual people's name and ticket numbers will not play important role in the prediction as they have more than 32 levels. But the title which is stated in their name will be really meaningful as it will have less than 32 levels. Furthermore, I can retrieve deck names from Cabin Column. Lastly, I can make family size column from SibSp and Parch column.

```
#make Title column
total$Title <- gsub('(.*, )|(\\..*)', '', total$Name)
total$Title[total$Title == 'Mlle'] <- 'Miss'
total$Title[total$Title == 'Ms'] <- 'Miss'
total$Title[total$Title == 'Mme'] <- 'Mrs'
```
```
#check title counts by sex
table(total$Sex, total$Title)
Capt Col Don Dona  Dr Jonkheer Lady Major Master Miss  Mr Mrs Rev Sir
female    0   0   0    1   1        0    1     0      0  264   0 198   0   0
male      1   4   1    0   7        1    0     2     61    0 757   0   8   1

the Countess
female            1
male              0
```

The levels are less than 32 so it seems like I can use this information to tree function. Now, I am going to make Family size column and Deck column.

```
#make family size column that contains the passenger themselves
total$FamilySize <- total$SibSp + total$Parch + 1
#make Deck column
total$Deck <- factor(sapply(total$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
```

I will remove some columns that seems to be not useful.
```
#remove PassengerId, Survived, Name,Ticket and Cabin columns
total1 <- total[, -c(1,2,4,9,11)]
```

#### Replace missing values
I will use Mice to replace missing values
```
total1 <- as.data.frame(unclass(total1))
miceMod <- mice(total1, method="rf")
miceOutput <- complete(miceMod)
#Check whether there is missing values
colnames(miceOutput)[colSums(is.na(miceOutput)) > 0]
```

## Prediction
```
#Split in to training and test sets
train <- miceOutput[1:891,]
test <- miceOutput[892:1309,]
```

#### Outlier handling
```
#add Survived column to train
train <- cbind(train, total[1:891,"Survived"])
colnames(train)[11] <- "Survived"
train <- as.data.frame(unclass(train))
```
```
#change to numeric
indx <- sapply(train, is.factor)
train[indx] <- lapply(train[indx], function(x) as.numeric(as.factor(x)))
indx <- sapply(test, is.factor)
test[indx] <- lapply(test[indx], function(x) as.numeric(as.factor(x)))
```

```
#Check Outliers
mod <- lm(Survived ~ ., data=train)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

as.numeric(names(cooksd)[(cooksd > 15*mean(cooksd, na.rm=T))])
```

![Alt text](https://github.com/ur4me/Titanic/blob/master/Outliers.png)

I will remove 648, 680, 738 rows
```
train <- train[-c(648,680,738),]
```


#### Prediction
```
#predict with Random Forest
train$Survived <- as.factor(train$Survived)
model_1 <- randomForest(Survived ~ ., data=train)
prediction <- predict(model_1, test)
solution <- data.frame(PassengerId = total[892:1309,"PassengerId"], Survived = prediction)
write.csv(solution, file = 'random_forest_Sol_Titanic.csv', row.names = F)
```
