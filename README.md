# **Titanic**
*Predict survival on the Titanic*

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)



## Introduction

This is my first project that is uploaded on github so there should be much better way to predict the survivors.
As for this time, I will use RandomForest to make Machine Learning model. 


## Preparation

#### Initial works
```
#Load packages
library(mice)
library(dplyr)
library(randomForest)
```
```
#Set up Working directory
setwd('c:/Kaggle') #Location where I saved train.csv and test.csv files
```

We need to Combine train.csv and test.csv files in order to fill up the missing values. I will change brank cells to NAs as it is easier to fill up the missing values. 
```
#Combine train.csv and test.csv files
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
total <- bind_rows(train, test)
```

#### Adding new column
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

Now, I am going to make Family size column and Deck column.

```
#make family size column that contains the passenger themselves
total$FamilySize <- total$SibSp + total$Parch + 1
#make Deck column
total$Deck <- factor(sapply(total$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
```

#### Exploratory analysis
I will used Tableau for visualise our data.
First of all, I will see the relationship between sex and survived.

![Alt text](https://github.com/ur4me/Titanic/blob/master/Relationship%20between%20Sex%20and%20Survived.PNG)

As we expected, the proportion of survived female was much higher than that of male.

What does our family size variable look like? To help us understand how it may relate to survival, I will also use Tableau and plot it.

![Alt text](https://github.com/ur4me/Titanic/blob/master/Family%20Size.PNG)

We can see that there’s a survival penalty to singletons and those with family sizes above 4. We can collapse this variable into three levels which will be helpful since there are comparatively fewer large families. 

#### Filling up missing values
Before imputation, I will remove some variables and change categorical to factors.
```
total1 <- total[,-c(1,2,4,9,11)]
total1 <- as.data.frame(unclass(total1))
```

I will impute missing values by using Mice package.

```
#imputing missing values
miceMod <- mice(total1, method="rf")
miceOutput <- complete(miceMod)
#Check whether there is missing values
colnames(miceOutput)[colSums(is.na(miceOutput)) > 0]
```

#### Separation
I will separate back to train and test for prediction.

```
#separate
train1 <- miceOutput[1:891,]
test1 <- miceOutput[892:1309,]
```

#### Outlier handling

I will use multivariate method to check outliers.

```
#add Survived column to train1
Survived <- train$Survived
train1 <- cbind(train1,Survived)
```
```
#outlier handling
mod <- lm(Survived ~ ., data=train1)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
```

![Alt text](https://github.com/ur4me/Titanic/blob/master/Rplot.png)


I will remove outlier rows.
```
train1 <- train1[-which(cooksd >0.1),]
```

## Prediction

I will use simple Random Forest model to predict the test set.

```
#predict with Random Forest
model_1 <- randomForest(Survived ~ ., data=train1)
prediction <- predict(model_1, test1)
prediction <- ifelse(prediction > 0.5, 1 , 0)
solution <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'random_forest_Sol2.csv', row.names = F)
```


