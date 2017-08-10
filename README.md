# **Titanic**
*Predict survival on the Titanic*

## Table of contents

- [Introduction](#introduction)
- [Preparation](#preparation)
- [Prediction](#prediction)
- [Conclusion](#conclusion)


## [Introduction](-introduction)

This is my first project that is uploaded on github so there should be much better way to predict the survivors.
As for this time, I will use tree function to make Machine Learning model. In order to use tree function, it is really important to know the types of variables so that I can change the types to appropriate ones. 


## Preparation

#### Initial works
```
#Load packages
library(tree)
library(dplyr)
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

#### Filling up missing values
I will say this is the most important and time consuming step. To begin with, let's check which columns contain missing values.
```
#find names of columns which contain missing values
colnames(total)[colSums(is.na(total)) > 0]

[1] "Survived" "Age"      "Fare"     "Cabin"    "Embarked" "Deck" 
```
I don't need to fill up the missing values in Cabin column as not only it contains more than 32 levels, but also I already retrieved more meaningful values to Deck column. As I will predict the survivors in the final step, I should fill up the missing values. I will use tree function to expect the missing values. In order to use tree function, Char need to be changed to Factor.
```
#Change Char to Factor
total <- as.data.frame(unclass(total))
```
```
#Check the structure of total
str(total)
'data.frame':	1309 obs. of  15 variables:
 $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
 $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
 $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
 $ Name       : Factor w/ 1307 levels "Abbing, Mr. Anthony",..: 156 287 531 430 23 826 775 922 613 855 ...
 $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
 $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
 $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
 $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
 $ Ticket     : Factor w/ 929 levels "110152","110413",..: 721 817 915 66 650 374 110 542 478 175 ...
 $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
 $ Cabin      : Factor w/ 186 levels "A10","A11","A14",..: NA 107 NA 71 NA NA 164 NA NA NA ...
 $ Embarked   : Factor w/ 3 levels "C","Q","S": 3 1 3 3 3 2 3 3 3 1 ...
 $ Title      : Factor w/ 15 levels "Capt","Col","Don",..: 11 12 10 12 11 11 11 9 12 12 ...
 $ FamilySize : num  2 2 1 2 1 1 1 5 3 2 ...
 $ Deck       : Factor w/ 8 levels "A","B","C","D",..: NA 3 NA 3 NA NA 5 NA NA NA ...
```
```
#Get rows that contain NAs in Age column
missing_age <- total[which(is.na(total$Age)),]

#make tree model (Do not choose PassengerId column, Survived column and the column with more than 32 levels. 
model <- tree(formula = Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + Deck, data = total)

#predict the NAs with the model
prediction <- predict(object = model, newdata = missing_age)

#replace predicted value to NAs
total$Age[is.na(total$Age)] <- prediction
```

I will use very similar way to fill up the missing values for other columns 

```
#Get rows that contain NAs in Fare column
missing_fare <- total[which(is.na(total$Fare)),]

#make tree model 
model <- tree(formula = Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + FamilySize + Deck, data = total)

#predict the NAs with the model
prediction <- predict(object = model, newdata = missing_fare)

#replace predicted value to NAs
total$Fare[is.na(total$Fare)] <- prediction

#Get rows that contain NAs in Embarked column
missing_embarked <- total[which(is.na(total$Embarked)),]

#make tree model 
model <- tree(formula = Embarked ~ Pclass + Sex + Age + SibSp + Parch + Fare + Title + FamilySize + Deck, data = total)

#predict the NAs with the model
prediction <- predict(object = model, newdata = missing_embarked)

#replace predicted value to NAs
total$Embarked[is.na(total$Embarked)] <- prediction
```

I got warning message.
```
Warning messages:
1: In `[<-.factor`(`*tmp*`, is.na(total$Embarked), value = c(3L, 1L,  :
  invalid factor level, NA generated
2: In x[...] <- m :
  number of items to replace is not a multiple of replacement length
```

To solve the issue, I need to go back and change the prediction formula.
```
#Change the formula
prediction <- predict(object = model, newdata = missing_embarked, type = "class")

#rerun the replacement command
total$Embarked[is.na(total$Embarked)] <- prediction

#Get rows that contain NAs in Deck column
missing_deck <- total[which(is.na(total$Deck)),]

#make tree model 
model <- tree(formula = Deck ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = total)

#predict the NAs with the model
prediction <- predict(object = model, newdata = missing_deck, type = "class")

#replace predicted value to NAs
total$Deck[is.na(total$Deck)] <- prediction
```

## Prediction
```
#Split in to training and test sets
train <- total[1:891,]
test <- total[892:1309,]
```
#### Prediction
Before the prediction, I need to change Survived column type to factor
```
#Change certain column to factor
train$Survived <-as.factor(train$Survived)
```

Now, let's make the model and predict test data set
```
#set up model
model <- tree(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + Deck, data = train)

#Prediction
prediction <- predict(object = model, newdata = test, type = "class")
```
Finally, save the work
```
#make a dataframe with two columns
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

#save a file
write.csv(solution, file = 'my_second_Solution.csv', row.names=F)
```

## Conclusion
I got 79% accuracy for this one which seems to be not that bad for the first outcome. From next project I need to think about outliers and better ML model to solve problems.
