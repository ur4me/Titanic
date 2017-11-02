library(mice)
library(dplyr)
library(randomForest)
library(caret)
library(shiny)
setwd('c:/Kaggle/titanic')
#Combine train.csv and test.csv files
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
total <- bind_rows(train, test)


# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
#make Title column
total$Title <- gsub('(.*, )|(\\..*)', '', total$Name)
total$Title[total$Title == 'Mlle'] <- 'Miss'
total$Title[total$Title == 'Ms'] <- 'Miss'
total$Title[total$Title == 'Mme'] <- 'Mrs'
total$Title[total$Title %in% rare_title]  <- 'Rare Title'
#make family size column that contains the passenger themselves
total$FamilySize <- total$SibSp + total$Parch + 1
#make Deck column
total$Deck <- factor(sapply(total$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

#missing value imputation
total1 <- total[,-c(1,2,4,9,11)]
total1 <- as.data.frame(unclass(total1))

#imputing missing values
miceMod <- mice(total1, method="rf")
miceOutput <- complete(miceMod)

#separate
train1 <- miceOutput[1:891,]
test1 <- miceOutput[892:1309,]

#add Survived column to train1
Survived <- train$Survived
train1 <- cbind(train1,Survived)
train1$Survived <- as.factor(train1$Survived)

#predict with Random Forest
model_1 <- randomForest(Survived ~ ., data=train1)

predict(model_1, test1)


pred_tit <- function(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Title, FamilySize, Deck){
  inputdata <- c(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked, Title, FamilySize, Deck)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "FamilySize", "Deck")
  surv_prob <- predict(model_1,pred_data , type = "response" )
  return(surv_prob)
}

shinyServer(
  function(input, output) {
    output$prob <- renderText({pred_tit(input$Pclass,input$Sex, input$Age, input$SibSp, input$Parch, input$Fare, input$Embarked, input$Title, input$FamilySize, input$Deck )})
  })
