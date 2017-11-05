library(gbm)
library(plyr)
library(dplyr)
library(caret)
library(shiny)


#Combine train.csv and test.csv files
train1 <- read.csv("./data/cleaned_titanic.csv", na.strings = c("", "NA"), stringsAsFactors = T)
train1$Pclass <- as.factor(train1$Pclass)


modelFit <- train(Survived ~ ., data  = train1, method  = "gbm")

shinyServer(function(input, output){
  
  userdf <- train1[11,]
  values <- reactiveValues()
  values$df <- userdf
  newEntry <- observe({
    values$df$Pclass <- as.factor(input$Pclass)
    values$df$Sex <- as.factor(input$Sex)
    values$df$Age <- as.numeric(input$Age)
    values$df$SibSp <- as.integer(input$SibSp)
    values$df$Parch <- as.integer(input$Parch)
    values$df$Fare <- as.numeric(input$Fare)
    values$df$Embarked <- as.factor(input$Embarked)
    values$df$Title <- as.factor(input$Title)
    values$df$FamilySize <- as.integer(input$FamilySize)
    values$df$Deck <- as.factor(input$Deck)
  })
  output$results <- renderPrint({
    ds1 <- values$df
    a <- predict(modelFit, newdata = data.frame(ds1))
    names(a) <- NULL
    cat(a)
  })
})