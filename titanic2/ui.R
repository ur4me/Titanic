library(mice)
library(dplyr)
library(randomForest)
library(caret)
setwd('c:/Kaggle/titanic')
#Combine train.csv and test.csv files
train <- read.csv('train.csv', na.strings = c("", "NA"), stringsAsFactors = F)
test <- read.csv('test.csv', na.strings = c("", "NA"), stringsAsFactors = F)
total <- bind_rows(train, test)

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Titanic Survival Calculator"),
  sidebarPanel(
    p("Select person attributes to calculate his/her chances of surviving of the titanic sinking."),
    sliderInput(
      inputId = "Pclass",
      label = h3("Ticket class:"),
      min = 1,
      max = 3,
      step = 1,
      value = 2),
    radioButtons("Sex", label = h3("Sex:"),
                 choices = list("male" = "male", "female" = "female"), 
                 selected = "female"),
    sliderInput(
      inputId = "Age",
      label = h3("Age:"),
      min = 0.0,
      max = 100,
      step = 0.5,
      value = 17),
    sliderInput(
      inputId = "SibSp",
      label = h3("Number of Siblings/spouses:"),
      min = 0.0,
      max = 9,
      step = 1,
      value = 1),
    sliderInput(
      inputId = "Parch",
      label = h3("Number of Parents/Children:"),
      min = 0.0,
      max = 7,
      step = 1,
      value = 1),
    sliderInput(
      inputId = "Fare",
      label = h3("Passenger Fare:"),
      min = 0.0,
      max = 550,
      step = 0.5,
      value = 20),
    radioButtons("Embarked", label = h3("Port of Embarkation:"),
                 choices = list("C" = "C", "Q" = "Q", "S" = "S"), 
                 selected = "Q"),
    radioButtons("Title", label = h3("Title:"),
                 choices = list("Mr" = "Mr", "Miss" = "Miss", "Mrs" = "Mrs","Master" = "Master", "Rare Title" = "Rare Title"), 
                 selected = "Mr"),
    sliderInput(
      inputId = "FamilySize",
      label = h3("Family Size:"),
      min = 0.0,
      max = 12,
      step = 1,
      value = 2),
    radioButtons("Deck", label = h3("Deck:"),
                 choices = list("C" = "C", "F" = "F", "E" = "E","B" = "B", "D" = "D", "A" = "A","G" = "G","T" = "T"), 
                 selected = "C")
  ),
  mainPanel(
    h3("Survival Probability:"),
    h4(textOutput('prob')),
    p("Please note that this is estimated probability based on a logistic regression model."),
    p("That means this value is slightly different than historical survival rate."))))
