library(datasets)
data(Titanic)
head(as.data.frame(Titanic),5)

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Titanic Survival Calculator"),
  sidebarPanel(
    p("Select person attributes to calculate his/her chances of surviving of the titanic sinking."),
    selectInput("c", label =h3("Crew/Passenger:"), list("1st Class Passenger" = "1st","2nd Class Passenger" = "2nd", "3rd Class Passenger" = "3rd", "Crew Member" = "Crew")),
    radioButtons("s", label = h3("Sex:"),
                 choices = list("Male" = "Male", "Female" = "Female"), 
                 selected = "Female"),
    radioButtons("a", label = h3("Age:"),
                 choices = list("Child" = "Child", "Adult" = "Adult"),
                 selected = "Adult")),
  mainPanel(
    h3("Survival Probability:"),
    h4(textOutput('prob')),
    p("Please note that this is estimated probability based on a logistic regression model."),
    p("That means this value is slightly different than historical survival rate."))))
