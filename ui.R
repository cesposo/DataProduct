library(shiny)
library(glmnet)


shinyUI(pageWithSidebar(
  headerPanel("Voting Prediction dashboard"), 

  
  sidebarPanel(
    checkboxGroupInput("model_type", "Checkbox", c("Logistic Regression" = "1", "CART" = "2", "Random Forest" = "3"))
    
    ),
  

  mainPanel(
      plotOutput('Plot1', height = "1080px", width = "720px")
    )
))