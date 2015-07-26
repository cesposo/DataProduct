library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(RTextTools)
library(randomForest)
library(flexclust)
library(ggplot2)
library(maps)
library(ggmap)
library(reshape2)
library(shiny)

setwd("C:/Users/Blah/Dropbox/Coursers/EdX/The Analytics Edge/Data/Week 8")
polling = read.csv("PollingImputed.csv")

# Statemap initliziation 
statesMap = map_data("state")

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")


shinyUI(
  navbarPage("Political dashboard analytics", 
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Explore the Data",
                      sidebarPanel(
                        sliderInput("timeline", 
                                    "Timeline:", 
                                    min = 1950,
                                    max = 2015,
                                    value = c(1996, 2015)),
                        sliderInput("pieces", 
                                    "Number of Pieces:",
                                    min = -1,
                                    max = 5922,
                                    value = c(271, 2448) 
                        ),
                        #format = "####"),
                        uiOutput("themesControl"), # the id
                        actionButton(inputId = "clearAll", 
                                     label = "Clear selection", 
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAll", 
                                     label = "Select all", 
                                     icon = icon("check-square-o"))
                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Data 
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable")
                          ), # end of "Dataset" tab panel
                          tabPanel(p(icon("line-chart"), "Visualize the Data"), 
                                   h4('Number of Themes by Year', align = "center"),
                                   showOutput("themesByYear", "nvd3"),
                                   h4('Number of Pieces by Year', align = "center"),
                                   h5('Please hover over each point to see the Set Name and ID.', 
                                      align ="center"),
                                   showOutput("piecesByYear", "nvd3"),
                                   h4('Number of Average Pieces by Year', align = "center"),
                                   showOutput("piecesByYearAvg", "nvd3"),
                                   h4('Number of Average Pieces by Theme', align = "center"),
                                   showOutput("piecesByThemeAvg", "nvd3")
                          ) # end of "Visualize the Data" tab panel
                          
                        )
                        
                      )     
             ), # end of "Explore Dataset" tab panel
             
             tabPanel(p(icon("search"), "LookUp on Brickset Website"),
                      mainPanel(
                        h4("The page popped-up is the LEGO set database on Brickset.com."),
                        h4("Step 1. Please type the Set ID below and press the 'Go!' button:"),
                        textInput(inputId="setid", label = "Input Set ID"),
                        #p('Output Set ID:'),
                        #textOutput('setid'),
                        actionButton("goButtonAdd", "Go!"),
                        h5('Output Address:'),
                        textOutput("address"),
                        p(""),
                        h4("Step 2. Please click the button below. 
                           The link to the Set's page is being generated."),
                        p(""),
                        actionButton("goButtonDirect", "Generate Link Below!"),
                        p(""),
                        htmlOutput("inc"),
                        p("I was supposed to show you in an iframe below. However, it only
                          worked on localhost and has security issue after deployed to the cloud. Ooops...")
                        
                        )         
                      ),
             
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) 
             )
  
  )