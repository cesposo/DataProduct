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
library(glmnet)
library(shiny)

#Loading up the data

statesMap = map_data("state")
polling = read.csv("C:/Users/Blah/Dropbox/Coursers/Coursera Classes/Developing Data Products/Project/PollingImputed.csv")

trainP = subset(polling, Year %in% c(2004, 2008))
testP = subset(polling, Year == 2012)

#----------------------------------------------------------------------------------------
# Logistic Regression model 

model1 <- glm(Republican~SurveyUSA+DiffCount, data=trainP, family="binomial")
TestPrediction <- predict(model1, newdata = testP, type="response")
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, testP$State)

predictionDataFrame$region <- tolower(predictionDataFrame$testP.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

#-----------------------------------------------------------------------------------------
#CART model

model2 <- rpart(Republican~SurveyUSA+DiffCount, data=trainP)
TestPredirpart <- predict(model2, newdata = testP)
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, testP$State)


predictionDataFrame$region <- tolower(predictionDataFrame$testP.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

#-----------------------------------------------------------------------------------------
#Random Forest

model3 <- randomForest(Republican~SurveyUSA+DiffCount, data=trainP)
TestPrediction <- predict(model3, newdata = testP)
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)
predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, testP$State)


predictionDataFrame$region <- tolower(predictionDataFrame$testP.State)
predictionMap <- merge(statesMap, predictionDataFrame, by = "region")
predictionMap <- predictionMap[order(predictionMap$order),]

#-----------------------------------------------------------------------------------------
shinyServer(
  function(input, output)
  {
#    model_select <- reactive(
#    {
#    if (input$model_type == '1')
#    {
      
      output$Plot1 <- renderPlot({ 
        
      ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
      p1 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
      p2 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",  alpha = .3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
      print(p1)
      print(p2)
      }, height = 700)
  
#    }
#    else if (input$model_type == '2')
#    {
      
      output$Plot2 <- renderPlot({
        
      ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
      p1 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
      p2 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",  alpha = .3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
      print(p1)
      print(p2)
      }, height = 700)
    
      
#    }
#    else if (input$model_type == '3')
#    {
      
      output$Plot3 <- renderPlot({
        
      ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
      p1 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
      p2 <- ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black",  alpha = .3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
      print(p1)
      print(p2)
      }, height = 700)
  
 
#    }
      

#    }
#    )
      
      
      
    })

