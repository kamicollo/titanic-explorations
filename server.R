
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(caret)
library(rpart)


train = fread("data/train.csv")
train[, .outcome := as.factor(Survived)][, Survived := NULL]
setnames(train, names(train), tolower(names(train)))

test = fread("data/test.csv")
setnames(test, names(test), tolower(names(test)))

shinyServer(function(input, output) {
    
    #fitControl = trainControl(method = "none", number = 1)
    fitControl = trainControl(number = 5)
    
    model = train(.outcome ~ ., data = train, model = input$algorithm, trControl = fitControl, family ="binomial")
    output$shiny_algorithm = renderText({model$results$Accuracy[1]})
    output$shiny_threshold = renderText({input$threshold})
    output$shiny_cvtechnique = renderText({input$cvtech})
    output$shiny_features = renderText({paste(input$features, collapse = " ")})
})
