
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plotly)
library(ggplot2)
library(randomForest)


cleanData = function(dt) {
    setnames(dt, names(dt), tolower(names(dt)))
    dt[, fare := removeOutliers(fare)]
    dt[, cabin := gsub("\\s+", "", substr(dt$cabin, 1, 1))]
    dt[, cabin:= as.factor(cabin)][, embarked := as.factor(embarked)][, sex := as.factor(sex)]
    dt[, passengerid := NULL][, name:= NULL][, ticket := NULL]
    dt = dt[complete.cases(dt),]
}

removeOutliers = function(x) {
    x[x %in% boxplot.stats(x)$out] = NA
    return(x)
}

percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

train = cleanData(fread("data/train.csv"))
train[, .outcome := as.factor(survived)][, survived := NULL]

model = randomForest(.outcome ~ ., data = train, importance = TRUE, proximity = TRUE)

sample = train[1]

shinyServer(function(input, output) {
    
    output$shiny_plot = renderPlot({
        
        variables = c("pclass", "sex", "age", "sibsp", "parch", "fare", "embarked", "cabin")
        values = sapply(variables, function(x) { input[[x]]})
        
        X = variables[match('x', values)]
        Y = variables[match('y', values)]
        X.facet = variables[match('x-facet', values)]
        Y.facet = variables[match('y-facet', values)]
        
        if (is.na(X) && is.na(Y)) {
            #nothing selected -> return
            return()
        } else if (is.na(X)) {
            #only Y selected - switch variables
            X = Y
        }
        # do charting selection
        if (is.na(Y)) {
            # a density plot
            plot = ggplot(data = train, aes_string(x=X, color=".outcome")) + 
                geom_density(binwidth=1, alpha =0.5, position="identity")
        } else {
            plot = ggplot(data = train, aes_string(x=X, y=Y, color=".outcome")) + 
                geom_point(shape = 1) 
        }
        #add facets, if needed
        if (!is.na(X.facet) && !is.na(Y.facet)) {
            plot = plot + facet_grid(reformulate(X.facet, Y.facet))
        } else if (!is.na(X.facet)) {
            plot = plot + facet_grid(reformulate(X.facet))
        } else if (!is.na(Y.facet)) {
            plot = plot + facet_grid(reformulate(Y.facet))
        }
        
        return(plot)
    })
    
    #predicting survival    
    output$shiny_survival = renderText({
        sample$pclass = input$predict_pclass
        sample$age = input$predict_age
        sample$sex = factor(input$predict_sex, levels(sample$sex))
        sample$sibsp = input$predict_sibsp
        sample$parch = input$predict_parch
        sample$fare = input$predict_fare
        sample$embarked = factor(input$predict_embarked, levels(sample$embarked))
        sample$cabin = factor(input$predict_cabin, levels(sample$cabin))
        percent(predict(model, sample, type="prob")[2])
    })
    
    #calculating accuracy of the model
    output$shiny_accuracy = renderText({
        percent(1 - 
                    (model$confusion[1,1] + model$confusion[2,2]) / 
                    (model$confusion[1,1] + model$confusion[1,2] + model$confusion[2,1] + model$confusion[2,2])
                )
    })
    
})
