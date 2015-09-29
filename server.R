
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(plotly)
library(ggplot2)


train = fread("data/train.csv")
train[, .outcome := as.factor(Survived)][, Survived := NULL]
setnames(train, names(train), tolower(names(train)))

test = fread("data/test.csv")
setnames(test, names(test), tolower(names(test)))

removeOutliers = function(x) {
    x[x %in% boxplot.stats(x)$out] = NA
    return(x)
}

train[, fare := removeOutliers(fare)]

shinyServer(function(input, output) {
    
        output$shiny_plot = renderPlot({
            if (input$chart == "density") {
                ggplot(data = train, aes_string(x=input$features, color=".outcome")) + 
                    geom_density(binwidth=1, alpha =0.5, position="identity")
            } else if (input$chart == "scatter") {
                ggplot(data = train, aes_string(x=input$features, y=input$featuresy, color=".outcome")) + 
                    geom_point(shape = 1)
            } else if (input$chart == "facet") {
                ggplot(data = train, aes_string(x=input$features, y=input$featuresy, color=".outcome")) + 
                    geom_point(shape = 1) + facet_grid(sex ~ pclass)
            }
        })    
    
    

    
})
