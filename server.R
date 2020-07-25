library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)

Data <- read.csv("World Happiness Report.csv")

shinyServer(function(input, output, session){
    newVar <- reactive({
        #manipulate data
        Data <- Data %>% select(-Overall.rank, -Country.or.region) %>% filter(Year == input$Year)
    })
    
    output$title <- renderUI({
        text <- paste0("Investigation in year",input$Year)
        h1(text)
    })
    
    #create plot
    output$Plot <- renderPlot({
        newData <- newVar()
        g <- ggplot(newData, aes(x = GDP.per.capita, y=Score))
        g + geom_point(size = input$size)
        
    })
    
    #create text info
    output$info <- renderText({
        newData <- newVar()
        paste0("The average GDP in ", input$Year, " is ", round(mean(newData$GDP.per.capita),2),". ",
        "The average social support in ", input$Year, " is ", round(mean(newData$Social.support),2),". ",
        "The average healthy life expectancy in ", input$Year, " is ", round(mean(newData$Healthy.life.expectancy),2),". ",
        "The average freedom to make life choices in ", input$Year, " is ", round(mean(newData$Freedom.to.make.life.choices),2),". ",
        "The average generosity in ", input$Year, " is ", round(mean(newData$Generosity),2),". ",
        "The average Perceptions of corruption in ", input$Year, " is ", round(mean(newData$Perceptions.of.corruption),2),". ",
        "Finally, the average world happiness score in ", input$Year, " is ", round(mean(newData$Score),2),"."
        )
        })
    
    #create output of observations    
    output$table <- renderTable({
        newData <- newVar()
        newData
    })
    
    #create PCA biplot
    output$BiPlot <- renderPlot({
        PCs <- prcomp(select(Data, GDP.per.capita, Social.support) , scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(Data)), cex = 1.2)
    })
    
    # Download PCA biplot
    output$download_BiPlot <- downloadHandler(
        filename = "PCA.png",
        content = function(file) {
            png(file)
            PCAplot()
            dev.off()
        }
    )
    
    
    
    
    
    
    
    
    
})