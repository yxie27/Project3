library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)

EduData <- read.csv("/Users/yilinxie/Desktop/ST558/Project/Project3/Project3/EduData.csv")

shinyServer(function(input, output, session){
    newVar <- reactive({
        #read in data
        
        EduData <- EduData %>% select(Topic, gender, NationalITy, PlaceofBirth, raisedhands, Class) %>% filter(Topic == input$Topic)
    })
    
    output$title <- renderUI({
        text <- paste0("Investigation of ",input$Topic, " Topic")
        h1(text)
    })
    
    #create plot
    output$Plot <- renderPlot({
        newData <- newVar()
        g <- ggplot(newData, aes(x = Class, y=raisedhands,fill=Class))
        g + geom_boxplot( ) + scale_x_discrete(limits=c("H", "M", "L")) + geom_dotplot(binaxis='y', stackdir='center', dotsize = input$size) +
            scale_fill_discrete(name="Total Grade",breaks=c("H","M","L"),labels=c("High-Level(90-100)", "Middle-Level(70-89)", "Low-Level(0-69)"))
        
    })
    
    #create text info
    output$info <- renderText({
        newData <- newVar()
        newDataH <- newData %>% filter(Class == "H")
        newDataM <- newData %>% filter(Class == "M")
        newDataL <- newData %>% filter(Class == "L")
        paste0("The average hands raise for ", input$Toic, "topic for High-Level grade is ", round(mean(newDataH$raisedhands),2),". ",
        "The average hands raise for ", input$Toic, "topic for Middle-Level grade is ", round(mean(newDataM$raisedhands),2),". ",
        "The average hands raise for ", input$Toic, "topic for Low-Level grade is ", round(mean(newDataL$raisedhands),2),".")
        })
    
    #create output of observations    
    output$table <- renderTable({
        newData <- newVar()
        newData
    })
})