library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)


RawData <- read.csv("World Happiness Report.csv")

shinyServer(function(input, output, session){
    newVar <- reactive({
        #manipulate data
        Data <- RawData %>% select(-Overall.rank, -Country.or.region) %>% filter(Year == input$Year)
    })
    
#########################################Data Exploration#########################################
    output$title <- renderUI({
        text <- paste0("Investigation in year ",input$Year)
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
    

#########################################Principal Components Analysis#########################################
    #create PCA biplot
    output$BiPlot <- renderPlot({
        newData <- newVar()
        PCs <- prcomp(select(newData, input$Var) , scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(newData)), cex = 1.2)
        
    })
    
    #create scree plot
    output$ScreePlot <- renderPlot({
        newData <- newVar()
        PCs <- prcomp(select(newData, input$Var) , scale = TRUE)
        screeplot(PCs, type = "lines")
    })

    #create output of PCs    
    output$PCsValue <- renderPrint({
        newData <- newVar()
        PCs <- prcomp(select(newData, input$Var) , scale = TRUE)
        PCs
    }) 
    
    
    # Output PCA math using MathJax
    output$MathJax <- renderUI({
        withMathJax(
            helpText("Goal: Obtain a linear combination of the variables that accounts for the largest amount of variability (location not important for variance, assume 0 mean for each predictor)"),
            helpText("Find "),
            helpText("$$z_{i1}=\\phi_{11} x_{i1}+ \\phi_{21} x_{i2} + ......+ \\phi_{p1} x_{ip}$$"),
            helpText("values so that the set of z's has the largest variance."),
            helpText("Constraint: $$\\sum_{j=1}^{p}(\\phi^{2}_{j1})=1$$"),
            helpText("Once first PC found, now find next most variable linear combination."),
            helpText("Constraint: Must be uncorrelated with 1st PC. Process continues until min(n-1,p) PCs are found"),
            helpText(""),
            helpText("Solving for the PCs is equivalent to doing an eigenvalue decomposition on the covariance matrix!"),
            helpText("Eigenvectors represent the loadings phi's"),
            helpText("PCA is usually applied to the covariance matrix.
                     The covariance of two variables X and Y can be calculated using the following
                     formula: "),
            helpText('$$cov(X, Y) = \\frac{1}{n-1} \\sum_{i=1}^{n}(X_i-\\bar{x})(Y_i-\\bar{y})$$'),
            helpText("Eigenvalues represent how much of the variation exists on that PC; Largest eigenvalue (w/eigenvector) correspond to first PC")
        )
    })
    
    #Download PCA biplot
    PCAplot <- function(){
        newData <- newVar()
        PCs <- prcomp(select(newData, input$Var) , scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(newData)), cex = 1.2)
    }
    
    output$download_BiPlot <- downloadHandler(
        filename = "PCA_biplot.png",
        content = function(file) {
            png(file)
            PCAplot()
            dev.off()
        }
    )
    
    #Download scree plot
    PCAscreeplot <- function(){
        newData <- newVar()
        PCs <- prcomp(select(newData, input$Var) , scale = TRUE)
        screeplot(PCs, type = "lines")
    }
    
    output$download_ScreePlot <- downloadHandler(
        filename = "PCA_Scree_Plot.png",
        content = function(file) {
            png(file)
            PCAscreeplot()
            dev.off()
        }
    )

    
    
#########################################Modeling#########################################
    # Regression output
    output$summary <- renderPrint({
        newData <- newVar()
        fit <- lm(newData[,input$outcome] ~ newData[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    
    # Data output
    newVar2 <- reactive({
        Data <- RawData
    })
    
    output$tbl = DT::renderDataTable({
        #newData <- newVar()
        DT::datatable(newVar2(), options = list(lengthChange = FALSE))
    })
    
    #download datatable
    output$download_DataTable2 <- downloadHandler(
        filename = function(){paste("World Happiness Report dataset.csv")},
        content = function(file){write.csv(newVar2(), file, row.names = FALSE)}
    )
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        newData <- newVar()
        plot(newData[,input$indepvar], newData[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(newData[,input$outcome] ~ newData[,input$indepvar]), col="red")
        lines(lowess(newData[,input$indepvar],newData[,input$outcome]), col="blue")
    }, height=400)
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        newData <- newVar()
        hist(newData[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        newData <- newVar()
        hist(newData[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
#########################################Scroll through the Data#########################################
    #datatable
    newVar2 <- reactive({
        Data <- RawData
    })
    
    output$Data_table <- DT::renderDataTable({
        DT::datatable(newVar2())
    })
    
    #download datatable
    output$download_DataTable <- downloadHandler(
        filename = function(){paste("World Happiness Report dataset.csv")},
        content = function(file){write.csv(newVar2(), file, row.names = FALSE)}
    )
    
    
    
    
    
})