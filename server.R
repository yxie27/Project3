library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(tree)
library(randomForest)

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
    newVar2 <- reactive({
        Data <- RawData %>% select(-Overall.rank, -Country.or.region,-Year)
    })
    
    #create PCA biplot
    output$BiPlot <- renderPlot({
        newData2 <- newVar2()
        PCs <- prcomp(select(newData2, input$Var) , scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(newData2)), cex = 1.2)
        
    })
    
    #create scree plot
    output$ScreePlot <- renderPlot({
        newData2 <- newVar2()
        PCs <- prcomp(select(newData2, input$Var) , scale = TRUE)
        screeplot(PCs, type = "lines")
    })

    #create output of PCs    
    output$PCsValue <- renderPrint({
        newData2 <- newVar2()
        PCs <- prcomp(select(newData2, input$Var) , scale = TRUE)
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
            helpText("PCA is usually applied to the covariance matrix.The covariance of two variables X and Y can be calculated using the following formula: "),
            helpText('$$cov(X, Y) = \\frac{1}{n-1} \\sum_{i=1}^{n}(X_i-\\bar{x})(Y_i-\\bar{y})$$'),
            helpText("Eigenvalues represent how much of the variation exists on that PC; Largest eigenvalue (w/eigenvector) correspond to first PC")
        )
    })
    
    #Download PCA biplot
    PCAplot <- function(){
        newData2 <- newVar2()
        PCs <- prcomp(select(newData2, input$Var) , scale = TRUE)
        biplot(PCs, xlabs = rep(".", nrow(newData2)), cex = 1.2)
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
        newData2 <- newVar2()
        PCs <- prcomp(select(newData2, input$Var) , scale = TRUE)
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
        newData2 <- newVar2()
        fit <- lm(newData2[,input$outcome] ~ newData2[,input$indepvar])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(newVar2(), options = list(lengthChange = FALSE))
    })
    
    
    output$download_DataTable2 <- downloadHandler(
        filename = function(){paste("World Happiness Report dataset.csv")},
        content = function(file){write.csv(newVar2(), file, row.names = FALSE)}
    )
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        newData2 <- newVar2()
        plot(newData2[,input$indepvar], newData2[,input$outcome], main="Scatterplot",
             xlab=input$indepvar, ylab=input$outcome, pch=19)
        abline(lm(newData2[,input$outcome] ~ newData2[,input$indepvar]), col="red")
        lines(lowess(newData2[,input$indepvar],newData2[,input$outcome]), col="blue")
    }, height=400)
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        newData2 <- newVar2()
        hist(newData2[,input$outcome], main="", xlab=input$outcome)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        newData2 <- newVar2()
        hist(newData2[,input$indepvar], main="", xlab=input$indepvar)
    }, height=300, width=300)
    
    
    #Regression Tree
    output$Regre_tree <- renderPlot({
        newData2 <- newVar2()
        #obtain training and test sets
        set.seed(123)
        train <- sample(1:nrow(newData2), size = nrow(newData2)*0.8)
        test <- dplyr::setdiff(1:nrow(newData2),train)
        newData2Train <- newData2[train, ]
        newData2Test <- newData2[test, ]
        
        treeFit <- tree(Score ~ ., data=newData2Train)
        plot(treeFit)
        text(treeFit)
    })
    
    output$CVtree <- renderPrint({
        newData2 <- newVar2()
        #obtain training and test sets
        set.seed(123)
        train <- sample(1:nrow(newData2), size = nrow(newData2)*0.8)
        test <- dplyr::setdiff(1:nrow(newData2),train)
        newData2Train <- newData2[train, ]
        newData2Test <- newData2[test, ]
        
        treeFit <- tree(Score ~ ., data=newData2Train)
        
        cvTree <- cv.tree(treeFit)
        cvTree
    })
    
    #prediction1
    output$prediction1 <- renderPrint({
        newData2 <- newVar2()
        #obtain training and test sets
        set.seed(123)
        train <- sample(1:nrow(newData2), size = nrow(newData2)*0.8)
        test <- dplyr::setdiff(1:nrow(newData2),train)
        newData2Train <- newData2[train, ]
        newData2Test <- newData2[test, ]
        treeFit <- tree(Score ~ ., data=newData2Train)
        
        pred <- predict(treeFit, newdata=dplyr::select(newData2Test, -Score))
        sqrt(mean((pred-newData2Test$Score)^2))
    })
    
    
    #Random Forests modeling
    output$RF_tree <- renderPrint({
    newData2 <- newVar2()
    set.seed(123)
    train <- sample(1:nrow(newData2), size = nrow(newData2)*0.8)
    test <- dplyr::setdiff(1:nrow(newData2),train)
    newData2Train <- newData2[train, ]
    newData2Test <- newData2[test, ]
    rfFit <- randomForest(Score ~ ., data=newData2Train, mtry=ncol(newData2Train)/3, ntree = input$ntree, importance=TRUE)
    rfFit
    
    })
    
    
    #prediction2
    output$prediction2 <- renderPrint({
        newData2 <- newVar2()
        set.seed(123)
        train <- sample(1:nrow(newData2), size = nrow(newData2)*0.8)
        test <- dplyr::setdiff(1:nrow(newData2),train)
        newData2Train <- newData2[train, ]
        newData2Test <- newData2[test, ]
        rfFit <- randomForest(Score ~ ., data=newData2Train, mtry=ncol(newData2Train)/3, ntree = input$ntree, importance=TRUE)
        
        rfPred <- predict(rfFit, newdata = dplyr::select(newData2Test,-Score))
        sqrt(mean((rfPred-newData2Test$Score)^2))
    })
    
    
    
#########################################Scroll through the Data#########################################
    #datatable
    output$Data_table <- DT::renderDataTable({
        DT::datatable(newVar2())
    })
    
    #download datatable
    output$download_DataTable <- downloadHandler(
        filename = function(){paste("World Happiness Report dataset.csv")},
        content = function(file){write.csv(newVar2(), file, row.names = FALSE)}
    )
    
    
    
    
    
})