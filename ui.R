library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(tree)
#library(randomForest)
library(caret)
library(httr)
library(plotly)
library(RCurl)



#Read in data from my Github url address
url1<-'https://raw.githubusercontent.com/yxie27/Project3/master/World%20Happiness%20Report.csv'
GET(url1, write_disk(tf <- tempfile(fileext = ".csv")))
Data <- read.csv(tf)
Data2 <- Data %>% select(-Overall.rank, -Country.or.region,-Year)
#Data <- read.csv("World Happiness Report.csv")



ui <- dashboardPage(
  skin = "red",
  #add title                  
  dashboardHeader(
    title = "World Happiness Report 2018-2019",
    titleWidth = 750),
  
  #define sidebar items
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "info", "Information", icon = icon("dashboard")),
      menuItem(tabName = "data", "Data Exploration", icon = icon("table")),
      menuItem(tabName = "pca", "Principal Components Analysis", icon = icon("archive")),
      menuItem(tabName = "subdata", "Scroll through the Data", icon = icon("th")),
      menuItem(tabName = "model", "Modeling", icon = icon("laptop"))
    )),
  
  #define the body of the app
  dashboardBody(
    tabItems(
      #Information tab
      tabItem(tabName = "info",
              fluidRow(
                #two columns for each of the two items
                column(6,
                       #description of data
                       h1("Data Description"),
                       #box to contain the description
                       box(background = "red", width = 12,
                           em(h4(strong("The World Happiness Report")," is a point of interest survey of the state of worldwide bliss. ")), 
                           h4("The report proceeds to pick up worldwide acknowledgment as governments, organizations and respectful society progressively utilize joy pointers to educate their policy-making choices. 
                                                      Driving specialists over areas – financial matters, brain research, overview investigation, national insights, wellbeing, open approach and more – depict how estimations of well-being can be used effectively to evaluate the advance of countries. 
                                                      The reports survey the state of bliss within the world nowadays and appear how the modern science of bliss clarifies individual and national varieties in bliss."), 
                           h4("This file contains the Happiness Score for 153 countries along with the factors used to explain the score."),
                           h4("The Happiness Score is a national average of the responses to the main life evaluation question asked in the Gallup World Poll (GWP), which uses the Cantril Ladder."),
                           h4("The Happiness Score is explained by the following factors:"),
                           em(h5("- GDP per capita")),
                           em(h5("- Social support")),
                           em(h5("- Healthy Life Expectancy")),
                           em(h5("- Freedom to make life choices")),
                           em(h5("- Generosity")),
                           em(h5("- Perceptions of corruption")),
                           helpText(a(strong("Click here to get more information"), href = "https://www.kaggle.com/mathurinache/world-happiness-report/data"))
                       )
                       
                       
                ),
                
                column(6, 
                       #ability of the APP
                       h1("Ability of the APP"),
                       #box to contain the ability
                       box(background = "red", width = 12,
                           h4("The tabs across the top of the applet allow for navigation between the marginal order statistic distribution visualization, the joint order statistic distribution visualization, and the theory underlying order statistics."),
                           h4("The controls for the visualization sections of the applet are located to the left and the visualizations are available on the right."),
                           h4("Within this app, you can:"),
                           em(h5("- Explore the common numeric and graphical summaries of the data")),
                           em(h5("- Apply Principal Components Analysis (PCA)")),
                           em(h5("- Choose model for certain variables in the dataset and make prediction")),
                           em(h5("- Scroll through the data"))
                       )
                )
              )
      ),
      
      #Data Exploration tab
      tabItem(tabName = "data",
              fluidPage(
                #title
                titlePanel(
                  uiOutput("title")
                ),
                # Sidebar with options for the data set
                sidebarLayout(
                  sidebarPanel(
                    h3("Select the Year:"),
                    selectizeInput("Year", "Year", selected = 2018, choices = levels(as.factor(Data$Year))),
                    br(),
                    sliderInput("size", "Size of Points on Graph",
                                min = 1, max = 10, value = 5, step = 1),
                    #DT::dataTableOutput('dataByYear'),
                    downloadButton("download_DataTable3", "Download the Dataset by Selected Year")
                    
                  ),
                  # Show output
                  mainPanel(
                    h3("The ggplot of World Happiness Score V.S. GDP : "),
                    plotOutput("Plot",click = "plot_click"),
                    downloadButton("download_ggPlot", "Save image"),
                    h3("Click on plot to get the value of x and y: "),
                    verbatimTextOutput("clicktext"),
                    #plotlyOutput("Plot2"),
                    h3("The numeric summaries: "),
                    textOutput("info"),
                    #tableOutput("table")
                    DT::dataTableOutput('dataByYear')
                  )
                )
                
                
              )
              
              
      ),
      
      #Unsupervised Learning tab
      tabItem(tabName = "pca",
              fluidPage(
                #title
                headerPanel(h1("Principal Components Analysis (PCA)")),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Introduction", h4("Principal Components Analysis (PCA) is a dimension reduction technique."),
                             h4("If you have variables, they contain some joint variability/correlation, 
                                                                       PCA looks for linear combination of those variables that account for most of the variability.")
                    ),
                    tabPanel("Algorithm", uiOutput('MathJax')),
                    tabPanel("Biplot & Scree Plot & PCs Values",
                             sidebarLayout(
                               sidebarPanel(
                                 checkboxGroupInput("Var", "Please select variables for Principal Component Analysis:", choices = names(Data2),selected = list("GDP.per.capita","Social.support"))
                               ),
                               mainPanel(
                                 h3("The Biplot for the Selected Variables:"),
                                 plotOutput("BiPlot"),
                                 downloadButton("download_BiPlot", "Save image"),
                                 h3("The Scree Plot for the Selected Variables:"),
                                 plotOutput("Scree_Plot"),
                                 downloadButton("download_ScreePlot", "Save image"),
                                 h3("The PCs Values for the Selected Variables:"),
                                 verbatimTextOutput("PCsValue")
                               )
                             )
                    )
                    
                    
                  )
                  
                )
              )
      ),
      
      
      #Scroll through the Data tab
      tabItem(tabName = "subdata",
              mainPanel(
                h3("In order to avoid the loss of this data page (because Random Forest model takes a long time to operate), I put the data page before the modeling."),
                DT::dataTableOutput('Data_table'),
                downloadButton("download_DataTable", "Download the Dataset")
              )
              
              
              
      ),
      
      
      #Modeling tab
      tabItem(tabName = "model",
              tabsetPanel(
                tabPanel("Linear Regression Model",
                         fluidPage(
                           titlePanel("Regression Model"),
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("outcome", label = h3("Outcome"),
                                           choices = list("Score","GDP.per.capita","Social.support","Healthy.life.expectancy","Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption"), selected = 1),
                               
                               selectInput("indepvar", label = h3("Explanatory variable"),
                                           choices = list("Score","GDP.per.capita","Social.support","Healthy.life.expectancy","Freedom.to.make.life.choices","Generosity","Perceptions.of.corruption"), selected = 1)
                               
                             ),
                             
                             mainPanel(
                               
                               tabsetPanel(type = "tabs",
                                           
                                           tabPanel("Scatterplot", 
                                                    plotOutput("scatterplot"),
                                                    downloadButton("download_scatterplot", "Save image")
                                                    ), 
                                           tabPanel("Distribution", # Plots of distributions
                                                    fluidRow(
                                                      column(6, plotOutput("distribution1"),downloadButton("download_histogram1", "Save image")),
                                                      column(6, plotOutput("distribution2"),downloadButton("download_histogram2", "Save image")))
                                           ),
                                           tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                                           tabPanel("Data", DT::dataTableOutput('tbl'),downloadButton("download_DataTable2", "Download the Dataset")) # Data as datatable
                                           
                               )
                             )
                           ))
                ),
                
                
                
                tabPanel("Regression Tree Model",
                         fluidPage(
                           titlePanel("Regression Tree Model"),
                           sidebarLayout(
                             sidebarPanel(
                                 sliderInput("pred", "Select the Values of the GDP", min = 0, max = 3, value = 0.001, step = 0.001)
                               
                               
                             ),
                             mainPanel(
                             plotOutput("Regre_tree"),
                             #verbatimTextOutput("CVtree"),
                             h3("The value of root MSE about the prediction is: "),
                             verbatimTextOutput("prediction1"),
                             h3("The Predicted World Happiness Score is:"),
                             verbatimTextOutput("prediction1_GDP")
                           )
                           )
                           
                         )
                ),
                
                
                
                tabPanel("Random Forests Model",
                         fluidPage(
                           titlePanel("Random Forests Model"),
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("numberTree","Select the Number of Trees: ", min = 1, max = 12, value = 6, step = 1),
                               sliderInput("pred2", "Select the Values of the GDP", min = 0, max = 3, value = 0.001, step = 0.001)
                             ),
                             mainPanel(
                               h3("The value of root MSE about the prediction is (Slow...Please wait): "),
                               verbatimTextOutput("prediction2"),
                               h3("The Results of Random Forest Model (Slow...Please wait): "),
                               verbatimTextOutput("rf_output"),
                               h3("The Misclassificatoon Rate is (Slow...Please wait): "),
                               verbatimTextOutput("rf_output2"),
                               h3("The Predicted World Happiness Score is (Slow...Please wait):"),
                               verbatimTextOutput("prediction2_GDP")
                             )
                           )
                         )
                )
                
                
                
                
              ))
      

      
      
      
      
      
    )
  )
  

)