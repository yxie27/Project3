library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "red",
                    #add title                  
                    dashboardHeader(
                        title = "Students' Academic Performance Dataset (xAPI-Edu-Data)",
                        titleWidth = 750),
                    
                    #define sidebar items
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem(tabName = "info", "Information", icon = icon("dashboard")),
                            menuItem(tabName = "data", "Data Exploration", icon = icon("table")),
                            menuItem(tabName = "unsuper", "Unsupervised Learning", icon = icon("archive")),
                            menuItem(tabName = "model", "Modeling", icon = icon("laptop")),
                            menuItem(tabName = "subdata", "Scroll through the Data", icon = icon("th"))
                        )),
                    
                    #define the body of the app
                    dashboardBody(
                        tabItems(
                            #Information tab
                            tabItem(tabName = "info",
                                    fluidRow(
                                        #add in latex functionality if needed
                                        withMathJax(),
                                        
                                        #two columns for each of the two items
                                        column(6,
                                               #description of data
                                               h1("Data Description"),
                                               #box to contain the description
                                               box(background = "red", width = 12,
                                                   h3("This is an educational data set which is collected from learning management system (LMS) called Kalboard 360."), 
                                                   h3("Kalboard 360 is a multi-agent LMS, which has been designed to facilitate learning through the use of leading-edge technology."), 
                                                   h3("Such system provides users with a synchronous access to educational resources from any device with Internet connection.")
                                                   
                                               )
                                               
                                               
                                        ),
                                        
                                        column(6, 
                                               #ability of the APP
                                               h1("Ability of the APP"),
                                               #box to contain the ability
                                               box(background = "red", width = 12,
                                                   h3("The tabs across the top of the applet allow for navigation between the marginal order statistic distribution visualization, the joint order statistic distribution visualization, and the theory underlying order statistics."),
                                                   h3("The controls for the visualization sections of the applet are located to the left and the visualizations are available on the right.")
                                               )
                                        )
                                    )),
                            
                            #Data Exploration tab
                            tabItem(tabName = "data",
                                    
                            ),
                            
                            #Unsupervised Learning tab
                            tabItem(tabName = "unsuper"),
                            
                            #Modeling tab
                            tabItem(tabName = "model")
                            
                        )
                    ))
