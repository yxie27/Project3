# Project3
## Packages required:
``` r
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
```
## The code required to run the app:
``` r
shiny::runGitHub("Project3", "yxie27")
```
## In order to avoid the loss of "Scroll through the data" page (because Random Forest model takes a long time to operate), I put this page before the modeling page.
