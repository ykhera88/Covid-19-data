#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
shinyUI(
    fluidPage(
        useShinyjs(),
        titlePanel("Data423: Assignment 2-Yuvraj Khera 14217806"),
        
        h3("Covid-19 data"),
        tabsetPanel(
            tabPanel("Raw data table",
                     DT::dataTableOutput(outputId = "Assignment2rawdatatable")
            ),
            tabPanel("raw data summary",
                     htmlOutput(outputId = "summaryassignment2rawdata")
            ),
            tabPanel("Data table",
                     DT::dataTableOutput(outputId = "Assignment2datatable")
            ),         
            
            tabPanel("Summary",
                     htmlOutput(outputId = "summaryassignment2data")
                     
            ),
            tabPanel("Missing Data Vis Miss",
                     plotOutput(outputId ="Missing_overall"),
                     checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE)
            ),
            tabPanel("Missing Data Visdat",
                     plotOutput(outputId ="Missing")
            ),
            tabPanel("Missing Pattern",
                     plotOutput(outputId ="Missingpattern")  
                     
            ),
            tabPanel("Missing Patern Corrogram",
                     plotOutput(outputId ="missingcorrgram"),
                     selectInput(inputId = "CorrMeth", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "spearman")
            ),
            
            tabPanel("Remove variables",
                     sliderInput(inputId = "VarThresh",label ="Variable missingness threshold", min=0, max=100, value = 50, step=1),
                     textOutput(outputId = "variablesremove")
            ),
            
            tabPanel("Variable importance",
                     plotOutput(outputId ="rf"),
                     verbatimTextOutput(outputId ="variableimportance")
            ),
            tabPanel("Remove variables Final",
                     sliderInput(inputId = "VarThreshfinal",label ="Variable missingness threshold", min=0, max=100, value = 50, step=1),
                     textOutput(outputId = "variablesremovefinal")
            ),
            tabPanel("Remove observations",
                     sliderInput(inputId = "obsThresh",label ="Observation missingness threshold", min=0, max=100, value = 50, step=1),
                     textOutput(outputId = "observationremove")
                     
            ),
            tabPanel("Remove observations Final",
                     sliderInput(inputId = "obsThreshfinal",label ="Observation missingness threshold", min=0, max=100, value = 50, step=1),
                     textOutput(outputId = "observationremovefinal")
            ),
            
            tabPanel("Summary pre imputation",
                     htmlOutput(outputId = "summary")
            ), 
            tabPanel("Missing Data pre imputation",
                     plotOutput(outputId ="Missing_overall2"),
                     checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE)
            ),
            
            tabPanel("RPart Plot",
                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "treeplot"))
            ),
            
            tabPanel("GGpairs",
                     selectizeInput(inputId = "select", label = "Show variables:", choices = assignment2colnames, multiple = TRUE, selected = assignment2colnames[2:5]),    
                     shinycssloaders::withSpinner(
                         plotOutput(outputId = "Pairs"))
            ), 
            tabPanel("Box Plot", 
                     selectizeInput(inputId ="select2",label = "Show variables:",choices = assignment2numericalcolnames,multiple = FALSE, selected = assignment2numericalcolnames),
                     plotOutput(outputId = "Boxplot"),
                     checkboxInput(inputId = "standardise", label = "Show standardized", value = TRUE),
                     checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                     sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
                     
            ),
            
            
        
            
            tabPanel(" Train Data table",
                     shinycssloaders::withSpinner(
                     DT::dataTableOutput(outputId = "Assignment2TRAIN"))
                     
            ),
            
            tabPanel(" Test Data table",
                     shinycssloaders::withSpinner(
                     DT::dataTableOutput(outputId = "Assignment2TEST"))
             ),
            
            tabPanel("Prediction Values",
                     shinycssloaders::withSpinner(
              verbatimTextOutput(outputId = "Assignment2RECIPE"))
            ),
            tabPanel("TEST RMSE",
                     shinycssloaders::withSpinner(
                         verbatimTextOutput(outputId = "RMSE"))
            ),
             tabPanel("Preditced vs actual plot",
                      shinycssloaders::withSpinner(
                     plotOutput(outputId = "plot1"))
            ),
        
             
              tabPanel("Residualplot",
                       shinycssloaders::withSpinner(
                 plotOutput(outputId = "residualplot")),
                 sliderInput(inputId = "range_residual", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
             )
    )
)
)
        
    














    










