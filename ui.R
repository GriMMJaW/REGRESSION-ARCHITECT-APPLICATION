#This file contains the code for the ui.R file of the Regression Architecture Application.

library(shiny)
library(rCharts)

shinyUI(fluidPage(
  pageWithSidebar(
    headerPanel("REGRESSION ARCHITECT"),
    sidebarPanel(
      fileInput(inputId="input_data",label="Upload a .csv file",accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
      radioButtons("regression_choice","Select Regression Technique:",c("Multiple Linear Regression"="glm","Linear Logistics Regression"="logit","Poisson Regression"="poisson")),
      selectInput("selection_criteria","Model Selection Criteria:",c("AIC"="AIC","BIC"="BIC")),
      textInput("y","Enter Response Variable (Column Name):"),
      actionButton("regression_modeller","Create Regression Model"),
      br(),
      br(),
      actionButton("makeplot", "Plot Response Variables"),
      br(),
      br(),
      actionButton("residualplots","Create Residual Plots"),
      br(),
      br(),
      br(),
      p("Documentation:",a("Regression Architect Documentation",href="Documentation.html"))
      ),
    mainPanel(
      h5("Sample of the uploaded dataset:"),
      verbatimTextOutput("head"),
      br(),
      h5("Your response variable name:"),
      textOutput("text1"),
      br(),
      h5("The best regression criteria according to the selected criteria is:"),
      verbatimTextOutput("regression_output"),
      br(),
      h5("The confidence intervals for the regression coefficients are:"),
      verbatimTextOutput("confidence_intervals"),
      br(),
      h5("Response Variable Plot:"),
      showOutput("regression_plot","morris"),
      h5("The residual plots are shown below:"),
      plotOutput("residual_plots") 
  )
))
)