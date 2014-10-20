#The below code is the server file for the app Regression Architect

##Libraries Used
library(shiny)
library(rCharts)
library(ggplot2)
library(reshape2)
library(plyr)

##The main Program
shinyServer(
  function(input, output) {
    #This chunck of code reads the uploaded .csv dataset
    my_data <- reactive({
      input_file <- input$input_data
      if (is.null(input_file)) return(NULL)
      data <- read.csv(input_file$datapath,header=T)
      data
      })
    
    #This chunck of code displays first 3 rows of the uploaded data set.
    output$head <- renderPrint({
      if(class(my_data())=="data.frame") head(my_data(),n=3)
      else "Dataset hasn't been uploaded yet"
                               })
    
    #This chuck of code verifies the response variable selected.
    output$text1 <- renderText({
      if(input$y %in% names(my_data())) input$y
      else "Please Enter a Valid Response"
        })

    #This is the function that creates the regression models
    regression_models <- reactive({
          if(input$selection_criteria=="AIC")
            {
            if(input$regression_choice=="glm")
              {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,data=my_data())
                        step_model <- step(model,trace=0)
                        step_model}, error = function(e) {print("Enter a response variable")})
              }
            else if(input$regression_choice=="logit")
              {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,family=binomial("logit"),data=my_data())
                        step_model <- step(model,trace=0)
                        step_model}, error = function(e) {print("Enter a binary response variable for logistics regression!!!")})              
              }
            else
              {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,family="poisson",data=my_data())
                        step_model <- step(model,trace=0)
                        step_model}, error = function(e) {print("Enter a count response variable for poisson regression!!!")})
              }
          }
          else
            {
            if(input$regression_choice=="glm")
              {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,data=my_data())
                        step_model <- step(model,trace=0,k=log(nrow(my_data())))
                        step_model}, error = function(e) {print("Enter a response variable")})
              }
              else if(input$regression_choice=="logit")
                {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,family=binomial("logit"),data=my_data())
                        step_model <- step(model,trace=0,k=log(nrow(my_data())))
                        step_model}, error = function(e) {print("Enter a binary response variable for logistics regression!!!")})              
                }
              else
                {
              tryCatch({response_name <- paste(input$y)
                        formula <- paste(response_name,"~.",sep="")
                        model <- glm(formula,family="poisson",data=my_data())
                        step_model <- step(model,trace=0,k=log(nrow(my_data())))
                        step_model}, Warning = function(w){print("Please enter a count variable with integer value!!")},error = function(e) {print("Enter a count response variable for poisson regression!!!")})
                }   
            }
    })
    
    #This piece of code passes the summary of the regression model created by the above function.
    output$regression_output <- renderPrint({
      input$regression_modeller
      isolate(if(input$y != "" & class(my_data())=="data.frame"){summary(regression_models())}
      else{"Upload a Data Set!"})
        })
    
    #This code chunck creates confidence intervals for the regression co-efficients of the regression model.
    output$confidence_intervals <- renderPrint({
      input$regression_modeller
      isolate(if(input$y != "" & class(my_data())=="data.frame"){confint(regression_models())}
              else{"Regression hasn't been performed yet!"})
      })
    
    #This chart plots the predicted values of response variables along over the actual values of response variable.
    output$regression_plot <-renderChart({
      input$makeplot
      isolate(if(class(regression_models())[1]=="glm")
        {
        if(input$regression_choice=="logit"){
          yhat <- regression_models()$fitted.values
          yhat[yhat>=0.5] = 1
          yhat[yhat<0.5] = 0}
        else{
          yhat <- regression_models()$fitted.values
        }
        predicted_dataset <- data.frame(observations=1:nrow(my_data()),y=regression_models()$y,yhat)
      m1 <- mPlot(x = "observations", y = c("y","yhat"), type = "Line",data=predicted_dataset)
      m1$set(main="Response Variable Plot")
      m1$addParams(dom="regression_plot")
      return(m1)
      }
      else
        {predicted_dataset <- data.frame(observations=1:100,original=rep(0,100),predicted=rep(0,100))
           m1 <- mPlot(x = "observations", y = c("y","yhat"), type = "Line",data=predicted_dataset)
           m1$set(main="Response Variable Plot")
           m1$addParams(dom="regression_plot")
           return(m1) 
      })
      })
    
    #The below code produces the residual plots for the regression model.
    output$residual_plots <- renderPlot({
      input$residualplots
      isolate(
        if(class(regression_models())[1]=="glm"){par(mfrow=c(2,2),bg="deepskyblue4",pch=22)
                                                 plot(regression_models())}
        else{plot(1, type="n", axes=F, xlab="", ylab="")}
      )
                                       })
    
    
  }
)