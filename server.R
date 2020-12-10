library(shiny)
library(tidyverse)
library(ggplot2)
library(car)
library(DT)
library(broom)
library(shinythemes)

#here we are creating a funciton with inputs of "input" and "output" that allow us to create what we want to output 
#to our shiny app to be visible by the viewers
function(input, output) {
  music_read <- read.csv(file = "https://www.dropbox.com/s/gyfj104hddo3ml2/STAT323_ProjectData.csv?dl=1", sep =",", header = TRUE, row.names = NULL)
  
  colnames(music_read) <- c("Name", "Major", "Gender", "Q1", "Q2", "Q3", "Article_Type", "Music_Type", "Correct", "Time", "HrsReadWk", "HrsMusicWk", "ReadWhileListen")
  
  #renderPlot for the normal quantile plot to be called in the UI
  output$normplot <- renderPlot({
    #Takes what the user specified and saves it as response
    response <- switch(input$rvar_norm, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Normal Quantile Plot function called; will display the plot
    qqPlot(x = response, distribution = "norm")
  })
  #renderPlot for the residual plot to be called in the UI
  output$varplot <- renderPlot({
    #Takes what the user specified and saves it as response
    response <- switch(input$rvar2, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Takes what the user specified and saves it as explanatory
    explanatory <- switch(input$evar2, 
                          "Major" = music_read$Major, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #Residual Plot function called; will display the plot
    residualPlots(lm(response ~ explanatory, data =music_read), main = "Residual Plots to Check Equal Variance")
    
  })
  #renderTable for the levene test to be called in the UI
  output$levenetest <- renderTable({
    #Takes what the user specified and saves it as response
    response <- switch(input$rvar2, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Takes what the user specified and saves it as explanatory
    explanatory <- switch(input$evar, 
                          "Major" = music_read$Major, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #Levene Test function called; will display the test results
    leveneTest(lm(response ~ explanatory, data =music_read))
  })
  
  #renderTable for the ANOVA table to be called in the UI
  output$ANOVA <- renderTable({
    #Takes what the user specified and saves it as response
    response <- switch(input$rvar, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Takes what the user specified and saves it as explanatory
    explanatory <- switch(input$evar, 
                          "Major" = music_read$Major, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #Anova table function called; will display the ANOVA results 
    Anova(lm(response ~ explanatory, data =music_read))
  }
  )
  #The following code creates scatterplots for selected response and explanatory data variables
  output$plot1 <- renderPlot({
    #The possible inputs for the response variable are given here
    response <- switch(input$var_s, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #The possible inputs for the explanatory variable are given here
    explanatory <- switch(input$evar_s, 
                          "Correct" = music_read$Correct, 
                          "Time" = music_read$Time,
                          "HrsMusicWk" = music_read$HrsMusicWk, 
                          "ReadWhileListen" = music_read$ReadWhileListen)
    #This line of code below creates a scatterplot for the selected response and explanatory variables 
    scatterplot(y = response, x = explanatory)
  })
  
  #The following code outputs a residual plot
  output$residualPlot <- renderPlot({
    #Here the possible variables for the response are given -- all numeric
    response <- switch(input$var_RP, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Here the possible variables for the two explanatory variables are given -- these are also numeric
    explanatory <- switch(input$evar_RP, 
                          "Correct" = music_read$Correct, 
                          "Time" = music_read$Time,
                          "HrsMusicWk" = music_read$HrsMusicWk, 
                          "ReadWhileListen" = music_read$ReadWhileListen)
    explanatory2 <- switch(input$evar_RP2, 
                           "Correct" = music_read$Correct, 
                           "Time" = music_read$Time,
                           "HrsMusicWk" = music_read$HrsMusicWk, 
                           "ReadWhileListen" = music_read$ReadWhileListen)
    #Here the residual plot is created with the selected variables for the response 
    #and the selected explanatory variables
    residualPlots(lm(response ~ explanatory + explanatory2, data = music_read), 
                  main = "Residual Plot", xlab = "Residuals")
  })
  #The following code outputs a VIF table for a linear model with two explanatory variables 
  output$vif <- renderPrint({
    #Here the possible variables for the response are given -- all numeric
    response <- switch(input$var3, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsMusicWk" = music_read$HrsMusicWk,
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Here the possible variables for the two explanatory variables are given -- these are also numeric
    explanatory <- switch(input$evar5, 
                          "Correct" = music_read$Correct, 
                          "Time" = music_read$Time,
                          "HrsMusicWk" = music_read$HrsMusicWk, 
                          "ReadWhileListen" = music_read$ReadWhileListen)
    explanatory2 <- switch(input$evar6, 
                           "Correct" = music_read$Correct, 
                           "Time" = music_read$Time,
                           "HrsMusicWk" = music_read$HrsMusicWk, 
                           "ReadWhileListen" = music_read$ReadWhileListen)
    
    vif(lm(response ~ explanatory + explanatory2, data = music_read))
  })
  
  #This code outputs a table summarizing a linear model between specified variables
  output$lm <- renderPrint({
    #Here the possible variables for the response are given -- all numeric
    response <- switch(input$var4, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #Here the possible variables for the two explanatory variables are given -- these are also numeric
    explanatory <- switch(input$evar7, 
                          "Correct" = music_read$Correct, 
                          "Time" = music_read$Time,
                          "HrsMusicWk" = music_read$HrsMusicWk, 
                          "ReadWhileListen" = music_read$ReadWhileListen)
    explanatory2 <- switch(input$evar8, 
                           "Correct" = music_read$Correct, 
                           "Time" = music_read$Time,
                           "HrsMusicWk" = music_read$HrsMusicWk, 
                           "ReadWhileListen" = music_read$ReadWhileListen)
    #this gives a summary of the linear model with the explanatory variables and response varibale chosen 
    #by the user 
    summary(lm(response ~ explanatory + explanatory2, data = music_read))
  })
  
  #renderPlot for the density plot to be called in the UI
  output$densityPlot <- renderPlot({
    #takes user input and uses it to determine the response variable to use
    response <- switch(input$rvard, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #takes user input and uses it to determine the explanitory/factor variable to use
    explanatory <- switch(input$evard, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #densityplot function called, displays density plot of desired variables
    densityPlot(response, g = explanatory)
  }) 
  
  #renderPlot for the condidence ellipse to be called in the UI
  output$ellipsePlot <- renderPlot({
    #takes user input and uses it to determine the response variable to use
    response <- switch(input$rvar_e, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #takes user input and uses it to determine the explanitory/factor variable to use
    explanatory <- switch(input$evar_e, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #confidenceEllipse function called, displays confidence ellipse of desired variables
    confidenceEllipse(lm(response ~ explanatory),levels=0.95, Scheffe=FALSE, center.pch=19, center.cex=1.5)
  }) 
  
  #renderPlot for the boxplot to be called in the UI
  output$boxPlot <- renderPlot({
    #takes user input and uses it to determine the response variable to use
    response <- switch(input$rvar_b, 
                       "Correct" = music_read$Correct, 
                       "Time" = music_read$Time,
                       "HrsReadWk"  = music_read$HrsReadWk,
                       "HrsMusicWk" = music_read$HrsMusicWk, 
                       "ReadWhileListen" = music_read$ReadWhileListen)
    #takes user input and uses it to determine the explanitory/factor variable to use
    explanatory <- switch(input$evar_b, 
                          "Gender" = music_read$Gender,
                          "Article Type"  = music_read$Article_Type,
                          "Music Type" = music_read$Music_Type)
    #Boxplot function called, displays boxplot of desired variables
    Boxplot(formula = response ~ explanatory)
  }) 
  
  #Creates a data table output named "Table"
  output$table <- DT::renderDataTable(DT::datatable({
    data <- music_read #Reading in our data into a new data frame called data
    #The following code is for the data tab
    #If input for major is not all, then the data table only shows the selected major
    if (input$major != "All") {
      data <- data[data$Major == input$major,]
    }
    #If input for gender is not all, then the data table only shows the selected gender
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender,]
    }
    #If input for article type is not all, then the data table only shows the selected article type
    if (input$articletype != "All") {
      data <- data[data$Article_Type == input$articletype,]
    }
    #If input for music type is not all, then the data table only shows the selected music type 
    if (input$musictype != "All") {
      data <- data[data$Music_Type == input$musictype,]
    }
    data
  }))
  
}