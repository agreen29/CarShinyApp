library(markdown)
library(shiny)
library(tidyverse)
library(ggplot2)
library(car)
library(DT)
library(shinythemes)

music_read <- read.csv(file = "https://www.dropbox.com/s/gyfj104hddo3ml2/STAT323_ProjectData.csv?dl=1", sep =",", header = TRUE, row.names = NULL)

colnames(music_read) <- c("Name", "Major", "Gender", "Q1", "Q2", "Q3", "Article_Type", "Music_Type", "Correct", "Time", "HrsReadWk", "HrsMusicWk", "ReadWhileListen")


#Starting the navigation bar stating Car Package
navbarPage(theme = shinytheme("cosmo"), "The Car Package!",
           #First tab iniated of the website
           tabPanel("Data",
                    #Intro at the top of this page talking about the data being used
                    mainPanel(h1("Exploring the Data"),
                              p("The following dataset represents the data that will be explored 
                                throughout this application. This dataset was created in an attempt to discover
                                how listening to music while reading affects reading speed and comprehension. 
                                The study took 24 second year California Polytechnic State University Students within the 
                                College of Math and Science and assigned them
                                a music type (EDM, Rap, Classical, or No Music) and an article type (Challenging or Easy). 
                                Following reading the article, the study participants were asked to take a quiz consisting of 
                                five questions (score out of 5 is recorded within the variable Correct). 
                                For every question answered wrong, 10 seconds were added to the time recorded for the 
                                study participant to read the article."), br(),
                              p("Q1: On average, how many hours a week do you spend reading (for fun or for class)? 
                                (A:< 5 hours B: 5 - 10 hours C: 11 - 15 hours D: 15 + hours"),
                              p("Q2: On average, how many hours a week do you spend listening to music?  
                                A: < 7 hours ( < 1 hour a day)  B: 7 - 14 hours (1 - 2 hrs a days) 
                                C: 15 - 21 hours (2 - 3 hrs a day) D: 15 + hours (3 + hrs a day) "),
                              p("Q3: Do you listen to music while you read? A: Always B: Sometimes C: Rarely D: Never"),
                              p("Correct: The humber of questions, out of 5, that were answered correctly on the quiz."),
                              p("HrsReadWk: This is an indicator variable for Q1 where 1 = A, 2 = B, 3 = C and 4 = D."),
                              p("HrsMusicWk: This is an indicator variable for Q2 where 1 = A, 2 = B, 3 = C and 4 = D"),
                              p("ReadWhileListen: This is an indicator variable for Q2 where 1 = A, 2 = B, 3 = C and 4 = D"),
                              helpText("Explore the data by choosing options to sort by."),
                              #The following code creates columns to put each select input within, the columns are of length 3
                              fluidRow(
                                #Creates an input selecter for Gender
                                column(3,
                                       selectInput("gender",
                                                   "Gender:",
                                                   c("All", unique(as.character(music_read$Gender))))
                                ),
                                #Creates an input selecter for Major
                                column(3,
                                       selectInput("major",
                                                   "Major:",
                                                   c("All", unique(as.character(music_read$Major))))
                                ),
                                #Creates an input selecter for Article Type
                                column(3,
                                       selectInput("articletype",
                                                   "Article Type:",
                                                   c("All", unique(as.character(music_read$Article_Type))))
                                ),
                                #Creates an input selecter for Music Type
                                column(3,
                                       selectInput("musictype",
                                                   "Music Type:",
                                                   c("All", unique(as.character(music_read$Music_Type))))
                                ),
                                #Creates the data table within our main panel
                                DT::dataTableOutput("table")
                              )
                              )
                                ),
           #Second tab of the website
           navbarMenu("ANOVA",
                      #Creating the first tab in the drop down menu 
                      tabPanel("Overview of ANOVA and Test Results",
                               #Creating the layout for the side bar and main panel
                               sidebarLayout(
                                 #Creating the side bar
                                 sidebarPanel(
                                   helpText("Explore ANOVA output by selecting a response and explanatory variable."),
                                   #Allowing the user to input their chosen response variable
                                   selectInput("rvar",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ), 
                                   #Allowing the user to input their chosen explanatory variable
                                   selectInput("evar",
                                               "Explanatory Variable:",
                                               choices = c("Major", "Gender", "Article Type","Music Type" ),
                                               selected = "ReadWhileListen"
                                   ),
                                   #Creating text shown at the end of the side bar
                                   helpText("Data from Music_Read")
                                   
                                 ),
                                 #Creating what will be displayed on the main page 
                                 mainPanel(
                                   tabsetPanel(
                                     #Creating tab heading before displaying the output
                                     tabPanel("ANOVA Table",
                                              #Displaying the ANOVA table on the main page        
                                              tableOutput(outputId = "ANOVA")
                                     ),
                                     #Displaying an introduction into ANOVA displayed at the top of the website
                                     tabPanel(
                                       helpText("The ANOVA analysis finds significant relationships between the explanatory variables and the response variable
                                                when we have multiple groups wihtin each variable. ANOVA stands for Analysis of Variance and analyzes the ratio
                                                of the between group variablility and the within group variability. If the within group variability is large, the
                                                F-statistic will be large and the p-value will be small, thus significant, indicating that our groups are significant.")
                                       )
                                       ),
                                   tabsetPanel(
                                     #New heading for Assumptions seperated from ANOVA table
                                     tabPanel("Assumption Check:",
                                              helpText("Equal Variance: If the Levene's test is not significant and the Residual vs. Predicted Plot has even spreads,
                                                       we can assume that this assumption is met."),
                                              helpText("Normality: If the residuals follow the linear line on the Normal Quantile Plot, we can assume that this 
                                                       assumption is met."), 
                                              helpText("Independence: To check the independence assumption, the design of the analysis must be examined to ensure the 
                                                       response variable of the data points do not depend on each other or affect other data points' response variable.")
                                              )
                                              )
                                   )
                                       )
                                     ),
                      #Second tab in the ANOVA drop down menu
                      tabPanel("Normality Assumption",
                               #Creating the layout for the side bar and main panel
                               sidebarLayout(
                                 #Creating the side bar
                                 sidebarPanel(
                                   #Text displayed at the top of the side bar
                                   helpText("Explore the normality assumption by selecting a response variable."),
                                   #Allowing the user to choose their specified response variable
                                   selectInput("rvar_norm",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ), 
                                   #Text displayed at the bottom of the side bar
                                   helpText("Data from Music_Read")
                                   
                                 ),
                                 #Creating what will be displayed in the main panel of the page
                                 mainPanel(
                                   tabsetPanel(
                                     #Creating heading and display the output
                                     tabPanel("Normal Quantile Plot of Residuals",
                                              plotOutput(outputId = "normplot")
                                     )
                                   )
                                   
                                 )
                                 
                               )
                      ),
                      #Creating the third tab in the ANOVA drop down menu
                      tabPanel("Equal Variance Assumption",
                               #Creating the layout for the side bar and main panel
                               sidebarLayout(
                                 #Creating the side bar
                                 sidebarPanel(
                                   helpText("Explore the equal variance assumption by selecting a response variable 
                                            and an explanatory variable of interest (A Levene's test will also be calculated)."),
                                   #Allowing user to choose their specified response variable
                                   selectInput("rvar2",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #Allowing the user to choose their specified explanatory variable
                                   selectInput("evar2",
                                               "Explanatory Variable:",
                                               choices = c("Major", "Gender", "Article Type","Music Type" ),
                                               selected = "ReadWhileListen"
                                   ),
                                   #Text displayed at the end of the side bar
                                   helpText("Data from Music_Read")
                                   
                                            ),
                                 #Creating what will be displayed on the main page
                                 mainPanel(
                                   tabsetPanel(
                                     #Heading and plot shown on the main panel of this page
                                     tabPanel("Residual Plots to Check Equal Variance", 
                                              plotOutput(outputId = "varplot")
                                              
                                     )
                                   ),
                                   tabsetPanel(
                                     #Creating the second output shown on the main page 
                                     tabPanel("Levene Test for Equal Variance",
                                              tableOutput(outputId = "levenetest")
                                              
                                     )
                                   )
                                   
                                   
                                 )
                                 
                                   )
                               
                      )
                    ),
           #Below we are creating a new navbarmenu for Linear Regressiono
           navbarMenu("Linear Regression",
                      #The code below creates the first tab within the navbar menu
                      tabPanel("Overview of Linear Model and Test Results",
                               sidebarLayout(
                                 #Creating a sidebar panel where input can be selected
                                 sidebarPanel(
                                   helpText("Explore the output of a linear regression by selecting a response variable
                                            and two explanatory variables."),
                                   #Selecting a response variable
                                   selectInput("var4",
                                               "Response Variable:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #Selecting the first explanatory variable
                                   selectInput("evar7",
                                               "Explanatory Variable 1:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #Selecting the second explanatory variable
                                   selectInput("evar8",
                                               "Explanatory Variable 2:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   helpText("Data from Music_Read")
                                   ),
                                 #The main panel is created here -- we have text and output for our linear model
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Overview and Test Results of Linear Model",
                                              verbatimTextOutput("lm")
                                     )
                                     ,
                                     tabPanel(
                                       #Text describing what linear regression is
                                       helpText("Linear regression is a statistical method used to create a linear model. The linear model is used to predict behavior based 
                                                on the relationship between one or more explanatory variables and the response.")
                                       )
                                     ),
                                   tabsetPanel(
                                     #Text explaining the assumptions of linear regression
                                     tabPanel("Assumption Check:",
                                              helpText("Linear relationship: Looking at the scatter plots there should be a linear trend in the data."),
                                              helpText("Equal Variance: The residual plots should have no pattern and no group of residuals that is 
                                                       2x the size of the smallest residual."), 
                                              helpText("Variance Inflation Factor: values higher than 10 indicate that multicollinearity is a problem. ")
                                              )
                                   )
                                   )
                                 
                                   )
                      ),
                      #The next tab within our navbar menu is created here, it is called Scatterplots
                      tabPanel("Scatterplots",
                               sidebarLayout(
                                 sidebarPanel(
                                   #Help text below shows the user how they can use the interactive feature
                                   helpText("Select a response variable and an explanatory variable to see how they are plotted
                                            against one another."),
                                   #Selects a response variable
                                   selectInput("var_s",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #Selects an explanatory variable 
                                   selectInput("evar_s",
                                               "Explanatory Variable:",
                                               choices = c("Correct", "Time", "HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   helpText("Data from Music_Read")
                                   ),
                                 
                                 #within the main panel, the scatterplots are displayed -- outputs plot1 where the scatterplots
                                 #are saved
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Scatterplots",
                                              plotOutput("plot1")
                                     )))
                                 
                               )
                               ),
                      #The next tab within the navbar menu is residual plots
                      tabPanel("Residual Plot",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Select a response variable and two explantory variables to output residual plots."),
                                   #Below our the options that can be selected within the tab
                                   selectInput("var_RP",
                                               "Response Variable:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   selectInput("evar_RP",
                                               "Explanatory Variable 1:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   selectInput("evar_RP2",
                                               "Explanatory Variable 2:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   helpText("Data from Music_Read")
                                 ),
                                 #This chunk of code outputs the residual plots 
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("Residual Plots",
                                              plotOutput("residualPlot")
                                     )
                                   )
                                 )
                               )
                      ),
                      #This chunk of outputs the VIF within another tab of the navbar menu
                      tabPanel("Variance Inflation Factor",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Select a response variable and two explanatory variables to view the calculated 
                                            variance inflation factor."),
                                   #Inputs that can be selected to display
                                   selectInput("var3",
                                               "Response Variable:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Correct"
                                   ),
                                   selectInput("evar5",
                                               "Explanatory Variable 1:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   selectInput("evar6",
                                               "Explanatory Variable 2:",
                                               choices = c("Correct", "Time","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "HrsMusicWk"
                                   ),
                                   helpText("Data from Music_Read")
                                   ),
                                 #VIF for the chosen inputs is displayed here
                                 mainPanel(
                                   tabsetPanel(
                                     tabPanel("The Results of the Variance Inflation Factor",
                                              verbatimTextOutput("vif"),
                                              helpText("NOTE: Variance inflation factor will not work if the same explanatory variable is 
                                                       chosen twice.")
                                              )
                                     )
                                   )
                                 )
                               )
                      
           ),
           #Fourth tab of the website
           navbarMenu("Graphs",
                      #Firts dropdown tab for graph
                      tabPanel("Density Plot",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Select a response variable and a variable to group by to produce a density plot."),
                                   #taking user input for a response variable argument
                                   selectInput("rvard",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #taking user input for a explanitory/factor variable argument
                                   selectInput("evard",
                                               "Group By Variable:",
                                               choices = c("Gender", "Article Type","Music Type" ),
                                               selected = "ReadWhileListen"
                                   ),
                                   #note of where data came from
                                   helpText("Data from Music_Read")
                                 ),
                                 mainPanel(
                                   tabsetPanel(
                                     #have tab at the top of the page for header
                                     tabPanel("Density Plot",
                                              #display the density plot with variables of users choosing
                                              plotOutput(outputId = "densityPlot")
                                     )
                                   )
                                 )
                               )
                      ),
                      #Second dropdown tab for graph
                      tabPanel("Ellipse Plot",
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Select a response variable and a group by variable in order to produce 
                                            an ellipse plot."),
                                   #taking user input for a response variable argument
                                   selectInput("rvar_e",
                                               "Response Variable:",
                                               choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen" ),
                                               selected = "Time"
                                   ),
                                   #taking user input for a explanitory/factor variable argument
                                   selectInput("evar_e",
                                               "Group By Variable:",
                                               choices = c("Gender", "Article Type","Music Type" ),
                                               selected = "Gender"
                                   ),
                                   #note of where data came from
                                   helpText("Data from Music_Read")
                                   ),
                                 mainPanel(
                                   tabsetPanel(
                                     #have tab at the top of the page for header
                                     tabPanel("Ellipse Plot",
                                              #display the Ellipse plot with variables of users choosing
                                              plotOutput(outputId = "ellipsePlot")
                                     )
                                   )
                                 )
                               )
           ),
           #Third dropdown tab for graph
           tabPanel("Boxplot",
                    sidebarLayout(
                      sidebarPanel(
                        helpText("Select a response variable and group by variable in order to produce boxplots of the variables."),
                        #taking user input for a response variable argument
                        selectInput("rvar_b",
                                    "Response Variable:",
                                    choices = c("Correct", "Time", "HrsReadWk","HrsMusicWk", "ReadWhileListen"),
                                    selected = "Time"
                        ),
                        #taking user input for a explanitory/factor variable argument
                        selectInput("evar_b",
                                    "Group By Variable:",
                                    choices = c("Gender", "Article Type","Music Type" ),
                                    selected = "Gender"
                        ),
                        #note of where data came from
                        helpText("Data from Music_Read")
                      ),
                      mainPanel(
                        tabsetPanel(
                          #have tab at the top of the page for header
                          tabPanel("Boxplot",
                                   #display the boxplot with variables of users choosing
                                   plotOutput(outputId = "boxPlot")
                          )
                        )
                      )
                    )
           )
           )
           )