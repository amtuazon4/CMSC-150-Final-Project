#Name:              Andre M. Tuazon
#Student Number:    2020-00839
#Section:           AB1L

#Exercise 10


#imports the shiny library
library(shiny)

#This serves as the User Interface of the Application
shinyUI(fluidPage(
  
  #Shows the title
  titlePanel(title = "Quadratic Spline Interpolation and Simplex Method Application"),
  
  #This divides the application to 2 tabs, one for the Quadratic Spline Interpolation, and the other for the Simplex Method
  tabsetPanel( selected = "Quadratic Spline Interpolation",
    tabPanel(title = "Quadratic Spline Interpolation",
        
        #Divides the Quadratic Spline Interpolation to 2 parts which is a sidebar panel and a main panel
        sidebarLayout(
          
          sidebarPanel(
            
            #inputs for the Quadratic Spline Interpolation
            h2("Quadratic Spline Interpolation"),
            numericInput(inputId = "qsiNumPts", label = "Number of data points:", value = 4, min = 4),
            uiOutput("qsiInputUI"),
            
            #action button to show the result
            actionButton(inputId = "qsiSolve", label = "Solve")
            
          ),
          #This Main panel shows the 1 textOutput for the approximated y-value,
          #                         and 1 verbatimTextOutput for the console output of the poly.qsi function
          mainPanel(
            
            #ui output for the results of the Quadratic Spline Interpolation
            uiOutput("qsiUI")
            
          )
        )
             
    ),
    
    

    #tab for Simplex Method
    tabPanel(title = "Simplex Method",
      #This divides the current tab to 2 more tabs which are the tab for the DIVOC Shipping Analysis and the the other one for normal simplex method
      tabsetPanel(selected = "Normal Simplex Method",
        
          #This is the tab for the Normal Simplex Method
          tabPanel(title = "Normal Simplex Method",

                   
#Simplex Method part------------------------------------------------------------
            sidebarLayout(
              
              sidebarPanel(
                
                #inputs for the Simplex Method
                h2("Simplex Method"),
                selectInput(inputId = "MaxMin", label = "Maximize or Minimize", choices = c("Maximize","Minimize"), selected = "Maximize"),
                numericInput(inputId = "numVar", label = "Number of Variables", value = "2", min = 1),
                numericInput(inputId = "numEq", label = "Number of Constraints", value = "4", min = 1),
                br(),
                h3("Objective Function (Z):"),
                textOutput("objFuncRep"),
                br(),
                uiOutput(outputId = "objFuncUI"),
                br(),
                uiOutput("eqUI"),
                
                #action button to show the results
                actionButton(inputId = "simplexSolve", label = "Solve")
              ),

              mainPanel(
                
                #ui output for the results
                uiOutput("simplexUI")
                
              )
            )       
        
                 
        ),
        
#-------------------------------------------------------------------------------
        #This is the tab for DIVOC Shipping Analysis
        tabPanel(title = "DIVOC Shipping Analysis",
          
          #This divides the current tab to a sidebar Panel and a main panel       
          sidebarLayout(
            
            sidebarPanel(
              
              #inputs for the DIVOC Shipping Analysis problem in the pdf
              h2("DIVOC Shipping Analysis"),
              uiOutput("divocInputUI"),
              
              #action button to show the results
              actionButton(inputId = "divocSolve", label = "Solve")
             
            ),
            
            
            mainPanel(
              
              #ui output to show the results
              uiOutput("divocUI"),
            )
          )
        )
        
      )
    ))
))

