#Name:              Andre M. Tuazon
#Student Number:    2020-00839
#Section:           AB1L

#Exercise 10


#Sources files needed to get the poly.qsi and simplex functions, and tableau3 for the DIVOC Shipping Analysis
source("TuazonEx08.R")
source("TuazonEx09.R")

#imports the shiny library
library(shiny)

#This serves as the Server of the Application
shinyServer(function(input, output){
  
  #to render the UI for the inputs of x and y depending on the number of data points
  output$qsiInputUI <- renderUI({
    ui_list = vector(mode = "list")
    x_names = sapply(1:input$qsiNumPts, function(i){paste0("qsiX",i)})
    y_names = sapply(1:input$qsiNumPts, function(i){paste0("qsiY",i)})
    xlabels = sapply(0:(input$qsiNumPts-1), function(i){paste0("x", i, ":")})
    ylabels = sapply(0:(input$qsiNumPts-1), function(i){paste0("y", i, ":")})
    
    
    for(i in 1:input$qsiNumPts){
      new_list = vector(mode = "list")
      new_list[[1]] = h3(paste0("Data Point ", i))
      new_list[[2]] = numericInput(inputId = x_names[i], label = xlabels[i], value = "0")
      new_list[[3]] = numericInput(inputId = y_names[i], label = ylabels[i], value = "0")
      ui_list[[i]]  = new_list
    }
    ui_list[[length(ui_list)+1]] = hr()
    ui_list[[length(ui_list)+1]] = h3("To be Approximated")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "approximate", label = "x-value to  be approximated", value = "5")
    
    return(ui_list)
    
  })
  
  #solves the Quadratic Spline Interpolation problem depending on the data points inputted
  #it will trigger when the action button with inputId qsiSolve is clicked
  qsiResult <- eventReactive(input$qsiSolve,{
    ind = vector()
    dep = vector()
    
    for(i in 1:input$qsiNumPts){
      x_val = paste0("input$qsiX",i)
      y_val = paste0("input$qsiY",i)
      
      x_val = eval(parse(text = x_val))
      y_val = eval(parse(text = y_val))
      
      ind[i] = x_val
      dep[i] = y_val
    }
    
    new_list = list(ind,dep)
    approx = input$approximate
    
    answer = poly.qsi(new_list, approx)
    return(answer)
  })
  
  #render the text for the resulting y-value of the Quadratic Spline Interpolation
  output$qsiYApprox <- renderText({
    return(paste0("The approximated y-value is ", (qsiResult())$y))
  })
  
  #renders the output of the functions from the poly.qsi function
  output$qsiResultConsole <- renderPrint({
    x = (qsiResult())$qsi.fxns
    return(x)
  })
  
  
  #creates the UI needed to show the Results of the Quadratic Spline Interpolation from the given data points
  output$qsiUI <- renderUI({
    ui_list = vector(mode = "list")
    if(!is.list(qsiResult())){
      ui_list[[length(ui_list)+1]] = h3("No Solution")
    }else{
      ui_list[[length(ui_list)+1]] = h1("Results")
      ui_list[[length(ui_list)+1]] = hr()
      ui_list[[length(ui_list)+1]] = h3("The Approximated y-value")
      ui_list[[length(ui_list)+1]] = h4(textOutput("qsiYApprox"))
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Functions")
      ui_list[[length(ui_list)+1]] = verbatimTextOutput("qsiResultConsole")
    }
    return(ui_list)
  })
  
  
  
#Normal Simplex Method part ----------------------------------------------------
  
  #Generates the text to represent the Objective Function
  output$objFuncRep <- renderText({
    the_string = "Z = __x1 "
    
    if(input$numVar > 1){  
      for(i in 2:input$numVar){
        the_string = paste0(the_string, "+ __x", i, " ")
      }
    }
    
    return(the_string)
  })
  
  
  #render the UI for the objective function inputs
  output$objFuncUI <- renderUI({
    
    objvar_names = sapply(1:(input$numVar), function(i){paste0("objVar",i)})
    objlabel_names = sapply(1:(input$numVar), function(i){paste0("Coefficient of x",i, ":")})
    
    new_list = vector(mode = "list")
    for(i in 1:(input$numVar)){
      new_list[[i]] = numericInput(inputId = objvar_names[i], label = objlabel_names[i], value = "0")
    }
    
    return(new_list)

  })
  
  #generates the UI inputs for each constraint specifically its variables, symbol, and constant
  output$eqUI = renderUI({
    var_names = vector(mode = "list")
    for(i in 1:(input$numEq)){
      new_vec = vector()
      for(j in 1:(input$numVar)){
        new_vec[j] = paste0("eq",i,"var",j)
        var_names[[i]] = new_vec
      }
    }
    label_names = sapply(1:(input$numVar), function(i){paste0("Coefficient of x",i,":")})
    
    symbol_names = sapply(1:(input$numEq), function(i){paste0("sym", i)})
    
    constant_names = sapply(1:(input$numEq), function(i){paste0("const", i)})
    
    ui_list = vector(mode = "list")
    
    for(i in 1:input$numEq){
      eq_list = vector(mode = "list")
      ui_list[[i]] = eq_list
      
      ui_list[[i]][[1]] = h3({paste0("Constraint ", i)})
      
      for(j in 1:input$numVar){
        ui_list[[i]][[j+1]] = numericInput(inputId = var_names[[i]][j], label = label_names[j], value = "0")
      }
      
      ui_list[[i]][[length(ui_list[[i]])+1]] = selectInput(inputId = symbol_names[i], label = "Symbol:", choices = c(">=", "<=", "="), selected = "<=")
      ui_list[[i]][[length(ui_list[[i]])+1]] = numericInput(inputId = constant_names[i], label = "Constant:", value = "0")
      ui_list[[i]][[length(ui_list[[i]])+1]] = br()
    }
    
    return(ui_list)
    
  })
  
  #generates the initial tableau for the Simplex Method from the inputted values from the objective function and constraints depending if the user selected Maximize or Minimize
  simplexMatrix <- reactive({
    #generates the variables names for the dimension names of the initial tableau
    variables = sapply(1:input$numVar, function(i){paste0("x",i)})

    
    #does the following code when the user chose Maximize    
    if(input$MaxMin == "Maximize"){
      
      #stores each input in a matrix called data_inputted
      slack_count = 0
      slack_index = vector()
      data_inputted = matrix(0, nrow = input$numEq, ncol = (input$numVar+1))
      for(i in 1:input$numEq){
        val_vec = vector()
        for(j in 1:input$numVar){
          val = paste0("input$eq",i,"var",j)
          val = eval(parse(text = val))
          
          val_vec[length(val_vec)+1] = val
        }
        con = paste0("input$const",i)
        con = eval(parse(text = con))
        val_vec[length(val_vec)+1] = con
        
        data_inputted[i,] = val_vec
        
        symVal = paste0("input$sym",i)
        symVal = eval(parse(text = symVal))
        
        #adjust the values of the constraints depending on the symbol selected
        if(symVal == "<="){
          slack_count = slack_count + 1
          slack_index[length(slack_index)+1] = i
        } else if(symVal == ">="){
          data_inputted[i,] = data_inputted[i,]*-1
          slack_count = slack_count + 1
          slack_index[length(slack_index)+1] = i
        } 
        
      }
      
      #generates more dimension names for the initial tableau, specifically the rows and the slack variables
      slack_variables = sapply(1:slack_count, function(i){paste0("s",i)})
      constraints = sapply(1:input$numEq, function(i){paste0("r", i)})
      
      #the following sets up the initial tableau with 0's named initial tableau
      col_names = variables
      if(slack_count != 0){
        for(i in slack_variables){
          col_names[length(col_names)+1] = i
        }
      }
      
      col_names[length(col_names)+1] = "Z"
      col_names[length(col_names)+1] = "Solution"
      
      row_names = constraints
      row_names[length(row_names)+1] = "obj func"
      
      numRow = length(row_names)
      numCol = length(col_names)
      
      initial_tableau = matrix(0, nrow = numRow, ncol = numCol, dimnames = list(row_names, col_names))
      
      #inputs the proper data in the initial tableau from the matrix data_inputted
      for(i in 1:nrow(data_inputted)){
        for(j in 1:(ncol(data_inputted)-1)){
          initial_tableau[i,j] = data_inputted[i,j]
        }
      }
      
      counter = 1
      for(i in slack_index){
        initial_tableau[i,(counter + ncol(data_inputted) - 1)] = 1
        counter = counter + 1
      }
      
      new_vec = data_inputted[,ncol(data_inputted)]
      initial_tableau[1:length(new_vec),ncol(initial_tableau)] = new_vec
      
      obj_func = vector()
      for(i in 1:input$numVar){
        obj_val = paste0("input$objVar",i)
        obj_val = eval(parse(text=obj_val))
        
        obj_func[length(obj_func)+1] = obj_val
      }
      obj_func = obj_func * -1
      
      initial_tableau[nrow(initial_tableau), 1:length(obj_func)] = obj_func
      
      initial_tableau[nrow(initial_tableau), (ncol(initial_tableau)-1)] = 1
      
      #returns the initial tableau
      return(initial_tableau)
      
    #does the following code when the user chose Minimize
    } else{ 
      
      #same as earlier, inputs the values inputted and does the proper changes depending on the symbol chosen in each constraint to a matrix called data_inputted
      data_inputted = matrix(0, nrow = input$numEq + 1, ncol = (input$numVar+1))
      for(i in 1:input$numEq){
        val_vec = vector()
        for(j in 1:input$numVar){
          val = paste0("input$eq",i,"var",j)
          val = eval(parse(text = val))
          if(is.na(val) || is.infinite(val) || is.nan(val)){
            return(NA)
          }
          
          
          val_vec[length(val_vec)+1] = val
        }
        con = paste0("input$const",i)
        con = eval(parse(text = con))
        val_vec[length(val_vec)+1] = con
        
        data_inputted[i,] = val_vec
        
        symVal = paste0("input$sym",i)
        symVal = eval(parse(text = symVal))
        
        if(symVal == "<="){
          data_inputted[i,] = data_inputted[i,]*-1
        } 
      }
      
      obj_values = vector()
      for(i in 1:input$numVar){
        obj_val = paste0("input$objVar", i)
        obj_val = eval(parse(text = obj_val))
        obj_values[i] = obj_val
      }
      
      obj_values[length(obj_values)+1] = 1
        
      data_inputted[nrow(data_inputted),] = obj_values
      
      #transposes data_inputted
      data_inputted = t(data_inputted)
      
      
      #sets up the initial tableau named initial_tableau with 0's
      numCol = input$numEq + input$numVar + 2
      numRow = input$numVar + 1
      
      slack_variables = sapply(1:input$numEq, function(i){paste0("s",i)})
      variables = sapply(1:input$numVar, function(i){paste0("x",i)})
      equations = sapply(1:input$numVar, function(i){paste0("r",i)})
      

      row_names = equations
      row_names[length(row_names)+1] = "obj func"
      
      
      col_names = slack_variables
      
      for(i in variables){
        col_names[length(col_names)+1] = i
      }
      col_names[length(col_names)+1] = "Z"
      col_names[length(col_names)+1] = "Solution"
      
      initial_tableau = matrix(0, nrow = length(row_names), ncol = length(col_names), dimnames = list(row_names, col_names))
      
      #inputs the values from the transposed data_inputted to the initial tableau
      for(i in 1:input$numVar){
        for(j in 1:input$numEq){
          initial_tableau[i,j] = data_inputted[i,j]
        }
      }
      
      for(i in 1:input$numVar){
        initial_tableau[i,(input$numEq + i)] = 1
      }
      
      initial_tableau[1:input$numVar, ncol(initial_tableau)] = data_inputted[1:input$numVar, ncol(data_inputted)]
      
      
      initial_tableau[nrow(initial_tableau), 1:input$numEq] = data_inputted[nrow(data_inputted), 1:input$numEq] * -1
      initial_tableau[nrow(initial_tableau), (ncol(initial_tableau)-1)] = 1
      
      #returns the initial tableau
      return(initial_tableau)
      }
    
  })
  
  #evaluates the initial tableau using the simplex function in TuazonEx09.R depending if the user chose Maximize or Minimize
  simplexResult <- eventReactive(input$simplexSolve,{
    if(input$MaxMin == "Maximize"){
      return(simplex(simplexMatrix(), TRUE, FALSE))
    } else{
      return(simplex(simplexMatrix(), FALSE, FALSE))
    }
  })
  
  #renders the Final Tableau from the Simplex function
  output$simplexFinalTableau <- renderTable({
    return((simplexResult())$final.tableau)
  },
    rownames = TRUE  
  )
  
  #renders the table for the final solution
  output$simplexFinalSolution <- renderTable({
      return((simplexResult())$basic.solution)  
  })
  
  #returns the choice of the user if its Maximize or Minimize when the action button with inputId simplexSolve is clicked
  maxMin <- eventReactive(input$simplexSolve,{
    return(input$MaxMin)
  })
  
  #returns the input of the user about the number of variables to be used in the simplex method when the action button with inputId simplexSolve is clicked
  VarCount <- eventReactive(input$simplexSolve,{
    return(input$numVar)
  })
  
  #returns the input of the user about the number of constraints to be used in the simplex method when the action button with inputId simplexSolve is clicked
  EqCount <- eventReactive(input$simplexSolve,{
    return(input$numEq)
  })
  
  #generates the text for the optimal value in the simplex method
  output$simplexOptimalValue <- renderText({
    if(maxMin() == "Maximize"){
      the_string = paste0("The Maximum Value is: ", (simplexResult())$opt.val)
      return(the_string)
    }else{
      the_string = paste0("The Minimum Value is: ", (simplexResult())$opt.val)
      return(the_string)
    }
  })
  
  #renders the UI for the output of the simplex method problem
  output$simplexUI <- renderUI({
    ui_list = vector(mode = "list")
    if(!is.list(simplexResult())){
      ui_list[[length(ui_list)+1]] = h3("No Solution")
    }else{
      ui_list[[length(ui_list)+1]] = h1("Results")
      ui_list[[length(ui_list)+1]] = hr()
      ui_list[[length(ui_list)+1]] = h3("Final Tableau")
      ui_list[[length(ui_list)+1]] = tableOutput("simplexFinalTableau")
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Final Solution")
      ui_list[[length(ui_list)+1]] = tableOutput("simplexFinalSolution")
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Optimal Value")
      ui_list[[length(ui_list)+1]] = h4(textOutput("simplexOptimalValue"))
    }
    return(ui_list)
  })
  

#-------------------------------------------------------------------------------

  
#DIVOC Shipping Analysis--------------------------------------------------------
  
  #renders the UI for the inputs of the data needed for DIVOC Shipping Analysis problem
  output$divocInputUI <- renderUI({
    ui_list = vector(mode = "list")
    
    ui_list[[length(ui_list)+1]] = h3("Demand of each Warehouse")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SAC", label = "Sacramento, California", value = "180")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SL", label = "Salt Lake City", value = "80")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "ALB", label = "Albuquerque, New Mexico", value = "200")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "CHI", label = "Chicago, Illinois", value = "160")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "NYC", label = "New York City, New York", value = "220")
    ui_list[[length(ui_list)+1]] = br()
    
    
    ui_list[[length(ui_list)+1]] = h3("Supply of each Plant")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "DEN", label = "Denver, Colorado", value = "310")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "PHO", label = "Phoenix, Arizona", value = "260")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "DAL", label = "Dallas, Texas", value = "280")
    ui_list[[length(ui_list)+1]] = br()
    
    ui_list[[length(ui_list)+1]] = h3("Shipping Cost from each Plant to each Warehouse")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SAC_DEN", label = "Denver to Sacramento", value = "10")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SAC_PHO", label = "Phoenix to Sacramento", value = "6")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SAC_DAL", label = "Dallas to Sacramento", value = "3")
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SL_DEN", label = "Denver to Salt Lake City", value = "8")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SL_PHO", label = "Phoenix to Salt Lake City", value = "5")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "SL_DAL", label = "Dallas to Salt Lake City", value = "4")
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "ALB_DEN", label = "Denver to Albuquerque", value = "6")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "ALB_PHO", label = "Phoenix to Albuquerque", value = "4")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "ALB_DAL", label = "Dallas to Albuquerque", value = "5")
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "CHI_DEN", label = "Denver to Chicago", value = "5")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "CHI_PHO", label = "Phoenix to Chicago", value = "3")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "CHI_DAL", label = "Dallas to Chicago", value = "5")
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "NYC_DEN", label = "Denver to New York City", value = "4")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "NYC_PHO", label = "Phoenix to New York City", value = "6")
    ui_list[[length(ui_list)+1]] = numericInput(inputId = "NYC_DAL", label = "Dallas to New York City", value = "9")
    ui_list[[length(ui_list)+1]] = br()
    ui_list[[length(ui_list)+1]] = br()
    
    
    return(ui_list)
  })
  
  #solves the DIVOC Shipping Analysis problem using simplex method depending on the given demands, supply, and shipping costs of each plant to each warehouse
  divocResult <- eventReactive(input$divocSolve, {
    plants = c("DEN", "PHO", "DAL")
    warehouses = c("SAC", "SL", "ALB", "CHI", "NYC")
    
    demands = vector()
    for(i in warehouses){
      dem_val = paste0("input$", i)
      dem_val = eval(parse(text = dem_val))
      demands[length(demands)+1] = dem_val
    }
    demands = demands * -1
    
    supply = vector()
    for(i in plants){
      ware_val = paste0("input$",i)
      ware_val = eval(parse(text = ware_val))
      
      supply[length(supply)+1] = ware_val
    }
    
    shipping_costs = vector()
    for(i in warehouses){
      for(j in plants){
        ship_val = paste0("input$",i,"_",j)
        ship_val = eval(parse(text = ship_val))
        shipping_costs[length(shipping_costs)+1] = ship_val
      }
    }
    
    
    initial_tableau = tableau3
    
    
    initial_tableau[nrow(initial_tableau), 1:5] = demands
    initial_tableau[nrow(initial_tableau), 6:8] = supply
    initial_tableau[1:15, ncol(initial_tableau)] = shipping_costs
    return(simplex(initial_tableau, FALSE, TRUE))
  })
  
  #renders the table for the final tableau of the DIVOC Shipping Analysis Problem
  output$divocFinalTableau <- renderTable({
    return((divocResult())$final.tableau)
  },
  rownames = TRUE
  )
  
  #renders the table for the Final Solution of the DIVOC Shipping Analysis Problem
  output$divocFinalSolution <- renderTable({
    return((divocResult())$basic.solution)
  })
  
  #renders the text to show the optimal value of the DIVOC Shipping Analysis Problem
  output$divocOptimalValue <- renderText({
    return(paste0("The minimum cost is: $", (divocResult())$opt.val))
  })
  
  #renders the table for the Shipping Number of PPEs from each plant to each warehouse
  output$divocShippingNum <- renderTable({
    return((divocResult())$shipping.num)
  },
  rownames = TRUE
  )
  
  #renders the UI for the output of the DIVOC Shipping Analysis Problem
  output$divocUI <- renderUI({
    ui_list = vector(mode = "list")
    if(!is.list(divocResult())){
      ui_list[[length(ui_list)+1]] = h3("No Solution")
    }else{
      ui_list[[length(ui_list)+1]] = h1("Results")
      ui_list[[length(ui_list)+1]] = hr()
      ui_list[[length(ui_list)+1]] = h3("Final Tableau")
      ui_list[[length(ui_list)+1]] = tableOutput("divocFinalTableau")
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Final Solution")
      ui_list[[length(ui_list)+1]] = tableOutput("divocFinalSolution")
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Optimal Value")
      ui_list[[length(ui_list)+1]] = h4(textOutput("divocOptimalValue"))
      ui_list[[length(ui_list)+1]] = br()
      ui_list[[length(ui_list)+1]] = h3("Shipping Number of PPE from each Plant to each Warehouse")
      ui_list[[length(ui_list)+1]] = tableOutput("divocShippingNum")
    }
    return(ui_list)
  })
  
  
  
  
  
  
})