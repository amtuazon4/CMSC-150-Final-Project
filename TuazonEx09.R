#Name:              Andre M. Tuazon
#Student Number:    2020-00839
#Section:           AB1L

#Exercise 9


#function for simplex method
#parameters:
#1. Tableau -> matrix that represent the initial tableau of a problem
#2. isMax -> boolean value that when true will implement maximization but when false, implements minimization
#3. problem -> boolean value that when true will return value d to the value to be returned but when false, does not include return value d

simplex <- function(tableau, isMax, problem){
  
  
  #Do the following code when problem is TRUE, case for the problem in the pdf
  if(problem==TRUE){
    
    #returns NA if isMax is TRUE since the main goal of the problem in the pdf is to minimize the total shipping cost
    if(isMax == TRUE){
      return(NA)
    }
    
    #performs the minimization simplex method which is coded below this section
    result = simplex(tableau, FALSE, FALSE)
    
    #returns NA when result is NA
    if(is.na(result)[1]){
      return(NA)
    }
    
    #gets the shipping number of each plant to each warehouse and puts it in a matrix where
    #DEN = Denver, Colorado, PHO = Phoenix, Arizona, DAL = Dallas, Texas, SAC = Sacramento, California, SL = Salt Lake City, Utah, ALB = Albuquerque, New Mexico,
    #CHI = Chicago, Illinois, and NYC = New York City
    result_num =  result$basic.solution
    shipping.num = matrix(0, nrow = 3, ncol = 5, dimnames = list(c("DEN", "PHO", "DAL"), c("SAC", "SL", "ALB","CHI", "NYC")))
    for(i in 1:3){
      for(j in 0:4){
        shipping.num[i,(j+1)] = result_num[8+i+(3*j)]
      }
    }
    #gets the final tableau, basic solution, optimal value, and shipping number computation earlier, then stores them in a list named new_list
    new_list = list(final.tableau = result$final.tableau, basic.solution = result$basic.solution, opt.val = result$opt.val, shipping.num = shipping.num)
    
    #returns new_list
    return(new_list)
    
  }
  
  
  #Case for Maximization simplex method problems, occurs when isMax is TRUE and problem is FALSE 
  if(isMax == TRUE && problem == FALSE){
    
    #loop for solving the given initial tableau
    while(min(tableau[nrow(tableau),])<0){
      
      #gets the vector and the column index of the pivot column
      bot_row = tableau[nrow(tableau),]
      target = min(bot_row)
      PC = which(bot_row==target)[1]
      pc_vec = tableau[,PC]
        
      #solves for the test ratio of each row of the tableau
      solution_col = tableau[,ncol(tableau)]
      TR = solution_col/pc_vec
      
      
      #collects the positive test ratios among the test ratios calculated earlier
      selection = vector()
      for(i in TR){
        if(i>0 && i != Inf && !(is.nan(i))){
          selection[length(selection)+1] = i
        }
      }
      
      #when there no positive test ratio among the test ratios calculated earlier, returns NA and prints "No feasible solution"
      if(length(selection)==0){
        print("No feasible Solution")
        return(NA)
      }
      
      #gets the smallest positive test ratio, the row index of the pivot row, and the pivot element      
      minPosTR = min(selection)
      PR = which(TR==minPosTR)[1]
      PE = tableau[PR,PC]
      
            
      #Normalization
      nPR = tableau[PR,] / PE
      tableau[PR,] = nPR

      #elimination of other rows
      for(i in 1:nrow(tableau)){
        if(i == PR){
          next
        }
        c = tableau[i, PC]
        tableau[i, ] = tableau[i,] - (nPR * c)
        
      }
    }
    
    #gets dimension names needed for the basic.solution
    dimnames2 = vector()
    for(i in 1:(ncol(tableau)-1)){
      dimnames2[i] = dimnames(tableau)[[2]][i]
    }
    
    #gets the matrix of the basic solution of the tableau
    basic.solution = matrix(0, nrow = 1, ncol = (ncol(tableau)-1), dimnames = list(c(1),dimnames2))
    for(i in 1:ncol(tableau)){
      selection2 = vector()
      for(j in tableau[,i]){
        if(j>0){
          selection2[length(selection2)+1] = j
        }
      }
      
      if(length(selection2)==1 && selection2[1] == 1){
        target = which(tableau[,i]==1)
        ans = tableau[target,ncol(tableau)]
        basic.solution[1, i] = ans
      }
    }
    
    
    #gets the final tableau, basic solution, and the optimal value, then stores in a list named new_list
    new_list = list(final.tableau = tableau, basic.solution = basic.solution, opt.value = basic.solution[ncol(basic.solution)])
    
    #returns new_list
    return(new_list)
    
  }
  
  #Case for Minimization simplex method problems, occurs when isMax is FALSE and problem is FALSE 
  if(isMax == FALSE && problem == FALSE){
    
    #loop for solving the given initial tableau
    while(min(tableau[nrow(tableau),])<0){
      
      #gets the vector and the column index of the pivot column
      bot_row = tableau[nrow(tableau),]
      target = min(bot_row)
      PC = which(bot_row==target)[1]
      pc_vec = tableau[,PC]
      
      #solves for the test ratio of each row of the tableau
      solution_col = tableau[,ncol(tableau)]
      TR = solution_col/pc_vec
      
      #collects the positive test ratios among the test ratios calculated earlier
      selection = vector()
      for(i in TR){
        if(i>0 && i != Inf && !(is.nan(i))){
          selection[length(selection)+1] = i
        }
      }
      
      #when there no positive test ratio among the test ratios calculated earlier, returns NA and prints "No feasible solution"
      if(length(selection)==0){
        # print("No feasible Solution")
        return(NA)
      }
      
      #gets the smallest positive test ratio, the row index of the pivot row, and the pivot element
      minPosTR = min(selection)
      PR = which(TR==minPosTR)[1]
      PE = tableau[PR,PC]
      
      
      #Normalization
      nPR = tableau[PR,] / PE
      tableau[PR,] = nPR
      
      #elimination of other rows
      for(i in 1:nrow(tableau)){
        if(i == PR){
          next
        }
        c = tableau[i, PC]
        tableau[i, ] = tableau[i,] - (nPR * c)
        
      }
    }
    
    #gets the bottom row and the dimension column names of the tableau
    vec = c(tableau[nrow(tableau), 1:(ncol(tableau)-1)])
    col_names = dimnames(tableau)[[2]]
    new_col_names = vector()
    for(i in 1:(length(col_names)-1)){
      new_col_names[i] = col_names[i]
    }
  
    #gets the basic solution of the tableau and stores it in a matrix
    basic.solution = matrix(vec, nrow=1, ncol=(ncol(tableau)-1), dimnames = list(c(1), new_col_names))
    if(basic.solution[1,ncol(basic.solution)]==1){
      basic.solution[1,ncol(basic.solution)] = tableau[nrow(tableau), ncol(tableau)]
    }
    
    #gets the final tableau, basic solution, and the optimal value, then stores in a list named new_list
    new_list = list(final.tableau = tableau, basic.solution = basic.solution, opt.val = basic.solution[1, ncol(basic.solution)])
    
    #returns new_list
    return(new_list)
    
  }
  
  
  
  
}

# cn refers to the constraints where
# ex. c1 = 1st constraint
#     c2 = 2nd constraint
#     cn = nth constraint

# sn refers to the slack variable where
# ex. s1 = slack variable 1
#     s2 = slack variable 2
#     sn = slack variable n

# xn refers to the variables where
# ex. x1 = variable 1
#     x2 = variable 2
#     xn = variable n

#Z refers to the function to be optimized, solution refers to the solution column, obj func refers to the objective function
rownames = c("c1", "c2", "c3", "c4", "obj func")
colnames = c("x1", "x2", "s1", "s2", "s3", "s4", "Z", "solution")
tableau = matrix(NA, nrow = 5, ncol = 8, dimnames = list(rownames, colnames))

r1 = c(7, 11, 1, 0, 0, 0, 0, 77)
r2 = c(10, 8, 0, 1, 0, 0, 0, 80)
r3 = c(1, 0, 0, 0, 1, 0, 0, 9)
r4 = c(0, 1, 0, 0, 0, 1, 0, 6)
r5 = c(-150, -175, 0, 0, 0, 0, 1, 0)

tableau[1,] = r1
tableau[2,] = r2
tableau[3,] = r3
tableau[4,] = r4
tableau[5,] = r5


y = simplex(tableau, TRUE, FALSE)

# print(y)

rownames = c("c1", "c2", "obj func")
colnames = c( "s1", "s2", "x1", "x2", "Z", "solution")
tableau2 = matrix(NA, nrow = 3, ncol = 6, dimnames = list(rownames, colnames))

r1 = c(1, 7, 1, 0,0, 14)
r2 = c(2, 6, 0, 1, 0, 20)
r3 = c(-4, -20, 0, 0, 1, 0)



tableau2[1,] = r1
tableau2[2,] = r2
tableau2[3,] = r3

z = simplex(tableau2, FALSE, FALSE)

# print(z)


#----------------------------------------------------------------------------

#Setting up the Initial Tableau for the problem in the pdf
tableau3 = matrix(0, nrow=9, ncol=16)

tableau3[1, 1:3] = 1
tableau3[1, ncol(tableau3)] = 180

tableau3[2, 4:6] = 1
tableau3[2, ncol(tableau3)] = 80

tableau3[3, 7:9] = 1
tableau3[3, ncol(tableau3)] = 200

tableau3[4, 10:12] = 1
tableau3[4, ncol(tableau3)] = 160

tableau3[5, 13:15] = 1
tableau3[5, ncol(tableau3)] = 220

for(i in 0:4){
  tableau3[6, (1+(i*3))] = -1
  tableau3[7, (2+(i*3))] = -1
  tableau3[8, (3+(i*3))] = -1
}

tableau3[(6:8), ncol(tableau3)] = c(-310, -260, -280)

tableau3[nrow(tableau3),] = c(10,6,3, 8,5,4, 6,4,5, 5,3,5, 4,6,9, 1)


tableau3 = t(tableau3)

solution_col = tableau3[,ncol(tableau3)]

tableau3 = tableau3[,-9]

tableau3[nrow(tableau3),] = tableau3[nrow(tableau3),]* -1

for(i in 1:16){
  tableau3 = cbind(tableau3, c(0))
  tableau3[i, (i+8)] = 1
}

# the different acronyms in the dimension names below refer to the warehouses in the problem, namely:
#SAC = Sacramento, California, SL = Salt Lake City, Utah, ALB = Albuquerque, New Mexico,
#CHI = Chicago, Illinois, and NYC = New York City

#Furthermore, the corresponding number attached to the acronyms refers to plants where the PPE equipment came from, namely:
#   1 =  Denver, Colorado
#   2 =  Phoenix, Arizona
#   3 =  Dallas, Texas
tableau3 = cbind(tableau3, solution_col)
tableau3[nrow(tableau3), ncol(tableau3)] = 0
row_names = c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12","r13","r14","r15","obj func")
col_names = c("s1","s2","s3","s4","s5","s6","s7","s8", "SAC1","SAC2","SAC3","SL1","SL2","SL3","ALB1","ALB2","ALB3","CHI1","CHI2","CHI3","NYC1","NYC2","NYC3", "Z", "Solution")
dimnames(tableau3) = list(row_names, col_names)

#For modifying the demand, supply, and shipping costs

#Test Case 2
# demand = c(100, 100, 100, 100, 100)
# supply = c(200, 200, 200)
# shipping_costs = c(5,6,3, 6,7,5, 7,8,7, 8,9,11, 9,10,13)
# demand = demand * -1


#Test Case 3
# demand = c(431, 332, 350, 450, 400)
# supply = c(1400, 400, 200)
# shipping_costs = c(30,26,11, 29,24,13, 31,23,15, 35,25,20, 33,27,17)
# demand = demand * -1

#Test Case 4
# demand = c(20, 20, 20, 20, 20)
# supply = c(100, 100, 100)
# shipping_costs = c(5,5,5, 5,5,5, 5,5,5, 5,5,5, 5,5,5)
# demand = demand * -1

#Test Case 5
demand = c(20, 25, 90, 60, 70)
supply = c(50, 50, 50)
shipping_costs = c(30,26,11, 29,24,13, 31,23,15, 35,25,20, 33,27,17)
demand = demand * -1



#modifying the initial tableau based on the given demand, supply, and shipping costs above
# tableau3[nrow(tableau3), 1:5] = demand
# tableau3[nrow(tableau3), 6:8] = supply
# tableau3[1:15, ncol(tableau3)] = shipping_costs

# print(tableau3)
w = simplex(tableau3, FALSE, TRUE)
print(w)

