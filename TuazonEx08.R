#Name:              Andre M. Tuazon
#Student Number:    2020-00839
#Section:           AB1L

#Exercise 8

#function for Quadratic Spline Interpolation where the parameters are
#     data -> list of x and y values, where index 1 is a vector of x-values and index 2 is a vector of y-values
#     x    -> x-value to be approximated by Quadratic Spline Interpolation
poly.qsi <- function(data, x){
  
  
  #getting the values
  ind = data[[1]]
  dep = data[[2]]
  approx = x
  
  #checks if the there are unequal length of x and y values from the data points given,
  #also checks if the given data points are less than 4, since the condition 1 and 3 needs starts at i=2
  #if the conditions above are true, return NA
  
  len_x = length(ind)
  len_y = length(dep)
  
  if(len_x != len_y){
    return(NA)
  } else if(len_x < 4 || len_y < 4){
    return(NA)
  }
  
  
  #Arranging data points based on the x-values in ascending order
  temp_x = ind
  ind = sort(ind)
  numData = length(ind)
  
  new_dep = vector()
  for(i in 1:numData){
    index = which(temp_x==ind[i])
    new_dep[i] = dep[index] 
  }
  dep = new_dep
  
  #getting the number of intervals
  n = numData - 1
  
  #stores coefficients of each variable in a vector in this way
  # c(b1, c1, a2, b2, c2, ... , an, bn, cn)
  #where    b1 -> coefficient of b1
  #         c1 -> coefficient of c1
  #         a2 -> coefficient of a2
  #         ....
  #         an -> coefficient of an
  #         bn -> coefficient of bn
  #         cn -> coefficient of cn
  
  #vector to be used to store the pattern above
  base_vector = c(1:(3*n)) * 0
  
  #matrix to store all inputted data
  data_inputted = matrix(data = 0, nrow = 1, ncol =(3*n))
  
  
  
  
  #getting the coefficients from the equations of condition 1 
  #i = 2 to n (zero based so +1 since r is 1 based indexing)
  for(i in 3:(n+1)){
    target_index = i-1
    
    if(i == 3){
      b1 = ind[target_index]
      c1 = 1
      fx1 = dep[target_index]
      
      eq1 = base_vector
      eq1[1:2] = c(b1, c1)
      eq1[3*n] = fx1

      a2 = b1^2
      b2 = b1
      c2 = 1
      fx2 = fx1
      
      eq2 = base_vector
      eq2[3:5] = c(a2, b2, c2)
      eq2[3*n] = fx2
      
      data_inputted[1,] = eq1
      data_inputted = rbind(data_inputted, eq2, deparse.level = 0)

    } else{
      an1 = (ind[target_index])^2 
      bn1 = ind[target_index]
      cn1 = 1
      fxn1 = dep[target_index]
      
      an2 = an1
      bn2 = bn1
      cn2 = cn1
      fxn2 = fxn1
      
      eq1 = base_vector
      eq2 = base_vector

      starting_index1 = (i-3)*3
      starting_index2 = (i-2)*3
      
      eq1[starting_index1:(starting_index1+2)] = c(an1,bn1,cn1)
      eq1[3*n] = fxn1
      
      eq2[starting_index2:(starting_index2+2)] = c(an2,bn2,cn2)
      eq2[3*n] = fxn2
      
      data_inputted = rbind(data_inputted, eq1, eq2, deparse.level = 0)
    }
  
  }
  
  #getting the coefficients from the equations of condition 2
  
  b1 = ind[1]
  c1 = 1
  fx1 = dep[1]
  
  an = (ind[n+1])^2
  bn = ind[n+1]
  cn = 1
  fxn = dep[n+1]
  
  eq1 = base_vector
  eqn = base_vector
  
  eq1[1:2] = c(b1, c1)
  eq1[n*3] = fx1
  
  eqn[(3*n-3):(3*n)] = c(an, bn, cn, fxn)
  
  data_inputted = rbind(data_inputted, eq1, eqn, deparse.level = 0)
  
  
  
  #getting the coefficients from the equations of condition 3
  
  for(i in 3:(n+1)){
    target_index = i-1
    
    an_minus = 2*ind[target_index]
    bn_minus = 1
    an = (-1)*an_minus
    bn = -1
    
    eq = base_vector
    if(i==3){
      eq[1] = bn_minus
      eq[3:4] = c(an, bn)
    }else{
      starting_index1 = (i-3)*3
      starting_index2 = (i-2)*3
      eq[(starting_index1):(starting_index1+1)] = c(an_minus, bn_minus)
      eq[starting_index2:(starting_index2+1)] = c(an, bn)
      
    }
    
    data_inputted = rbind(data_inputted, eq, deparse.level = 0)
  }
  
  #Gauss-Jordan Elimination to solve the equations from the conditions above
  n = (3*n-1)
  a  = data_inputted
  
  
  for(i in 1:n){
    
    if(i != n){
      col = a[i:n, i]
      target = max(abs(col))
      if(target==0){
        return(NA)
      }

      index = which(abs(a[,i]) == target)[1]
      
      #swapping
      temp = a[i, ]
      a[i,] = a[index,]
      a[index,] = temp
    }
    
    a[i,] = a[i,]/a[i,i]
    
    for(j in 1:n){
      if(i==j){
        next
      }
      normalized_row = a[j,i] * a[i,]
      a[j,] = a[j,] - normalized_row
    }
    
  }  
  
  x = vector()
  x[n] = a[n,n+1]/a[n,n]
  for(i in (n-1):1){
    x[i] = (a[i,n+1] - sum(a[i, (i+1):n] * x[(i+1):n])) / a[i,i]
  }
  
  #gets the final set of functions and store them in a list
  solution = c(0)
  for(i in x){
    solution[length(solution)+1] = i
  }
  
  eq_num = length(solution)/3
  
  new_list = vector(mode = "list")
  for(i in 1:eq_num){
    the_string = "function (x) "
    starting_index = (i-1)*3 + 1
    
    the_string = paste0(the_string, solution[starting_index], " * x^2 + ", solution[starting_index+1], " * x^1 + ", solution[starting_index+2], ";")
    func = eval.parent(parse(text = the_string))
    
    new_list[[length(new_list)+1]] = func
    
  }
  
  
  #returns NA when x-value to be approximated is out of range
  lowest = ind[1]
  highest = ind[len_x]
  if(approx<lowest || approx > highest ){
    return(NA)
  }
  
  
  #gets the approximated value of y from the x-value to be approximated
  numIntervals = len_x - 1
  y = 0
  for(i in 1:numIntervals){
    if(approx >= ind[i]){
      y = new_list[[i]](approx)
    }
  }
  
  
  #if the approximated y-value is NaN, this code returns NA
  if(is.nan(y)){
    return(NA)
  }
  
  #stores the list of functions and approximated value of y from the x-value to be approximated in a list called answer, then return answer
  answer = list(qsi.fxns = new_list, y = y)
  return(answer)
}


# x = c(5,4,3,2,1)
# y = c(6,7,8,9,10)

# x = c(3.0, 4.5, 7.0, 9.0)
# y = c(2.5, 1.0, 2.5, 0.5)
# 
# x = sort(x, decreasing = TRUE)
# y = c(0.5, 2.5, 1.0, 2.5)
# approx = 5

# x = c(0, 10, 15, 20, 22.5, 30 )
# y = c(0, 227.04, 362.78, 517.35, 602.97, 901.67)
# approx = 16

# data = list(x, y)
# z = poly.qsi(data, approx)
# print(z)


