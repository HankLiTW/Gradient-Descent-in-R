#derivative (This section demonstrates how to do derivative in R)
library(stats) #import package "stats"
f1.exp <- expression(x^2 + 3*x +1) # define the function we want to differentiate
dx1 <- D(f1.exp,"x") # first derivative
x <- 1:5 #define x
eval(dx1) #get value
dx2 <- D(dx1, "x") # second derivative
eval(dx2) #get value
#partial derivative
f2.exp <- expression(x^2*y+y^2) #define the function we want to differentiate
dx1 <- D(f2.exp,"x") #first partial derivative with respect to x
dy1 <- D(f2.exp,"y") #first partial derivative with respect to y
x <- 1:5 #define x
y <- 2:6 #define y
eval(dx1) #get value
eval(dy1)
#if length(x)/length(y) == integer
x <- 1:5
y <- 1:10
eval(dx1) #calculable
#if length(x)/length(y) != integer
eval(dy1)
x <- 1:5
y <- 2:5
eval(dx1)#calculable, but a warning message appears
eval(dy1)
#the shoter vector will repeat
######################################################################################################
#gradient descent (This section displays how to implement gradient descent in R)
#2-dimentional case
#find the minimum value of f(x) = 5*x^6-3*x^3-2*x^2+x-10
f <- expression(5*x^6-3*x^3-2*x^2+x-10) #define function
# define gradientdescent function, where gamma is our learning rate(0 to 1 usually), and iter is
# how many times we try to find our minimum.
gradientDesc_D2 <- function(f,initial_value,gamma,iter)
{
  l <- gamma
  dx <- D(f,"x") #find derivative
  result <- numeric(length = length(initial_value)) #create a vector to store results
  x_ans <- numeric(length = length(initial_value)) #create a vector to store x
  for (i in 1:length(initial_value)) { 
    x <- initial_value[i] #take initial x
    for (j in 1:iter) {
      x <- x-l*eval(dx) #find min x
    }
    result[i] <- eval(f)
    x_ans[i] <- x
  }
  ans <- min(result) #find min value
  x <- unique(round(x_ans[grep(ans,result)],10)) #find x value when the function is minimized
  if (is.nan(ans) == TRUE){
    print("The minimum of the function does not exist")
  }else{
    paste("The minimum of the function is",ans,"at postion x =", x , sep = " ")
  }
  
}
#test
set.seed(100)
x <- runif(3,0,1)
gradientDesc_D2(f,x,0.12,500)
#check
fx <- function(x){
  return(5*x^6-3*x^3-2*x^2+x-10)
}
curve(fx,-1,1)
#we find that Gradient Descent only found local minimum
#try to adjust our learning rate
gradientDesc_D2(f,x,0.0012,500)
#we find the global minimum
#we can alter any parameter to find global minimum
#3-dimentional case
#find the minimium of f(x,y) = (x+y)^2-(x+y)-2
f <- expression((x+y)^2-(x+y)-2)
gradientDesc_D3 <- function(f,initial_value_x,initial_value_y,gamma,iter)
{
  l <- gamma
  dx <- D(f,"x") #find partial derivative with respect to x
  dy <- D(f,"y") #find partial derivative with respect to y
  result <- numeric(length = length(initial_value_x)) #create a vector to store results
  x_ans <- numeric(length = length(initial_value_x)) #create a vector to store x
  y_ans <- numeric(length = length(initial_value_y))
  for (i in 1:length(initial_value_x)) { 
    x <- initial_value_x[i] #take initial x
    y <- initial_value_y[i] #take initial y
    for (j in 1:iter) {
      x_new <- x - l*eval(dx) #store new-found x temporarily
      y <- y - l*eval(dy) #find min x and min y
      x <- x_new
    }
    result[i] <- eval(f)
    x_ans[i] <- x
    y_ans[i] <- y
  }
  ans <- min(result) #find min value
  x <- x_ans[grep(ans,result)]
  y <- y_ans[grep(ans,result)]#find x value when the function is minimized 
  if (is.nan(ans) == TRUE){
    print("The minimum of the function does not exist")
  }else{
    paste("The minimum of the function is",ans,"at postion x =", x ,", y =",y, sep = " ")
  }
  
}
#test
set.seed(10)
x <- runif(10,-1,1)
y <- runif(10,-1,1)
gradientDesc_D3(f,x,y,0.2,50000) #minimum at several (x,y)