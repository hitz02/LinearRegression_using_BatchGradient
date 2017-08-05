housing <- read.csv(file = choose.files(),header = T)

dim(housing)
str(housing)

#Assigning column names
colnames(housing) <- c('area', 'bedrooms', 'price')

#Normalizing the features
area_scaled <- (housing$area - mean(housing$area))/sd(housing$area)
price_scaled <- (housing$price - mean(housing$price))/sd(housing$price)
bedroom_scaled <- (housing$bedrooms - mean(housing$bedrooms))/sd(housing$bedrooms)

#Combing the features and giving x0 values 1
housing_x <- cbind(rep(1, length(housing$area)), area_scaled, bedroom_scaled)
#housing_y <- price_scaled
housing_y <- housing$price

# housing_grad <- function(x, y, theta) {
#   gradient <- (t(x) %*% ((x %*% t(theta)) - y))
#   return(t(gradient))
# }

#Gradient Descent Computation
grad_descent_housing <- function(x,y,m,alpha)
{
  theta <- matrix(c(0,0,0), nrow = 1)
  cost <- vector()
  d = rep(NA, m*3)
  theta_mat <- matrix(d,nrow = m, ncol = 3) #Creating m*3 NA matrix
  
  for(i in 1:m)
  {
    theta[1] <- theta[1] - alpha * sum((x %*% t(theta)) - y)  #theta[1] is actually theta[0]
    theta[2] <- theta[2] - alpha * sum(((x %*% t(theta)) - y) %*% t(x[,2]))
    theta[3] <- theta[3] - alpha * sum(((x %*% t(theta)) - y) %*% t(x[,3]))
    cost[i] <- (sum(((x %*% t(theta)) - y) * ((x %*% t(theta)) - y)) * 0.5) 
    #cost[i] <- (sum(((x %*% t(theta)) - y)^2) * 0.5) 
   theta_mat[i,] <- c(theta[1],theta[2],theta[3])
  }
  return(cbind(theta_mat,cost))
}

#Calling Gradient Descent function with 3 iterations and alpha = 0.0001
(housing_g = grad_descent_housing(housing_x,housing_y,3,0.001))

#Plotting iteration vs cost  function
plot(0:2,housing_g[,4])


#Solution using closed form equation
solve(t(housing_x)%*%housing_x)%*%t(housing_x)%*%housing_y
