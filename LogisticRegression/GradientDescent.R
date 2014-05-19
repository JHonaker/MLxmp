# Logistic Regression:
# Through Gradient Descent

sigmoid <- function(x) 1 / (1 + exp(-x))

cost <- function(z, y, h=sigmoid) {
  # This creates a convex function that we can optimize over. 
  # It can be derived using maximum likelihood estimation.
  -(y * log(h(z)) + (1-y) * log(1 - h(z)))
}

J <- function(theta, X, Y) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.matrix(theta)) theta <- as.matrix(theta)
  if (!is.matrix(Y)) Y <- as.matrix(Y)
  
  1 / nrow(X) * sum(cost(X %*% theta , Y))
}

# To fit the regression coefficients we must minimize the cost, J(theta: (X,Y))
# In order to do this, we need to simultaneously update all theta_j
# values like so: theta_j := theta_j - alpha * dJ(theta)/dtheta_j
# where alpha is the specified "learning rate".

# To do this we will need the derivative of J(theta) wrt. theta_j
dJ <- function(theta, X, Y) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.matrix(theta)) theta <- as.matrix(theta)
  if (!is.matrix(Y)) Y <- as.matrix(Y)
  
  1 / nrow(X) * t(X) %*% cost(X %*% theta , Y)
}

update <- function(theta, X, Y, alpha=0.05) {
  theta <- theta - alpha * dJ(theta, X, Y)
  theta
}

solution <- matrix(NA, 2, 1000)
solution[,1] <- matrix(1,2,1)
for (i in 2:1000) {
  solution[,i] <- update(solution[,i-1], x, y)
}