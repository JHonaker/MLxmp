# Logistic Regression:
# Through Gradient Descent

source('../utility.R')

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
dJ <- function(theta, X, Y, h=sigmoid) {
    if (!is.matrix(X)) X <- as.matrix(X)
    if (!is.matrix(theta)) theta <- as.matrix(theta)
    if (!is.matrix(Y)) Y <- as.matrix(Y)
    
    1 / nrow(X) * t(X) %*% (h(X %*% theta) - Y)
}

update <- function(theta, X, Y, alpha=0.05) {
    theta <- theta - alpha * dJ(theta, X, Y)
    theta
}

sz <- 1000
x <- cbind( 1, rnorm(sz))
theta <- c(2, 3)
z <- x %*% theta
pr <- sigmoid(z)
y <- rbinom(sz, 1, pr)

iters <- 10000
solution <- matrix(NA, 2, iters)
Jmat <- rep(NA, iters)
dJmat <- rep(NA, iters)
solution[,1] <- as.matrix(c(1, 1))
Jmat[1] <- J(solution[,1], x, y)
for (i in 2:iters) {
    solution[,i] <- update(solution[,i-1], x, y)
    Jmat[i] <- J(solution[,i], x, y)
}
plot(1:iters, Jmat)