# Linear Regression:
# Through Gradient Descent
# Linear regression through gradient descent is almost the exact
# same as logistic regression.
# The only differences are the hypothesis and cost functions.

identity <- function(x)  { x }

cost <- function(z, y, h=identity) {
	(z - y)^2    
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
dJ <- function(theta, X, Y, h=identity) {
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
y <- z + rnorm(sz)

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