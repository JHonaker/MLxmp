# Least Squares Regression Algorithm

# The Least Squares method is a supervised learning algorithm that attempts to
# find a vector B such that the residual sum of squares:
# \sum^N_{i=1}{y_i - x^T_i B}
# 
# This technique forms the basis for the study of linear models

# First we define the residual sum of squares as:
RSS <- function (y, X, B) {
    t(y - X %*% B) %*% (y - X %*% B)
}

# If we differentiate with respect to B, we get:
# t(x)*(y-xB) = 0 for minimization

# If t(x)*x is nonsingular (i.e. invertable), we get a unique solution given by
beta_hat <- function (y, x) {
    if (!is.matrix(x))
        x <- as.matrix(x)

    # Add 1 to the left of the x matrix for estimation of the constant term
    x <- cbind(rep(1, nrow(x)), x)

    # solve gives the unique inverse of the matrix given.
    xTx <- t(x) %*% x
    
    # We can extend this to singular matricies by using the 
    # Moore-Penrose pseudoinverse instead of the traditional inverse
    # and then using the minimum norm solution as the inverse
    if (det(xTx) == 0) stop('The matrix t(x)*x is singular!')
    
    solve(t(x) %*% x) %*% t(x) %*% y
}
