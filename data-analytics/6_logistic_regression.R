# https://stats.stackexchange.com/questions/220304/manually-calculating-logistic-regression-coefficient
# https://www.analyticsvidhya.com/blog/2015/10/basics-logistic-regression/

# likelihood function
log_likelihood <- function(y, pi) {
        ll_units <- ifelse(y == 1, pi, 1 - pi)
        print(ll_units)
        return (prod(ll_units))
}

sigmoid <- function(z) {
        return (1 / (1 + exp(-z)))
}

find_pi <- function(x_new, beta) {
        pi <- sigmoid(x_new %*% beta)
        return (as.vector(pi))
}

logistic <- function(x, y, k, obs, dif) {
        
        # k: number of predictor variables
        # obs: sample size / number of observations
        
        # create zero vector of length, k
        beta <- rep(0, k)
        
        diff <- 10^3
        
        # loop until coefficients converge
        while (diff > dif) {
          pi <- find_pi(x, beta)
          W <- diag(pi * (1 - pi))      # diagonal of matrix 𝜋(1-𝜋)
          hessian <- t(x) %*% W %*% x
          gradient <- t(x) %*% (y - pi)
          derivative <- solve(hessian) %*% gradient
          beta = beta + derivative
          diff <- sum(derivative ^ 2)
        }
        # View(t(x_new)%*%W%*%x_new)
        return (beta)
}

library(ISLR)

# transform student column (1, 0) -> (Yes, No)
student <- ifelse(Default$student=="Yes", 1, 0)
X = cbind(student, Default[, 3:4])

# add an column of ones for
# calculating intercept value of logit
X <- as.matrix(cbind(bias = 1, X))

y <- ifelse(Default$default=="Yes", 1, 0)

beta <- logistic(X, y, k = dim(X)[2], obs = dim(X)[1], 1e-9)

mod1 <- glm(y ~ student + balance + income, family=binomial, data=as.data.frame(X))
est <- coefficients(mod1)

pi <- find_pi(X, beta)
ll <- log_likelihood(y[1:10], pi[1:10])
ll
