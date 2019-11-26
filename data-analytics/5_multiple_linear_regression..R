ad = read.csv("data/Advertising.csv")

mlr <- function(x, y) {
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  y_hat = x %*% beta_hat
  
  X = x - mean(x)
  Y = y - mean(y)
  
  e = y - y_hat
  rss = sum(e^2)
  tss = sum(Y^2)
  
  R_squared = 1 - rss/tss
  
  RSE = sqrt(rss / (length(x) - 2))
  
  # corr = sum (X * Y) / sqrt( sum(X^2) *  sum(Y^2) )
  cat(rss, '\n', tss, '\n', R_squared, '\n', RSE, '\n')
}

X = as.matrix(cbind(1, ad$TV, ad$radio, ad$newspaper))
y = as.matrix(ad$sales)
beta_hat = mlr(X, y)

mod1 <- lm(sales ~ ., data = ad)
?"%*%"
