budget = c(300, 150, 100, 150)
sales = c(20, 15, 10, 23)

test = c(200, 500)

lin_reg <- function(x, y, test) {
    x_bar = mean(x)
    y_bar = mean(y)
    X = x - x_bar
    Y = y - y_bar
    
    B1 = sum(X * Y) / sum(X**2)
    B0 = y_bar - B1 * x_bar
    
    y_hat = B0 + B1 * x
    
    rss = sum( (y - y_hat) ** 2 )
    tss = sum(Y**2)
    
    R_squared = 1 - rss/tss
    
    RSE = sqrt(rss / (length(x) - 2))
    
    corr = sum (X * Y) / sqrt( sum(X**2) *  sum(Y**2) )
    cat(R_squared, '\n', B0 + B1 * test, '\n', RSE, '\n', corr)
  }

lin_reg(budget, sales, test)