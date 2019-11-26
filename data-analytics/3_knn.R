# x <- data.frame(
#   Age = c(25, 35, 45, 20, 35, 52, 23, 40, 60, 48, 33),
#   Loan = c(40, 60, 80, 20, 120, 18, 95, 62, 100, 220, 150) * 1000,
#   Default = c(rep('N', 6), rep('Y', 5))
# )
# write.csv(x, file = "data/loan.csv", row.names = FALSE)

KNN <- function(x, ys, default, k, cl = NULL) {
  target = setdiff(names(x), names(ys))
  
  pred = c()
  t = x[,names(ys)]

  for (i in 1:nrow(ys)) {
    x$distance = c()
    y = ys[i,]
    
    for (j in 1:nrow(t)) {
      x$distance[j] <- sqrt(sum( (t[j,] - y) ^ 2 ))
    }
    
    x  = x[order(x$distance),]
    tb = table(x[1:k, target])
    # print(tb)
    class_counts = as.vector(tb)
    
    # number of observations for each class are same
    if (length(unique(class_counts)) == 1) {
      pred = c(pred, default)
    } else {
      tb = tb[order(tb, decreasing = T)]
      pred = c(pred, names(tb)[1])
    }
  }
  return (pred)
}

x <- read.csv("data/loan.csv")

df.size = nrow(x)

set.seed(69)
indices <- sample(x = df.size, size = 0.7 * df.size)
train <- x[indices,]
test  <- x[-indices,]

## test accuracy of KNN
target_classes <- subset(train, select = "Default")
test_classes   <- test$Default

test$Default <- NULL

accuracy <- function(x){sum(diag(x))/sum(x) * 100}

ks  <- 1:nrow(train)
acc <- c()
for (k in ks) {
  pred <- KNN(train, test, 'Y', k, target_classes)
  tab <- table(pred, test_classes)
  acc = c(acc, accuracy(tab))
  print(tab)
  print(acc)
}

plot(ks, acc, type = 'l')


## predict class of y using knn for different k values

# install.packages('ggplot2')
library(class)

y <- data.frame(
  Age = 48,
  Loan = 142000
)

gprint <- function(str) { print(glue::glue(str)) }
separator <- strrep('_', 20)
for(i in 1:11) {
  gprint("{separator}\n
         k = {i}")
  
  pred1 = as.character(knn(train = x[1:2], test = y, cl = x[,3], k = i))
  pred2 = KNN(x, y, default = 'Y', i)
  
  gprint('pre-defined  knn : {pred1}
          user-defined knn : {pred2}')
}
