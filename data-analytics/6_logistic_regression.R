library(ISLR)
# ISLR::Default

source("log2.R")
source("log3.R")

# # library(car)
# plot(x=Default$income, y=Default$balance)
# 
# library(corrplot)
# correlations <- cor(X[,2:ncol(X)])
# corrplot(correlations, method="circle")


student <- ifelse(Default$student=="Yes", 1, 0)
X = cbind(student, Default[, 3:4])

y <- ifelse(Default$default=="Yes", 1, 0)

mod1 <- glm(y ~ student + balance + income, family=binomial, data=as.data.frame(X))
# binomial(link = "logit")

est <- coefficients(mod1)
# coef
# summary(mod1)$coefficients[,"Estimate"]

preds1 <- sigmoid(X_ %*% est)>0.5
table(preds1)

source("log1.R")
# ncol

beta <- logistic(X, y, vars=dim(X)[2], obs=dim(X)[1], learningrate=0.01, 0.000000001)

boxplot(balance ~ student,
        data=Default,
        xlab="Student Status",
        ylab="Credit Card Balance",
        col=c("blue","orange"))

x_new <- as.matrix(cbind(bias=1, X))
pi <- find_pi(x_new, beta)
df <- as.data.frame(cbind(pi, bal=Default$balance, student))

cols <- c("pi","bal")
stud <- df[df$student==1,cols]
notstud <- df[df$student==0,cols]

rownames(stud) <- NULL
rownames(notstud) <- NULL
stud <- stud[order(stud$bal),]
notstud <- notstud[order(notstud$bal),]

xrange <- range(df$bal)
yrange <- range(df$pi)
plot(xrange, yrange, type = "n",
     xlab = "Credit Card Balance",
     ylab = "Default Rate")

lines(stud$bal, stud$pi, type = "l", col = "orange")
lines(notstud$bal, notstud$pi, type = "l", col = "blue")
abline(h=c(mean(stud$pi), mean(notstud$pi)),
       col=c("orange", "blue"), lty=2)

ll <- calculate_ll(y, pi)
ll

preds <- predict(mod1, newdata = data,type ="response")
calculate_ll(data$y,preds)



## log3.R
# mod  <- fit_logit(X, y)
# preds = predict.my_logit(mod, X, probs=F)


## log2.R

# beta <- logisticReg(X, y)
# beta
# 
# temp <- na.omit(cbind(y, x))
# X <- mutate(temp[, -1], bias =1)
# X <- as.matrix(X[,c(ncol(X), 1:(ncol(X)-1))])
# 
# # pred
# preds = sigmoid(X %*% beta)>0.5
# 
# # X[,c("bias","student")]


g <- function(x) {exp(x)/(1+exp(x))}
# same as sigmoid

p <- g(X %*% beta)
p1 <- g(X %*% est)

l <- ifelse(y==1, p, 1 - p)
l1 <- ifelse(y==1, p1, 1 - p1)
sum(log(l))


table( abs(p - p1) < 1e-01 )

e <- (X %*% beta)
e1 <- (X %*% est)
View(cbind(e, e1))
table( abs(e - e1) < 1e-01 )