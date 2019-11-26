# https://datascienceplus.com/how-to-perform-logistic-regression-lda-qda-in-r/

library(MASS)
library(ggplot2)
titanicDS = read.csv("data/titanic.csv")
dim(titanicDS)

str(titanicDS)

summary(titanicDS)

attach(titanicDS)
UniqueValue = function (x) {length(unique(x)) }
apply(titanicDS, 2, UniqueValue)

NaValue = function (x) {sum(is.na(x)) }
apply(titanicDS, 2, NaValue)

BlankValue = function (x) {sum(x=="") }
apply(titanicDS, 2, BlankValue)

MissPercentage = function (x) {100 * sum (is.na(x)) / length (x) }
apply(titanicDS, 2, MissPercentage)

titanicDS$Age[is.na(titanicDS$Age)] = mean(titanicDS$Age, na.rm=TRUE)
apply(titanicDS, 2, MissPercentage)

set.seed(1)
row.number = sample(1:nrow(titanicDS), 0.6*nrow(titanicDS))
train = titanicDS[row.number,]
test = titanicDS[-row.number,]
dim(train)
dim(test)

# logistic regression
attach(train)
model1 = glm(factor(Survived)~.-PassengerId-Name-Ticket-Cabin, data=train, family=binomial)
summary(model1)

#Remove Not significant features.
model2 = update(model1, ~.-Parch-Fare-Embarked)
summary(model2)

###Predict for training data and find training accuracy
pred.prob = predict(model2, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, Survived)


##Predict for test Data and find the test accuracy.
attach(test)
pred.prob = predict(model2, newdata= test, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, Survived)



# LDA model
attach(train)
lda.model = lda (factor(Survived)~factor(Pclass)+Sex+Age+SibSp, data=train)
lda.model


##Predicting training results.
predmodel.train.lda = predict(lda.model, data=train)
table(Predicted=predmodel.train.lda$class, Survived=Survived)

ldahist(predmodel.train.lda$x[,1], g= predmodel.train.lda$class)


attach(test)
predmodel.test.lda = predict(lda.model, newdata=test)
table(Predicted=predmodel.test.lda$class, Survived=test$Survived)

par(mfrow=c(1,1))
plot(predmodel.test.lda$x[,1], predmodel.test.lda$class, col=test$Survived+10)



# QDA model

attach(train)
qda.model = qda (factor(Survived)~factor(Pclass)+Sex+Age+SibSp, data=train)
qda.model

##Predicting training results.
predmodel.train.qda = predict(qda.model, data=train)
table(Predicted=predmodel.train.qda$class, Survived=Survived)

##Predicting test results.
attach(test)
predmodel.test.qda = predict(qda.model, newdata=test)
table(Predicted=predmodel.test.qda$class, Survived=test$Survived)

par(mfrow=c(1,1))
plot(predmodel.test.qda$posterior[,2], predmodel.test.qda$class, col=test$Survived+10)


