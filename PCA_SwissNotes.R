library(ISLR)
data(marketing)
library(e1071)
library(leaps)
library(caret)
library(car)
library(ggplot2)
library(rpart)
library(randomForest)
library(geneplotter)
library(gbm)
library(ElemStatLearn)

?marketing
summary(marketing)
names(marketing)
attach(marketing)
train_set = as.data.frame(marketing)
train_set["class"] = 1
reference_set = train_set[sample(nrow(train_set)),]
reference_set["class"] = 0

# Generating Classification tree
# for train_set

model.control = rpart.control(maxdepth = 4,minsplit = 5, xval = 10, cp =0)
fit = rpart(Income~., data = train_set, method = "class", control = model.control)
summary(fit)
x11()
plot(fit, uniform = TRUE, compress = TRUE, margin = 0.1, main = "Classification Tree for Train Dataset")
text(fit, use.n = TRUE)

# Generating Classification tree
# for reference_set

model.control = rpart.control(minsplit = 5, xval = 10, cp =0, maxdepth = 4)
fit1 = rpart(Income~., data = reference_set, method = "class", control = model.control)
summary(fit1)
x11()
plot(fit1, uniform = TRUE, compress = TRUE, margin = 0.1, main = "Classification Tree for Reference Dataset")
text(fit1, use.n = TRUE)