install.packages("magrittr")
library(magrittr)
install.packages("ElemStatLearn")
install.packages("arules")
library("arules")
install.packages("MASS")
library(ElemStatLearn)
library(MASS)
install.packages("dplyr")
library(dplyr)
boston <- subset(Boston)
head(Boston)

#Histograms
x11()
#Boston <- as.matrix(Boston)


x11()
par(mfrow = c(4,4))
hist(boston$crim)
hist(boston$zn)
hist(boston$indus)
hist(boston$chas)
hist(boston$nox)
hist(boston$rm)
hist(boston$age)
hist(boston$dis)
hist(boston$rad)
hist(boston$tax)
hist(boston$ptratio)
hist(boston$black)
hist(boston$lstat)
hist(boston$medv)

#boston$chas <- factor(boston$chas, labels = c("river","non-river"))
#boston$rad <- factor(boston$rad)
#boston$black <- cut(boston$black, breaks = 4, labels=c(">31.5%","18.5-31.5%","8-18.5%","<8%"))
#disc <- function(x) cut(x,breaks = 4, labels = c("low","medLow","medHigh","high"))
#boston <- select(boston, -one_of(c("chas","rad","black"))) %>%
          #mutate_at(.vars = c(1:4),.funs = disc)  %>%
          #bind_cols(select(boston,one_of(c("chas","rad","black")))) 
boston[["age"]] = ordered(cut(boston[["age"]], c(0, 20, 40, 60, 100)), labels = c("Teenager", "Young", "Middle-aged", "Senior"))
boston[["rm"]] = ordered(cut(boston[["rm"]], c(3,5,6,9)), labels = c("few","average","more"))
boston[["dis"]] = ordered(cut(boston[["dis"]], c(2,4,6,8)), labels = c("less", "average", "long"))
boston[["medv"]] = ordered(cut(boston[["medv"]], c(0, 15, 25, 50)), labels = c("low", "Middle", "High"))
boston[["lstat"]] = ordered(cut(boston[["lstat"]], c(0, 10, 20, 40)), labels = c("low", "Medium", "High"))
boston[["ptratio"]] = ordered(cut(boston[["ptratio"]], c(12, 15, 18,22)), labels = c("low","Medium","High"))
boston[["tax"]] = ordered(cut(boston[["tax"]], c(0, 300, 500, 800)), labels = c("Low", "Moderate", "High"))
boston[["crim"]] = ordered(cut(boston[["crim"]], c(0, 10 , 25 , 40)), labels = c("Low", "Medium", "High"))
boston[["chas"]] = factor(boston$chas, labels = c("river","no river"))



boston[["zn"]] = NULL
boston[["nox"]] = NULL
boston[["indus"]] = NULL
boston[["black"]] = NULL
boston[["rad"]] = NULL


#Converting into Binary Incidence Matrix
boston_matrix <- as(boston, "transactions")

summary(boston_matrix)

#Visualise using ItemFrequencyPlot 
x11()
itemFrequencyPlot(boston_matrix, support = 0, cex.names = 0.8)
x11()
itemFrequencyPlot(boston_matrix, support = 0.07, cex.names = 0.8)
                           
rules <- apriori(boston_matrix, parameter = list(support = 0.01, confidence = 0.6))
summary(rules)

#############C part###############
### Low Crime Area
### Measured by Distance

rules<-apriori(boston_matrix, parameter=list(support=0.01,confidence=0.6),appearance = list(lhs=c("dis=less"),default="rhs"))

rulesLowCrime <- subset(rules, subset = rhs %in% "crim=Low" & lift>1)
summary(rulesLowCrime)
inspect(head(sort(rulesLowCrime,by="confidence"),n=10))

######D Part#############
rules<-apriori(boston_matrix, parameter=list(support=0.01,confidence=0.6))
rulesLowPTRatio <- subset(rules, subset = rhs %in% "ptratio=low" & lift>1)
summary(rulesLowPTRatio)
inspect(head(sort(rulesLowPTRatio,by="confidence"),n=10))
