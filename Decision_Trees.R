swiss <-SwissBankNotes
boxplot(SwissBankNotes)
swiss_100 <- SwissBankNotes[1:100,]
x11()
boxplot(swiss_100)
swiss_200 <- SwissBankNotes[101:200,]
boxplot(swiss_200)

x11()
par(mfrow = c(2,3))
boxplot(SwissBankNotes$length, main = "Length")
boxplot(SwissBankNotes$height.left, main = "Height - Left")
boxplot(SwissBankNotes$height.right, main = "Height - Right")
boxplot(SwissBankNotes$inner.lower, main = "Inner - Lower" )
boxplot(SwissBankNotes$inner.upper, main = "Inner - Upper")
boxplot(SwissBankNotes$diagonal, main = "Diagonal")

par(mfrow = c(2,3))
boxplot(swiss_100$length, main = "Length")
boxplot(swiss_100$height.left, main = "Height - Left")
boxplot(swiss_100$height.right, main = "Height - Right")
boxplot(swiss_100$inner.lower, main = "Inner - Lower" )
boxplot(swiss_100$inner.upper, main = "Inner - Upper")
boxplot(swiss_100$diagonal, main = "Diagonal")


par(mfrow = c(2,3))
boxplot(swiss_200$length, main = "Length")
boxplot(swiss_200$height.left, main = "Height - Left")
boxplot(swiss_200$height.right, main = "Height - Right")
boxplot(swiss_200$inner.lower, main = "Inner - Lower" )
boxplot(swiss_200$inner.upper, main = "Inner - Upper")
boxplot(swiss_200$diagonal, main = "Diagonal")

#Principal Components
pc_ex1 <- prcomp(SwissBankNotes)
pc_ex2 <- prcomp(swiss_100)
pc_ex3 <- prcomp(swiss_200)

x11()
plot(pc_ex1, main = "All 200 Notes")

x11()
plot(pc_ex2,main = "Genuine Notes")

x11()
plot(pc_ex3, main = "Counterfeit Notes")

summary(pc_ex1)
summary(pc_ex2)
summary(pc_ex3)

x11()
biplot(pc_ex1)
biplot(pc_ex2)
biplot(pc_ex3)


head(pc_ex2$rotation)
head(pc_ex3$rotation)

x11()
biplot(pc_ex1)
biplot(pc_ex2)
biplot(pc_ex2)
