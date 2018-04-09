#Generate a data set
 
x = matrix(rnorm(20 * 3 * 50 ),ncol = 50)
x[1:20,] = x[1:20,1:ncol(x)] + 1
x[21:40,] = x[21:40,1:ncol(x)] -2
x[41:60,] = x[41:60,1:ncol(x)] + 2

#PCA for the observations
pcr.out <- prcomp(x, scale = TRUE)
plot(pcr.out$x[,1:2], col = 1:3, xlab = "PC1" , ylab = "PC2",pch = 19)
#biplot(pcr.out,scale = 0)

# Clustering with K = 3
km.out <- kmeans(x,3,nstart = 20)
km.out$cluster

levels(as.factor(km.out$cluster))

table(km.out$cluster,c(rep(1,20),rep(2,20),rep(3,20)))

#Clustering with K = 2

km.out2 <- kmeans(x,2,nstart = 20)
km.out2$cluster

levels(as.factor(km.out2$cluster))

table(km.out2$cluster,c(rep(1,20),rep(2,20),rep(3,20)))

#Clustering with K = 4

km.out4 <- kmeans(x,4,nstart = 40)
km.out4$cluster

levels(as.factor(km.out4$cluster))

table(c(rep(1,20),rep(2,20),rep(3,20)),km.out4$cluster)

#K means Clustering woth K = 3 for first 2 PCs
km.out3.pc <- kmeans(pcr.out$x[,1:20], 3 , nstart =20 )

levels(as.factor(km.out3.pc$cluster))

table(c(rep(1,20),rep(2,20),rep(3,20)),km.out3.pc$cluster)

#Clustering with K = 3 and data scaled

km.scaled  <- kmeans(scale(x), 3 , nstart =20 )

levels(as.factor(km.scaled$cluster))

table(c(rep(1,20),rep(2,20),rep(3,20)),km.scaled$cluster)


