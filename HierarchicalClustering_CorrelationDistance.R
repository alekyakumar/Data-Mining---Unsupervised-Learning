data <- primate.scapulae

#install.packages("caret")
#library(caret)
data$gamma[is.na(data$gamma)] <- mean(data$gamma, na.rm = T)
#View(data)

hc.complete <- hclust(dist(data),method="complete")
x11()
plot(hc.complete, main = "Complete Linkage") #Complete Linkage = 5
hc.complete <- cutree(hc.complete ,5)


hc.single <- hclust(dist(data),method="single")
x11()
plot(hc.single, main = "Single Linkage") #Complete Linkage = 5
hc.single <- cutree(hc.single ,3)

hc.average <- hclust(dist(data),method="average")
x11()
plot(hc.average, main = "Average Linkage") #Complete Linkage = 5
hc.average <- cutree(hc.average ,6)

table((hc.complete),data$classdigit)
table((hc.single),data$classdigit)
table((hc.average),data$classdigit)


#b) K Means clustering

data$class <- as.numeric(as.factor(data$class))
head(data)
pcr.out <- prcomp(data[,1:10])
plot(pcr.out, type = "line",main = "Plot to determine K value" )
#biplot(pcr.out,scale = 0)

# Clustering with K = 2
km.out <- kmeans(data,2,nstart = 20)
km.out$cluster

levels(as.factor(km.out$cluster))

table(km.out$cluster,data$classdigit)
