gene <-read.csv(file="C:/Users/Alekya Kumar/Desktop/Data Sciences - Sem 2/Data Mining/HW2/Ch10Ex11.csv", header=FALSE, sep=",")


#Hierarchical CLustering based on Correlation distance 
hc.complete <- hclust(as.dist(1 - cor(gene)),method="complete")
plot(hc.complete, main = "Complete Linkage")

hc.average <- hclust(as.dist(1 - cor(gene)),method="average")
plot(hc.average, main = "Average Linkage")


hc.single <- hclust(as.dist(1 - cor(gene)),method="single")
plot(hc.single, main = "Single Linkage")

#Genes that differ the most across the two groups 
pca.out <- prcomp(t(gene))
head(pca.out$rotation)

total <- apply(pca.out$rotation, 1, sum)
index <- order(abs(total), decreasing = TRUE)

index[1:20]
