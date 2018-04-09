?USArrests

hc.complete <- hclust(dist(USArrests),method="complete")
plot(hc.complete, main = "Complete Linkage")

#Cutting Dendrogram to determine the clusters
cutree(hc.complete ,3)

scaled_arrest <- scale(USArrests)
sclaed_hc <- hclust(dist(scaled_arrest) ,method = "complete")
plot(hclust(dist(scaled_arrest) ,method = "complete"), main = "Hierarchical CLustering with Scaled features")

#Comparing the tables 
cutree(sclaed_hc ,3)
table(cutree(hc.complete, 3), cutree(sclaed_hc, 3))
