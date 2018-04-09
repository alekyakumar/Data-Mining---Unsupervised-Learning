install.packages("kohonen")
library(kohonen)
install.packages("ElemStatLearn")
library(ElemStatLearn)
install.packages("phyclust")
library(phyclust)
install.packages("CRAN")
library(CRAN)
marray <- data.frame(nci)

marray.scaled <- scale(marray)

#Fit an  SOM for K = 2 
set.seed(1907)
sim_rand = c()
for (i in 1:5){
marray.2 <- supersom(data = marray.scaled, grid = somgrid(2,1,"hexagonal"),radius = i)

plot(marray.2, main = "Micro Array Data")
marray.2$unit.classif

#K means CLustering
km.out <- kmeans(marray.scaled,2,nstart = 20)
km.out$cluster

similarity = RRand(marray.2$unit.classif,km.out$cluster)

sim_rand = c(sim_rand , similarity$Rand)

table(marray.2$unit.classif,km.out$cluster)
}
sim_rand
x11()
plot(sim_rand, main = "Similarity percentage for each radius for K =2",type = "line",xlab = "Radius - 1:5",ylab = "Similarity Percentage")


sim_rand = c()
for (i in 1:5){
  marray.5 <- supersom(data = marray.scaled, grid = somgrid(5,1,"hexagonal"),radius = i)
  #x11()
  plot(marray.5, main = "Micro Array Data")
  marray.5$unit.classif
  
  #K means CLustering
  km.out <- kmeans(marray.scaled,5,nstart = 20)
  km.out$cluster
  
  similarity = RRand(marray.5$unit.classif,km.out$cluster)
  
  sim_rand = c(sim_rand , similarity$Rand)
  
  table(marray.5$unit.classif,km.out$cluster)
}
sim_rand
x11()
plot(sim_rand, main = "Similarity percentage for each radius for K = 5", type = "line",xlab = "Radius - 1:5",ylab = "Similarity Percentage")

sim_rand = c()
for (i in 1:5){
  marray.10 <- supersom(data = marray.scaled, grid = somgrid(10,1,"hexagonal"),radius = i)
  #x11()
  plot(marray.10, main = "Micro Array Data")
  marray.10$unit.classif
  
  #K means CLustering
  km.out <- kmeans(marray.scaled,10,nstart = 20)
  km.out$cluster
  
  similarity = RRand(marray.10$unit.classif,km.out$cluster)
  
  sim_rand = c(sim_rand , similarity$Rand)
  
  table(marray.10$unit.classif,km.out$cluster)
}
sim_rand
x11()
plot(sim_rand, main = "Similarity percentage for each radius for K = 10",type = "line",xlab = "Radius - 1:5",ylab = "Similarity Percentage")
mean(sim_rand)

sim_rand = c()
for (i in 1:5){
  marray.20 <- supersom(data = marray.scaled, grid = somgrid(20,1,"hexagonal"),radius = i)
  x11()
  plot(marray.20, main = "Micro Array Data")
  marray.20$unit.classif
  
  #K means CLustering
  km.out <- kmeans(marray.scaled,20,nstart = 20)
  km.out$cluster
  
  similarity = RRand(marray.20$unit.classif,km.out$cluster)
  
  sim_rand = c(sim_rand , similarity$Rand)
  
  table(marray.20$unit.classif,km.out$cluster)
}
sim_rand
x11()
plot(sim_rand, main = "Similarity percentage for each radius for K = 20",type = "l*",xlab = "Radius - 1:5",ylab = "Similarity Percentage")
mean(sim_rand)




