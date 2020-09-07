#UNSUPERVISED LEARNING........................................................
library(dplyr)
library(factoextra)
library(devtools)
library(rgl)
library(corrplot)
library(caret)

diab<- read.csv("diabetes.csv")

#SUB 0 TO NA
diab2 = diab[,-c(1,9)] %>%
  mutate_all(~replace(., . == 0, NA))

diab =data.frame(Pregnacies = diab$Pregnancies, diab2, Outcome = diab$Outcome)
head(diab)

#Sub NA with median

for(i in 1:ncol(diab)){
  diab[is.na(diab[,i]), i] <- median(diab[,i], na.rm = TRUE)
}
head(diab)

#PCA...................................................
# diab_result = diab[,9]
# diab_result
# 
# diab_pred = diab[,1:8]
# head(diab_pred)

#correlation between variables
corrplot(cor(diab[,1:8]), type = "lower", method = "number")

#PCA WITH THE FIRST TWO COMPONENTS

#2 principal components.........................
pca = princomp(diab[,1:8], cor = TRUE)
summary(pca)

fviz_eig(pca)

plot(pca$scores)
abline(h=0, v=0)

biplot(pca, xlabs=rep("o", nrow(diab)))

#3 principal components...........................
plot3d(pca$scores[,1:3], col = "Red") #, xlabs=rep("o", nrow(diab)), xlim = c(-2,2), ylim=c(-2,2), zlim=c(-2,2))

#Biplot
plot3d(pca$scores[,1:3],xlabs=rep("o", nrow(diab)), xlim = c(-2,2), ylim=c(-2,2), zlim=c(-2,2))
#text3d(pca$scores[,1:3])
text3d(pca$loadings[,1:3], texts=rownames(pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pca$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=2)


#Biplot reduced
plot3d(pca$scores[,1:3],xlabs=rep("o", nrow(diab)), xlim = c(-0.5,0.5), ylim=c(-0.5,0.5), zlim=c(-0.5,0.5))
#text3d(pca$scores[,1:3])
text3d(pca$loadings[,1:3], texts=rownames(pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pca$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=2)

pca$loadings

#Clustering............................................
set.seed(42)
cl = kmeans(diab[,1:8], 2)

diab$cluster = as.factor(cl$cluster)

diab$result = ifelse(as.factor(diab$Outcome) == 0, 1, 2)

plot3d(pca$scores[,1:3], col = diab$cluster, main = "k-means cluster")
plot3d(pca$scores[,1:3], col = diab$result, main = "acutal output")

with(diab, table(cluster, result))

#View(diab)

#Conduct k-means on the two dimensional data
#in output from PCA

pc.comp= pca$scores
pc.comp1 = pc.comp[,1]
pc.comp2 = pc.comp[,2]
pc.comp3 = pc.comp[,3]

X=cbind(pc.comp1, pc.comp2, pc.comp3)


#Calculate optimal k -elbow method

k.max = 15
wss = rep(NA, k.max)
nClust = list()
for (i in 1:k.max){
  diaClasses = kmeans(X, i)
  wss[i] = diaClasses$tot.withinss
  nClust[[i]] = diaClasses$size
}

plot(1:k.max, wss,
     type = "b", pch = 19,
     xlab = "Number of cluster K",
     ylab = "Total within-cluster sum of squares")


diaCluster = kmeans(X, 4)
diaCluster

diab$cluster6 = as.factor(diaCluster$cluster)

plot3d(pca$scores[,1:3], col = diab$cluster6, main = "k-means cluster")












 







