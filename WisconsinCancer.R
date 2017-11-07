

wisc.df <-data

wisc.data <- as.matrix(wisc.df[3:32])

row.names(wisc.data) <- wisc.df$id

diagnosis <- as.numeric(wisc.df$diagnosis=="M")

dim(wisc.data)

###Performing PCA
colMeans(wisc.data)

apply(wisc.data,MARGIN=2,sd)

##Performing scaling and PCA
wisc.pr <- prcomp(wisc.data,scale=TRUE)

summary(wisc.pr)

##Interpreting results
par(mfrow=c(1,2))
biplot(wisc.pr)
##Interpreting non scaled data
wisc.nons.pr <- prcomp(wisc.data,scale=FALSE)
biplot(wisc.nons.pr)
##scatter plot for observations of pca's
plot(wisc.pr$x[,c(1,2)],col=(diagnosis +1),xlab="PCA1",ylab="PCA2")

plot(wisc.pr$x[,c(1,3)],col=(diagnosis+1),xlab="PCA1",ylab="PCA3")

plot(wisc.pr$x[,c(2,3)],col=(diagnosis+1),xlab="PCA2",ylab="PCA3")

#####Calculating variability
#par(mfrow=c(1,2))
pr.var <- wisc.pr$sdev^2
pve <- pr.var/sum(pr.var)

##Plotting variance and cumulative proportion PC variable
plot(pve,ylim = c(0,1),col="red",xlab="Principal Component",ylab="Proportion Component of variance explained")
plot(cumsum(pve),ylim=c(0,1),col="red",xlab = "Principal Component",ylab="Cumulative Proportion of variance explained")

##Performing Hierarchical clustering

data.scaled <- scale(wisc.data)

##Calculating Euclidian distances

data.dist <- dist(data.scaled)

##Creating Hierarchical model 
wisc.hclust <- hclust(data.dist,method="complete")

##Plotting model

plot(wisc.hclust)

##Selecting Number of cluster (cutting tress )
wisc.hclust.clusters <-cutree(wisc.hclust,k=4)

###Now comparing cluster to diagnosis

table(diagnosis,wisc.hclust.clusters)

##Performing kmeans clustering
wisc.kc <- kmeans(scale(wisc.data),centers=2,nstart=20)

##Now comparing kmeans to diagnosis

table(wisc.kc$cluster,diagnosis)

##Now comparing hierarchical clustering to kmeans

table(wisc.hclust.clusters,wisc.kc$cluster)

##Finally clustering on PCA results

wisc.pr.hclust<- hclust(dist(wisc.pr$x[,1:7]),method = "complete")

wisc.pr.hclust.clusters <- cutree(wisc.hclust,k=4)

##Finally comparing all three
table(diagnosis,wisc.pr.hclust.clusters)

table(diagnosis,wisc.kc$cluster)
table(diagnosis,wisc.hclust.clusters)




#for selection of best model
wss <-0

for(i in 1:15) {
  km.out <- kmeans(wisc.data,centers=i,nstart = 20,iter.max = 50)
  wss[i] <- km.out$tot.withinss
}

plot(1:15,wss,type="b",xlab="no of clusters",ylab="within group sum of")
