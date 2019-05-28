#' ruspini_scaled data is in package cluster. It is a very simple data set with well separated clusters.
data(ruspini, package="cluster")
#' Shuffle rows
ruspini <- ruspini[sample(1:nrow(ruspini)),]
plot(ruspini)
#########

#' Scale each column in the data to zero mean and unit standard deviation (z-scores). This prevents one attribute with a large range to dominate the others for the distance calculation.
ruspini_scaled <- scale(ruspini)
plot(ruspini_scaled)
############
#' # Clustering methods
#' ## k-means Clustering
#'
#' Assumes Euclidean distances. We use k=10 clusters and run the algorithm 10 times with random initialized centroids. The best result is returned.
km <- kmeans(ruspini_scaled, centers=4, nstart=10)
km

plot(ruspini_scaled, col=km$cluster)
points(km$centers, pch=3, cex=2) # this adds the centroids
text(km$centers, labels=1:4, pos=2) # this adds the cluster ID
###########
#' Alternative plot from package cluster (uses principal components analysis for >2 dimensions)
library(cluster)
clusplot(ruspini_scaled, km$cluster)

#' Inspect the centroids (cluster profiles)
km$centers
#########
##fig.height=3, fig.width=10
def.par <- par(no.readonly = TRUE) # save default, for resetting...
layout(t(1:4)) # 4 plots in one
for(i in 1:4) barplot(km$centers[i,], ylim=c(-2,2), main=paste("Cluster", i))
par(def.par)  #- reset to default
#########
#' Find data for a single cluster
#'
#' All you need is to select the rows corresponding to the cluster. The next
#' example plots all data points of cluster 1
cluster1 <- ruspini_scaled[km$cluster==1,]
head(cluster1)
plot(cluster1, xlim = c(-2,2), ylim = c(-2,2))
#########
#' Try 10 clusters
plot(ruspini_scaled, col=kmeans(ruspini_scaled, centers=10)$cluster)
#########
#' ## Hierarchical Clustering
#'
#' dist defaults to method="Euclidean"
d <- dist(ruspini_scaled)
#' We cluster using complete link
hc <- hclust(d, method="complete")

#' Dendrogram
plot(hc)
rect.hclust(hc, k=4)

plot(as.dendrogram(hc), leaflab="none") # plot dendrogram without leaf labels
#########
#' More plotting options for dendrograms, including plotting
#' parts of large dendrograms can be found [here.](https://rpubs.com/gaston/dendrograms)

cluster_complete <- cutree(hc, k=4)
plot(ruspini_scaled, col=cluster_complete)

#' Try 10 clusters
plot(ruspini_scaled, col=cutree(hc, k=10))

#' Clustering with single link
hc_single <- hclust(d, method="single")
plot(hc_single)
rect.hclust(hc_single, k=4)

cluster_single <- cutree(hc_single, k=4)
plot(ruspini_scaled, col=cluster_single)

#' Try 10 clusters
plot(ruspini_scaled, col=cutree(hc_single, k=10))

