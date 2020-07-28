# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Cluster Analysis                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# load a few packages
library(lattice)
library(mvtnorm)


# Initial situation
dat <- rbind(rmvnorm(25, mean = c(3,3)),
             rmvnorm(20, mean = c(10, 8)),
             rmvnorm(10, mean = c(20, 1)))
plot(abs(dat), xlab = expression(x[1]), ylab = expression(x[2]))



# -------- Hierarchical agglomerative Methods ----------------------------------



# Example: body measurements
measure <- data.frame(
  "chest"=c(34, 37, 38, 36, 38, 43, 40, 38, 40, 41, 36, 36, 34, 33, 36, 37, 34,
            36, 38, 35),
  "waist"=c(30, 32, 30, 33, 29, 32, 33, 30, 30, 32, 24, 25, 24, 22, 26, 26, 25,
            26, 28, 23),
  "hips"=c(32, 37, 36, 39, 33, 38, 42, 40, 37, 39, 35, 37, 37, 34, 38, 37, 38,
           37, 40, 35)
)
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")
measure
plot(measure[,1:3], col=measure$gender)


# Distance matrix
dm <- dist(measure[, c("chest", "waist", "hips")])
round(dm, 2)


# Clustering
help(hclust)

# Single Linkage
cs <- hclust(dm, method = "single")
plot(cs)

# Complete Linkage
plot(cc <- hclust(dm, method = "complete"))

# Average Linkage
plot(ca <- hclust(dm, method = "average"))

# Centroid
plot(cz <- hclust(dm, method = "centroid"))

# Ward
plot(cw <- hclust(dm, method = "ward.D2"))



# Obtain clusters by drawing horizontal line into dendrogram, e.g. 

plot(cs)
abline(h = 3.5, col = "lightgrey")
lab <- cutree(cs, h = 3.6)
lab

plot(cc)
abline(h = 10, lty=2)
lab <- cutree(cc, h = 10)
lab
plot(measure[,1:3], col=lab)


plot(ca)
abline(h = 6.5, lty=2)
lab <- cutree(ca, h = 6)
lab
plot(measure[,1:3], col=lab)



# Cluster functions in package cluster
library(cluster)

# Agglomerative clustering with agnes, distance matrix from above
c.agnes <- agnes(dm, method="average")

# Distance matrix with daisy, provides a dissimilarity if variables with
# different scales of measurement (Skalenniveaus in German) are present
dm2 <- daisy(measure[, c("chest", "waist", "hips")])
round(dm2, 2)

pltree(c.agnes)
abline(h = 6, lty=2)

plot(c.agnes)
plot(c.agnes, which=2)
bannerplot(c.agnes)


# cutree to obtain clustering can be applied as well
cl <- cutree(as.hclust(c.agnes), h=6)
cl


# Visualizing
?clusplot
?clusplot.default
clusplot(x=measure[,1:3], clus=cl, color=FALSE, shade=TRUE, labels=0, lines=0)




# -------- K-means Clustering --------------------------------------------------


# Example: Chemical Analysis of pottery
library(MVA)
library(lattice)
help(pottery)
pottery
summary(pottery)

# Distances
pots <- scale(pottery[, colnames(pottery) != "kiln"])
pottery_dist <- dist(pots)

# Visualization in a Plot
levelplot(as.matrix(pottery_dist), xlab = "Pot Number", ylab = "Pot Number")

# Hierarcical clustering
ca <- hclust(pottery_dist, method = "average")
plot(ca)

# K-means with 3 Clusters
help("kmeans")
km3 <- kmeans(pots, centers = 3, nstart = 100)
km3

# Within variation (sum of squares) of the cluster as function of number
# of clusters for K-means
n <- nrow(pots)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(apply(pots, 2, var))
for (i in 2:6)
  wss[i] <- kmeans(pots, centers = i, nstart = 100)$tot.withinss
plot(1:6, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")


# Compare Clusters with archaeological site
pottery_cluster <- km3$cluster
xtabs(~ pottery_cluster + kiln, data = pottery)


# Comparison with hclust
table(pottery_cluster, cutree(ca, k=3))

library(cluster)
clusplot(pots, clus=km3$cluster, 
         color=FALSE, shade=TRUE, labels=0, lines=0)
   
   
         
         
# PAM: Partitioning around medoids (Robust Version of k-means)
library(cluster)

pam3 <- pam(pots, k=3)
pam3$clustering
pam3$silinfo

plot(pam3)

clusplot(pots, clus=pam3$cluster, 
         color=FALSE, shade=TRUE, labels=0, lines=0)
plot(silhouette(pam3))

# Comparison k-means and pam
table(pottery_cluster, pam3$clustering)



# -------- Mixture distribution approaches / Modell-based Clustering -----------

# Package
library(mclust)

# Clustering of pottery Data
help(Mclust)
mc <- Mclust(pots)
summary(mc)

# BIC-Plot
plot(mc, what = "BIC", col = "black")
help(mclustModelNames)

# Probabilities for belonging to the individual clusters
mc$z

# resulting clusters
mc$classification

# Comparison with above results
table(pottery_cluster, mc$classification)
xtabs(~ mc$classification + kiln, data = pottery)

# Look also at PCs
PCApots <- prcomp(pots)
plot(PCApots$x[,1:2], col = mc$classification)
plot(PCApots$x[,1:2], col = pottery_cluster)


