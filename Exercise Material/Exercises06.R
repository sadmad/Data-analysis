# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 6                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 18 ---------------------------------------------------------


# Data
library(cluster)
agri <- scale(agriculture)


# (a)

# Distance matrix
dm <- dist(agri)
round(dm, 2)

# Single Linkage
plot(cs <- hclust(dm, method = "single"))

# Complete Linkage
plot(cc <- hclust(dm, method = "complete"))

# Average Linkage
plot(ca <- hclust(dm, method = "average"))


# (b)

# Relatively clear clusters/group separation, e.g. 
plot(ca)
abline(h=1.5,lty=3)

# Cluster
clstr <- cutree(ca, k = 2)

# Call plot() without plotting data 
plot(agriculture, type="n")

# Add text
text(agriculture, labels=rownames(agriculture), col=clstr)


# (c)

help(agnes)

# Single Linkage
par(mfrow=c(2,2))
pltree(agnes(agri, method = "single"))

# Complete Linkage
pltree(agnes(agri, method = "complete"))

# Average Linkage
pltree(agnes(agri, method = "average"))


# (d)

help(diana)

# Divisive Clustering
pltree(diana(agri))
par(mfrow=c(1,1))



# -------- Exercise 19 ---------------------------------------------------------


# Dataset 
jet <- read.table("C:/Datenanalyse/Übung/jet.txt", header=TRUE)
jet
summary(jet)


# (a)

# reduced dataset 
X <- jet[, c("SPR", "RGF", "PLF", "SLF")]

# Scatter Plot Matrix
plot(X, pch = as.numeric(jet$CAR), col = as.numeric(jet$CAR))

# scaled/standardized dataset
Xs <- scale(X)

# K-means 4 cluster
km <- kmeans(Xs, centers = 4, nstart = 1000)

# Scatter Plot Matrix
plot(X, col = km$cluster)

# Boxplots, e.g.
boxplot(SPR ~ km$cluster, data = jet)

# Within-variation (Sum of squares)
n <- nrow(Xs)
wss <- rep(0, 10)
# For a single Cluster
wss[1] <- (n - 1) * sum(apply(Xs, 2, var))
# For 2-10 Clusters
for (i in 2:10)
{
  # Compuation of Sum of within-variation (sum of squares) over all clusters 
  wss[i] <- sum(kmeans(Xs, centers = i, nstart=1000)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")


# (b)


# PCA
jet_pca <- prcomp(X, scale=TRUE)
summary(jet_pca)
plot(jet_pca, type="l")
abline(h=1, lty=3)

# Visualize cluster in the space spanned by the first two principal components 
pr <- jet_pca$x[, 1:2]
plot(pr, col = km$cluster)

# or
library(cluster)
help("clusplot")
clusplot(X, km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


# (c)

# Agglomerative Clustering
library(cluster)
aa <- agnes(Xs, method = "average")
plot(aa)
pltree(aa)

ac <- agnes(Xs, method = "complete")
plot(ac)
pltree(ac)

# Compare with K-means
acc <- cutree(ac, k = 4)
table(km$cluster, acc)

acc
km$cluster


# Clustering based on mixture distributions
library(mclust)
mc <- Mclust(Xs)
summary(mc)
2*6.619093 - log(22)*62

# BIC-Plot
plot(mc, what = "BIC", col = "black")
help(mclustModelNames)

# Scatter Plot Matrix
plot(X, col = mc$classification)

# Compare with K-means
table(km$cluster, mc$classification)

# or with PCA
par(mfrow = c(1,3))
clusplot(X, clus = acc, main = "Complete Linkage", 
         color=FALSE, shade=TRUE, labels=0, lines=0)
clusplot(X, clus = km$cluster, main = "K-means", 
         color=FALSE, shade=TRUE, labels=0, lines=0)
clusplot(X, clus = mc$classification, main = "Mixture Distribution", 
         color=FALSE, shade=TRUE, labels=0, lines=0)
par(mfrow = c(1,1))



# -------- Exercise 20 ---------------------------------------------------------


# insurance data
library(insuranceData)

# automobile bodily injury claims
help("AutoBi")
data("AutoBi")
summary(AutoBi)


# (a)

# data 
AB <- AutoBi[,-c(1,4)]
summary(AB)


# (b)

# scale data
ABsc <- scale(AB)

# distance matrix
dm <- dist(ABsc)

# single linkage
cs <- hclust(dm, method = "single")
plot(cs)

# remove extreme outlier
AB <- AutoBi[-862,-c(1,4)]
summary(AB)
ABsc <- scale(AB)


# (c)

# distance matrix
dm <- dist(ABsc)

# single linkage
plot(cs <- hclust(dm, method = "single"))

# complete linkage
plot(cc <- hclust(dm, method = "complete"))

# average linkage
plot(ca <- hclust(dm, method = "average"))

# centroid
plot(cz <- hclust(dm, method = "centroid"))

# Ward
plot(cw <- hclust(dm, method = "ward.D2"))


# use heat map for illustration and interpreting clusters
myhclust <- function(x){hclust(x, method = "ward.D2")} 
heatmap(ABsc, hclustfun = myhclust, scale = "none", margins=c(8,5))

table(AB$ATTORNEY)
table(AB$CLMSEX)
table(AB$CLMINSUR)
table(AB$SEATBELT)


# determine concrete clusters by drawing a horizontal line
# in the dendrogram
plot(cw)
abline(h = 45, col = "lightgrey")
abline(h = 30, col = "lightgrey")
lab <- cutree(cw, h = 30)
table(lab)


# closer look into clusters, e.g.
attach(AB)
boxplot(LOSS ~ lab, varwidth = TRUE)
boxplot(CLMAGE ~ lab, varwidth = TRUE)

library(ggplot2)
help(qplot)
qplot(factor(lab), fill = factor(CLMSEX), geom = "bar")
qplot(factor(lab), fill = factor(ATTORNEY), geom = "bar")
detach(AB)


# k-means does not work with NAs
kmeans(ABsc, centers = 7, iter.max = 100, nstart = 100)
summary(AB)

# e.g., impute 
AB$CLMSEX[is.na(AB$CLMSEX)] <- sample(AB$CLMSEX[!is.na(AB$CLMSEX)], 
                                      sum(is.na(AB$CLMSEX)), replace = TRUE)
AB$CLMINSUR[is.na(AB$CLMINSUR)] <- sample(AB$CLMINSUR[!is.na(AB$CLMINSUR)], 
                                          sum(is.na(AB$CLMINSUR)), replace = TRUE)
AB$SEATBELT[is.na(AB$SEATBELT)] <- sample(AB$SEATBELT[!is.na(AB$SEATBELT)], 
                                          sum(is.na(AB$SEATBELT)), replace = TRUE)
AB$CLMAGE[is.na(AB$CLMAGE)] <- sample(AB$CLMAGE[!is.na(AB$CLMAGE)], 
                                      sum(is.na(AB$CLMAGE)), replace = TRUE)
summary(AB)

# and scale
ABsc <- scale(AB)


# within cluster sum of squres vs. the number of clusters
n <- nrow(ABsc)
wss <- rep(0, 20)
wss[1] <- (n - 1) * sum(apply(ABsc, 2, var))
for (i in 2:20)
  wss[i] <- kmeans(ABsc, centers = i, iter.max = 100, 
                   nstart = 100)$tot.withinss
plot(1:20, wss, type = "b", xlab = "Number of clusters",
     ylab = "Within cluster sum of squares")


# 7 looks fine again
km7 <- kmeans(ABsc, centers = 7, iter.max = 100, nstart = 100)
km7lab <- km7$cluster

# Heatmap again with imputed data
heatmap(ABsc, hclustfun = myhclust, scale = "none", margins=c(8,5))


# compare with hclust
table(lab, km7lab)


# as before
attach(AB)
boxplot(LOSS ~ km7lab, varwidth = TRUE)
boxplot(CLMAGE ~ km7lab, varwidth = TRUE)
qplot(factor(km7lab), fill = factor(CLMSEX), geom = "bar")
qplot(factor(km7lab), fill = factor(ATTORNEY), geom = "bar")
detach(AB)



# -------- Exercise 21 ---------------------------------------------------------


# Enter data and sort it
x <- c(1.56, 1.71, 1.54, 1.66, 1.58, 1.90, 1.85, 2.05, 1.72, 1.67) 
x <- sort(x)

# hierarchical/agglomerative
dmx <- dist(x)
plot(hclust(dmx, method = "single"))

# K-means starting partition
clstr <- c(1,1,1,1,1,1,1,2,2,2)
x[clstr == 1]

x[clstr == 2]

# (A) Check Minimum-Distance-Property
m1 <- mean(x[clstr == 1])
m2 <- mean(x[clstr == 2])
cbind((x - m1)^2,(x - m2)^2)

# (B) Check swapping with other Cluster 

# current
sum((x[clstr == 1] - m1)^2) + sum((x[clstr == 2] - m2)^2)

# 1.85 in different Cluster
clstr <- c(1,1,1,1,1,1,1,1,2,2)

# H increases
m1 <- mean(x[clstr == 1])
m2 <- mean(x[clstr == 2])
sum((x[clstr == 1] - m1)^2) + sum((x[clstr == 2] - m2)^2)

# Comparison with k-means
kmeans(cbind(x), centers = 2)

kmeans(cbind(x), centers = 2)$tot.withinss

