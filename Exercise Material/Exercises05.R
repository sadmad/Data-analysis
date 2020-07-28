# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 5                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 16 ---------------------------------------------------------


# (d)

# Enter distance matrix 
dm <- matrix(c(0,9,7,12,8,
               9,0,4,2,5,
               7,4,0,1,13,
               12,2,1,0,6,
               8,5,13,6,0),5,5, byrow = TRUE)

rownames(dm) <- colnames(dm) <- c("A","B","C","D","E")
dm <- as.dist(dm)

dm
# Single Linkage
cs <- hclust(dm, method="single")
plot(cs)

# Complete Linkage
cc <- hclust(dm, method="complete")
plot(cc)

# Average Linkage
ca <- hclust(dm, method="average")
plot(ca)



# -------- Exercise 17 ---------------------------------------------------------


# Dataset
library(HSAUR2)
data(USairpollution)
help(USairpollution)
USairpollution
UStmp <- USairpollution[,names(USairpollution)!="SO2"]



# (a)

# Standardize
UStmp <- scale(UStmp)

# distance matrix
dm <- dist(UStmp)
round(dm, 2)

# Single Linkage
cs <- hclust(dm, method = "single")
plot(cs)

# Complete Linkage
cc <- hclust(dm, method = "complete")
plot(cc)

# Average Linkage
plot(ca <- hclust(dm, method = "average"))


# Ward
plot(cw <- hclust(dm, method = "ward.D2"))



# (b)

# Complete Linkage
plot(cc)
abline(h=6,lty=3)

# Ward
plot(cw)
abline(h=6,lty=3)


# Cluster
clstr <- cutree(cw, h = 6)

table(clstr)


# Compare cluster, e.g. via boxplots 
boxplot(temp ~ factor(clstr), data=USairpollution, varwidth=TRUE)
boxplot(manu ~ factor(clstr), data=USairpollution, varwidth=TRUE)
boxplot(popul ~ factor(clstr), data=USairpollution, varwidth=TRUE)
boxplot(wind ~ factor(clstr), data=USairpollution, varwidth=TRUE)
boxplot(precip ~ factor(clstr), data=USairpollution, varwidth=TRUE)
boxplot(predays ~ factor(clstr), data=USairpollution, varwidth=TRUE)

boxplot(SO2 ~ factor(clstr), data=USairpollution, varwidth=TRUE)
