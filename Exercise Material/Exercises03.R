# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 3                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 10 ---------------------------------------------------------


# Load packages
library(MASS)
library(ggplot2)
library(lattice)

# Data
help(cabbages)
str(cabbages)


# (a)

# Boxplot
bwplot(HeadWt ~ Cult | Date, data = cabbages)

# or
qplot(Date, HeadWt, geom="boxplot", fill=Cult, data = cabbages)

# or
qplot(Cult, HeadWt, geom="boxplot", fill=Date, data = cabbages)

# or
qplot(Date, HeadWt, geom="boxplot", facets = Cult ~ ., data = cabbages) + coord_flip()

# or
qplot(Cult, HeadWt, geom="boxplot", facets = Date ~ ., data = cabbages) + coord_flip()



# (b)

# Scatter Plot
xyplot(VitC ~ HeadWt | Cult, data = cabbages)

# or
qplot(HeadWt, VitC, data=cabbages, facets=Cult ~ .)

# or
qplot(HeadWt, VitC, data=cabbages, color=Cult)

# or
qplot(HeadWt, VitC, data=cabbages, color=Date, facets=Cult ~ .)




# -------- Exercise 11 ---------------------------------------------------------


# Results olympic heptathlon competition 1988 
data("heptathlon",package="HSAUR2")
heptathlon
cor(heptathlon)
cov(heptathlon)


# (a)

# Scatter Plot Matrix
plot(heptathlon)



# (b)

# identify Athlet with lowest score 
wms <- which.min(heptathlon$score)

# in Scatter Plot Matrix
cx <- rep(1, nrow(heptathlon))
cx[wms] <- 2
plot(heptathlon, col= cx)

# delete respective row
X <- heptathlon[-wms,]

# Scatter Plot Matrix
plot(X)



# (c)

# delete variable score
sc <- which(colnames(X) == "score")
X <- X[,-sc]


# standardize data 
Xscaled <- scale(X)



# (d)

# Heatmap
heatmap(Xscaled, scale="none")

# better
heatmap(Xscaled, scale="none", margins=c(7,5))


# with lattice
library(lattice)

# Please note:
# this is only for the more advanced students
# if you are not yet that advanced in R you can ignore below code for now,
# it is not crucial for understanding the topics of the course
# Note also that this will be easier to understand
# once we discussed the topic Cluster Analysis


# without reordering performed by cluster algorithm
# Note that color code is inverse compared to
# the standard color code used in heatmap()
levelplot(t(Xscaled), col.regions=heat.colors, aspect=1)

# Manually reorder rows and columns according to result 
# of cluster algorithm 
ord2 <- order.dendrogram(as.dendrogram(hclust(dist((Xscaled))))) # Rows
ord1 <- order.dendrogram(as.dendrogram(hclust(dist(t(Xscaled))))) # Cols
levelplot(t(Xscaled)[ord1,ord2], col.regions=heat.colors, at=do.breaks(c(-2.5,2.7),20), aspect=1)
