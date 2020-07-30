#install.packages("StatDA")
library(StatDA)
data(ohorizon)

##### B #####

# Enter distance matrix
#install.packages('mvtnorm')
library(lattice)
library(mvtnorm)
df <- ohorizon
df.clust.el <- data.frame("Ni"=ohorizon$Ni[1:50], "Cu"=ohorizon$Cu[1:50])
par(mfrow=c(1,1))
hist(df.clust.el$Ni)
hist(df.clust.el$Cu)
hist(log10(df.clust.el$Ni))
hist(log10(df.clust.el$Cu))
any(is.na(df.clust.el))
plot(df.clust.el)
scale(df.clust.el)
df.clust.el[,1]<- log10(df.clust.el$Ni)
df.clust.el[,2]<- log10(df.clust.el$Cu)
plot(df.clust.el)
dm <- as.dist(df.clust.el[, c("Ni", "Cu")])
dm 

set.seed(51)
# Single Linkage
?hclust
cs <- hclust(dm, method = "single")
plot(cs)

# Complete Linkage
cc <- hclust(dm, method="complete")
plot(cc)

# Average Linkage
ca <- hclust(dm, method="average")
plot(ca)

#Mcquitty Linkage
cmc <- hclust(dm, method="mcquitty")
plot(cmc, cex = 0.6 )

#Centroid Linkage
cm <- hclust(dm, method="median")
plot(cm)

#Centroid Linkage
ccen <- hclust(dm, method="centroid")
plot(ccen)

# cut the tree and show the colors on plot
# mcquitty method
plot(cc)
abline(h = 0.25, col = "lightgrey")
lab <- cutree(cmc, h = 0.25)
lab
plot(df.clust.el, col=lab)

#complete method
plot(cc)
abline(h = 6.6, col = "lightgrey")
lab <- cutree(cc, h = 6.6)
lab
plot(df.clust.el, col=lab)

plot(cc)
abline(h = 9.5, col = "lightgrey")
lab <- cutree(cc, h = 10)
lab
plot(df.clust.el, col=lab)

### try with same element but with including the whole rows
###test with more rows

df.clust.el <- data.frame("Ni"=scale(log10(ohorizon$Ni)), "Cu"=scale(log10(ohorizon$Cu)))
length(df.clust.el[ ,1])

par(mfcol=c(2,2))
hist(df.clust.el[,1])
hist(df.clust.el[,2])
hist(log10(df.clust.el[,1]))
hist(log10(df.clust.el[,2]))
any(is.na(df.clust.el))

df.el.scale <- scale(df.clust.el)
plot(df.clust.el)
plot(df.el.scale)

plot(df.clust)
df.dist <- as.dist(df.clust.el[, c("Ni", "Cu")])
dm <- dist(df.clust.el[, c("Ni", "Cu")])
dm 
par(mfcol=c(1,1))
# Single Linkage
?hclust
cs <- hclust(dm, method = "single")
plot(cs)

# Complete Linkage
cc <- hclust(dm, method="complete")
plot(cc)

# Average Linkage
ca <- hclust(dm, method="average")
plot(ca)

#Mcquitty Linkage
cmc <- hclust(dm, method="mcquitty")
plot(cmc, cex = 0.6 )

#Centroid Linkage
cm <- hclust(dm, method="median")
plot(cm)

#Centroid Linkage
cc <- hclust(dm, method="centroid")
plot(ccen)

# cut the tree and show the colors on plot
# mcquitty method
plot(cc)
abline(h = 0.56, col = "lightgrey")
lab <- cutree(cc, h = 0.56)
lab
plot(df.clust.el, col=lab)

#complete method
plot(cc)
abline(h = 6.6, col = "lightgrey")
lab <- cutree(cc, h = 6.6)
lab
plot(df.clust.el, col=lab)

plot(cc)
abline(h = 9.5, col = "lightgrey")
lab <- cutree(cc, h = 10)
lab
plot(df.clust.el, col=lab)

###another -- elements###
df.clust.4el <- data.frame("Mg"=ohorizon$Mg[1:70], "Cu"=ohorizon$Cu[1:70], "Ni"=ohorizon$Ni[1:70], "Sr"=ohorizon$Sr[1:70])
hist(df.clust.el$Ni)
hist(df.clust.el$Cu)
hist(log10(df.clust.el$Ni))
hist(log10(df.clust.el$Cu))
any(is.na(df.clust.el))
plot(df.clust.el)
scale(df.clust.el)
df.clust.el[,1]<- log10(df.clust.el$Ni)
df.clust.el[,2]<- log10(df.clust.el$Cu)
plot(df.clust.el)
dm <- dist(df.clust.el[, c("Ni", "Cu")])
dm 

# Single Linkage
?hclust
cs <- hclust(dm, method = "single")
plot(cs)

# Complete Linkage
cc <- hclust(dm, method="complete")
plot(cc)

# Average Linkage
ca <- hclust(dm, method="average")
plot(ca)

#Mcquitty Linkage
cmc <- hclust(dm, method="mcquitty")
plot(cmc, cex = 0.6 )

#Centroid Linkage
cm <- hclust(dm, method="median")
plot(cm)

#Centroid Linkage
cc <- hclust(dm, method="centroid")
plot(ccen)

# cut the tree and show the colors on plot
# mcquitty method
plot(cmc)
abline(h = 0.13, col = "lightgrey")
lab <- cutree(cmc, h = 0.17)
lab
plot(df.clust.el, col=lab)

#complete method
plot(cc)
abline(h = 6.6, col = "lightgrey")
lab <- cutree(cc, h = 6.6)
lab
plot(df.clust.el, col=lab)

plot(cc)
abline(h = 9.5, col = "lightgrey")
lab <- cutree(cc, h = 10)
lab
plot(df.clust.el, col=lab)

##### C #####

library(ggplot2)
library(cluster)
#set the seed for same result
set.seed(617)
#split the important data for pulution the inviroment
el.log <- data.frame("U"=log10(ohorizon$U), "Hg"=log10(ohorizon$Hg))
ggplot(el.log, aes(Hg, U)) + geom_point()
head(el.log )
el.clu <- kmeans(el.log , 5, nstart =30)
clusplot(el.log , el.clu $cluster, color=TRUE, shade=TRUE, labels=0,lines=0, main="K-Means Between U and Hg" )
