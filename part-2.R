#install.packages("StatDA")
library(StatDA)
data(ohorizon)

##### B #####

# Enter distance matrix
#install.packages('mvtnorm')
library(lattice)
library(mvtnorm)
df <- ohorizon
df.clust.el <- data.frame("Ni"=ohorizon$Ni[1:70], "Cu"=ohorizon$Cu[1:70])
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

### try with same element but with including the whole rows
###test with more rows

df.clust.el <- data.frame("Ni"=scale(ohorizon$Ni), "Cu"=scale(ohorizon$Cu))
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
plot(log10(df.clust.el))
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
split.ohorizon <-data.frame("Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE, "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG, "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)

split.ohorizon1$LITO[split.ohorizon1$LITO=="9"]<-"A"
split.ohorizon1$LITO[split.ohorizon1$LITO=="10"]<-"A"
split.ohorizon1$LITO[split.ohorizon1$LITO=="51"]<-"B"
split.ohorizon1$LITO[split.ohorizon1$LITO=="52"]<-"B"
split.ohorizon1$LITO[split.ohorizon1$LITO=="81"]<-"C"
split.ohorizon1$LITO[split.ohorizon1$LITO=="82"]<-"C"
split.ohorizon1$LITO[split.ohorizon1$LITO=="83"]<-"C"
split.ohorizon1$LITO[split.ohorizon1$LITO=="7"]<-"D"


library(ggplot2)
ggplot(split.ohorizon1, aes(Ni, As, LITO = "A")) + geom_point()
library(cluster)
set.seed(101)
split.ohorizon.log <- data.frame("Ni"=log10(ohorizon$Ni), "Cu"=log10(ohorizon$Cu), "Cd"=log10(ohorizon$Cd))
head(split.ohorizon.log)
head(split.ohorizon)
split.ohorizon1.clu <- kmeans(split.ohorizon.log, 3, nstart =20)
split.ohorizon1.clu

clusplot(split.ohorizon.log, split.ohorizon1.clu$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )

