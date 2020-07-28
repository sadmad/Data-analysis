install.packages("StatDA")
library(StatDA)
data(ohorizon)

##### B #####

# Enter distance matrix 
df <- ohorizon
df.clust.el <- data.frame("Ni"=log10(ohorizon$Ni), "Cu"=log10(ohorizon$Cu), "Cd"=log10(ohorizon$Cd))

df.clust.el.mat <- 
df.dist <- as.dist(df.clust.el)
# Single Linkage

cs <- hclust(df.clust.el, method="single", )
plot(cs)

# Complete Linkage
cc <- hclust(dm, method="complete")
plot(cc)

# Average Linkage
ca <- hclust(dm, method="average")
plot(ca)

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

