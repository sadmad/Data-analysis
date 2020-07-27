install.packages("StatDA")
library(StatDA)
data(ohorizon)



#### C KMean ######
hist(ohorizon$Ni)
hist(log10(ohorizon$Ni))
hist(ohorizon$Cu)
hist(log10(ohorizon$Cu))
hist(ohorizon$Cd)
hist(log10(ohorizon$Cd))
hist(ohorizon$As)
hist(log10(ohorizon$As))
hist(ohorizon$Zn)
hist(log10(ohorizon$Zn))
hist(ohorizon$Pb)
hist(log10(ohorizon$Pb))
hist(ohorizon$Hg)
hist(log10(ohorizon$Hg))
hist(ohorizon$Cd)

#bivariate relationships of different element 
plot(log10(ohorizon$Ni),log10(ohorizon$Ni))
plot(log10(ohorizon$Ni),log10(ohorizon$Cu))
plot(log10(ohorizon$Ni),log10(ohorizon$Cd))
plot(log10(ohorizon$Ni),log10(ohorizon$As))    
plot(log10(ohorizon$Ni),log10(ohorizon$Zn)) 
plot(log10(ohorizon$Cu),log10(ohorizon$Pb))
plot(log10(ohorizon$Ni),log10(ohorizon$Hg))     


plot(log10(ohorizon$Cu),log10(ohorizon$Ni))
plot(log10(ohorizon$Cu),log10(ohorizon$Cd))
plot(log10(ohorizon$Cu),log10(ohorizon$As))
plot(log10(ohorizon$Cu),log10(ohorizon$Zn))
plot(log10(ohorizon$Cu),log10(ohorizon$Pb))
plot(log10(ohorizon$Cu),log10(ohorizon$Hg))


plot(log10(ohorizon$Cd),log10(ohorizon$Ni))
plot(log10(ohorizon$Cd),log10(ohorizon$Cu))
plot(log10(ohorizon$Cd),log10(ohorizon$As))
plot(log10(ohorizon$Cd),log10(ohorizon$Zn))
plot(log10(ohorizon$Cd),log10(ohorizon$Pb))
plot(log10(ohorizon$Cd),log10(ohorizon$Hg))


plot(log10(ohorizon$As),log10(ohorizon$Ni))
plot(log10(ohorizon$As),log10(ohorizon$Cu))
plot(log10(ohorizon$As),log10(ohorizon$Cd))
plot(log10(ohorizon$As),log10(ohorizon$Zn))
plot(log10(ohorizon$As),log10(ohorizon$Pb))
plot(log10(ohorizon$As),log10(ohorizon$Hg))


plot(log10(ohorizon$Zn),log10(ohorizon$Ni))
plot(log10(ohorizon$Zn),log10(ohorizon$Cu))
plot(log10(ohorizon$Zn),log10(ohorizon$Cd))
plot(log10(ohorizon$Zn),log10(ohorizon$As))
plot(log10(ohorizon$Zn),log10(ohorizon$Pb))
plot(log10(ohorizon$Zn),log10(ohorizon$Hg))


plot(log10(ohorizon$Pb),log10(ohorizon$Ni))
plot(log10(ohorizon$Pb),log10(ohorizon$Cu))
plot(log10(ohorizon$Pb),log10(ohorizon$Cd))
plot(log10(ohorizon$Pb),log10(ohorizon$As))
plot(log10(ohorizon$Pb),log10(ohorizon$Zn))
plot(log10(ohorizon$Pb),log10(ohorizon$Hg))

plot(log10(ohorizon$Hg),log10(ohorizon$Ni))
plot(log10(ohorizon$Hg),log10(ohorizon$Cu))
plot(log10(ohorizon$Hg),log10(ohorizon$Cd))
plot(log10(ohorizon$Hg),log10(ohorizon$As))
plot(log10(ohorizon$Hg),log10(ohorizon$Zn))
plot(log10(ohorizon$Hg),log10(ohorizon$Pb))

#
pairs(split.ohorizon)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(split.ohorizon)
split.ohorizon <-data.frame("Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE, "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG, "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)

aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$COUN), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$VEG_ZONE), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$GROUNDVEG), mean)
split.ohorizon1$LITO<- as.character(split.ohorizon1$LITO)
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
split.ohorizon.log <- data.frame("Ni"=log10(ohorizon$Ni), "Cu"=log10(ohorizon$Cu), "Cd"=log10(ohorizon$Cd), "As"=log10(ohorizon$As), "Zn"=log10(ohorizon$Zn), "Pb"=log10(ohorizon$Pb), "Hg"=log10(ohorizon$Hg))
head(split.ohorizon.log)
head(split.ohorizon)
split.ohorizon1.clu <- kmeans(split.ohorizon.log, 3, nstart =20)
split.ohorizon1.clu

clusplot(split.ohorizon.log, split.ohorizon1.clu$cluster, color=TRUE, shade=TRUE, labels=0,lines=0, )

