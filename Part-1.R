library(StatDA)
library(ggplot2)
library(PerformanceAnalytics)
data(ohorizon)
df <- ohorizon

#### abstract ####
#Surface Maps Constructed With Kriging

el=ohorizon[,"As"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","#aa3311","green","blue"),add.plot=T)
SymbLegend(X,Y,el,type="percentile",qutiles<-c(0,0.05,0.25,0.75,0.95,1),symbtype="EDA",symbmagn=0.8,
           leg.position="topright",leg.title="As [mg/kg]",leg.title.cex=0.8,leg.round=2,leg.wid=4,leg.just="rig")
text(min(X)+diff(range(X))*4/7,max(Y),paste(qutiles*100,collapse=","),cex=0.8)
text(min(X)+diff(range(X))*4/7,max(Y)-diff(range(Y))/25,"Percentiles",cex=0.8)
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=37e3,sizetext=0.8)
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)



#add the data to variable
summary(df)
head(df)

 ##### (a) ##### 

df.el <- data.frame("Ni"=df$Ni, "Cu"=df$Cu, 'Co'=df$Co, 'Pd'=df$Pd,'Cr'=df$Cr, 'S'=df$S, 
                    "Bi"=df$Bi,"Pb"=df$Pb, "As"=df$As, "Cd"=df$Cd, "Zn"=df$Zn, "Hg"=df$Hg, 
                    "U"=df$U, "Th"= df$Th, "Ti"=df$Ti, 
                    'pH'=df$pH,"XCOO"=df$XCOO, 'YCOO'=df$YCOO, 'ELEV'=df$ELEV,
                  'COUN'=df$COUN)
#new hist
par(mfrow=c(2,2))
edaplotlog(df.el$Ni, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Ni [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.el$Bi, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Bi [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.el$U, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="U [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.el$Ti, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Ti [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)

lapply(df.el[-20], function(x) shapiro.test(x))

### transformation ###
############(Ti,U,Bi,Co,Cr,Th,S,Ni,Cu,Cd,As,Zn,Pb,Hg,Pd,Pd.log,pH,XCOO,YCOO,ELEV)
#install.packages("MASS")
library(MASS)
#Ni

linearmodel = lm(Ni ~ Cu, data = ohorizon)
#plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Ni)^-.5 ~ Cu, data = ohorizon)
#plot(fullmodel.inv)

Ni.boxcox <- (ohorizon$Ni)^-0.379
hist(Ni.boxcox)
shapiro.test((Ni.boxcox))
qqnorm(Ni.boxcox)
qqline(Ni.boxcox)


#Cu

linearmodel = lm(Cu ~ Ni, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]

Cu.boxcox <- (ohorizon$Cu)^-0.7
hist(Cu.boxcox)
shapiro.test((Cu.boxcox))
qqnorm(Cu.boxcox)
qqline(Cu.boxcox)

#Co

linearmodel = lm(Co ~ Pd, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Co.boxcox <- (ohorizon$Co)^-0.28
hist(Co.boxcox)
shapiro.test((Co.boxcox))
qqnorm(Co.boxcox)
qqline(Co.boxcox)


#Pd

linearmodel = lm(Pd ~ Ni, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Pd)^-.5 ~ Ni, data = ohorizon)
Pd.boxcox <- (ohorizon$Pd)^-0.18
hist(Pd.boxcox)
shapiro.test((Pd.boxcox))
qqnorm(Pd.boxcox)
qqline(Pd.boxcox)

#Cr
linearmodel = lm(Cr ~ S, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Cr.boxcox <- (ohorizon$Cr)^-0.26
hist(Cr.boxcox)
shapiro.test((Cr.boxcox))
qqnorm(Cr.boxcox)
qqline(Cr.boxcox)

#Th

linearmodel = lm(Th ~ U, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Th.boxcox <- (ohorizon$Th)^-0.30
hist(Th.boxcox)
shapiro.test((Th.boxcox))
qqnorm(Th.boxcox)
qqline(Th.boxcox)

#U

linearmodel = lm(U ~ Th, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
U.boxcox <- (ohorizon$U)^-0.29
hist(U.boxcox)
shapiro.test((U.boxcox))
qqnorm(U.boxcox)
qqline(U.boxcox)

#As

linearmodel = lm(As ~ Pb, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
As.boxcox <- (ohorizon$As)^-0.53
hist(As.boxcox)
shapiro.test((As.boxcox))
qqnorm(As.boxcox)
qqline(As.boxcox)

#Pb

linearmodel = lm(Pb ~ As, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Pb.boxcox <- (ohorizon$Pb)^-0.42
hist(Pb.boxcox)
shapiro.test((Pb.boxcox))
qqnorm(Pb.boxcox)
qqline(Pb.boxcox)

#Cd

linearmodel = lm(Cd ~ As, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Cd.boxcox <- (ohorizon$Cd)^-0.095
hist(Cd.boxcox)
shapiro.test((Cd.boxcox))
qqnorm(Cd.boxcox)
qqline(Cd.boxcox)

#Zn

linearmodel = lm(Zn ~ As, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Zn.boxcox <- (ohorizon$Zn)^0.19
hist(Zn.boxcox)
shapiro.test((Zn.boxcox))
qqnorm(Zn.boxcox)
qqline(Zn.boxcox)

#S

linearmodel = lm(S ~ Ni, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
S.boxcox <- (ohorizon$S)^0.666
hist(S.boxcox)
shapiro.test((S.boxcox))
qqnorm(S.boxcox)
qqline(S.boxcox)

#Hg

linearmodel = lm(Hg ~ As, data = ohorizon)
bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
Hg.boxcox <- (ohorizon$Hg)^-0.059
hist(Hg.boxcox)
shapiro.test((Hg.boxcox))
qqnorm(Hg.boxcox)
qqline(Hg.boxcox)

df.trans<- data.frame(Ni.boxcox, Cu.boxcox, Co.boxcox, Pd.boxcox, Cr.boxcox, S.boxcox,
                      "log10.Bi" = log10(df.el$Bi), Cd.boxcox, As.boxcox, Zn.boxcox, Pb.boxcox, Hg.boxcox,
                      "log10.Ti" = log10(df.el$Ti), U.boxcox, Th.boxcox,
                      'pH'=df$pH, 'XCOO'=df$XCOO, 'YCOO'=df$YCOO, 'ELEV'=df$ELEV, "COUN" = df.el$COUN)
#Plot Transformed
par(mfrow=c(2,2))
edaplotlog(df.trans$Ni.boxcox, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Ni transformed",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.trans$log10.Bi, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Bi transformed",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.trans$U, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="U transformed",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)
edaplotlog(df.trans$log10.Ti, H.freq=F,box=T,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=F,
           P.main="",P.xlab="Ti transformed",P.ylab="Density",B.pch=3,B.cex=0.5,B.log=F)

lapply(df.trans[-20], function(x) shapiro.test(x))
#correlation of our splitted dataframe
#chart.Correlation(df.trans[,1:6])
#chart.Correlation(df.trans[,7:12])
#chart.Correlation(df.trans[,13:15])
df.1.trans <- cbind(df.trans[,1:6],df.trans[,17:18])
df.2.trans <- cbind(df.trans[,7:12],df.trans[,17:18])
df.3.trans <- cbind(df.trans[,13:15],df.trans[,17:18])
chart.Correlation(df.1.trans)
chart.Correlation(df.2.trans)
chart.Correlation(df.3.trans)

## part c ##
df.trans.1 <- na.omit(data.frame(Ni.boxcox, Cu.boxcox, Co.boxcox, Pd.boxcox, Cr.boxcox, S.boxcox,
                        "log10.Bi" = log10(df.el$Bi), Cd.boxcox, As.boxcox, Zn.boxcox, Pb.boxcox, Hg.boxcox,
                        "log10.Ti" = log10(df.el$Ti), U.boxcox, Th.boxcox,
                        'pH'=df$pH, 'XCOO'=df$XCOO, 'YCOO'=df$YCOO, 'ELEV'=df$ELEV,
                        "COUN" = df.el$COUN, 'VEG_ZONE'=df$VEG_ZONE, 'LITO'=df$LITO, 'GROUNDVEG'=df$GROUNDVEG))

df.el.1 <- na.omit(data.frame("Ni"=df$Ni, "Cu"=df$Cu, 'Co'=df$Co, 'Pd'=df$Pd,'Cr'=df$Cr, 'S'=df$S, 
                    "Bi"=df$Bi,"Pb"=df$Pb, "As"=df$As, "Cd"=df$Cd, "Zn"=df$Zn, "Hg"=df$Hg, 
                    "U"=df$U, "Th"= df$Th, "Ti"=df$Ti, 
                    'pH'=df$pH,"XCOO"=df$XCOO, 'YCOO'=df$YCOO, 'ELEV'=df$ELEV,
                    'COUN'=df$COUN, 'VEG_ZONE'=df$VEG_ZONE, 'LITO'=df$LITO, 'GROUNDVEG'=df$GROUNDVEG))

aggregate(df.trans.1[,1:16], list(df.trans.1$COUN), mean)
aggregate(df.el.1 [,1:16], list(df.el.1 $COUN), mean)

aggregate(df.trans.1[,1:16], list(df.trans.1$VEG_ZONE), mean)
aggregate(df.el.1 [,1:16], list(df.el.1 $VEG_ZONE), mean)

aggregate(df.trans.1[,1:16], list(df.trans.1$GROUNDVEG), mean)
aggregate(df.el.1 [,1:16], list(df.el.1 $GROUNDVEG), mean)

#LITO Classification 1

df.trans.1$LITO[df.trans.1$LITO == 9] <- "CALEDONIAN"
df.trans.1$LITO[df.trans.1$LITO == 10] <- "CALEDONIAN"

df.trans.1$LITO[df.trans.1$LITO == 51] <- "PALAEOZOIC"
df.trans.1$LITO[df.trans.1$LITO == 52] <- "PALAEOZOIC"

df.trans.1$LITO[df.trans.1$LITO == 81] <- "ALKALINE"
df.trans.1$LITO[df.trans.1$LITO == 82] <- "ALKALINE"
df.trans.1$LITO[df.trans.1$LITO == 83] <- "ALKALINE"

df.trans.1$LITO[df.trans.1$LITO == 7] <- "GRANITES"

#split out the other LITOS if they are numeric 1

df.lito.4groups.1<-df.trans.1[(df.trans.1$LITO == "CALEDONIAN" | df.trans.1$LITO == "PALAEOZOIC" |
                               df.trans.1$LITO == "ALKALINE" | df.trans.1$LITO == "GRANITES"),]

aggregate(df.lito.4groups.1[,1:16], list(df.lito.4groups.1$LITO), mean)


#LITO Classification 2

df.el.1$LITO[df.el.1$LITO == 9] <- "CALEDONIAN"
df.el.1$LITO[df.el.1$LITO == 10] <- "CALEDONIAN"

df.el.1$LITO[df.el.1$LITO == 51] <- "PALAEOZOIC"
df.el.1$LITO[df.el.1$LITO == 52] <- "PALAEOZOIC"

df.el.1$LITO[df.el.1$LITO == 81] <- "ALKALINE"
df.el.1$LITO[df.el.1$LITO == 82] <- "ALKALINE"
df.el.1$LITO[df.el.1$LITO == 83] <- "ALKALINE"

df.el.1$LITO[df.el.1$LITO == 7] <- "GRANITES"

#split out the other LITOS if they are numeric 2

df.lito.4groups.2<-df.el.1[(df.el.1$LITO == "CALEDONIAN" | df.el.1$LITO == "PALAEOZOIC" |
                               df.el.1$LITO == "ALKALINE" | df.el.1$LITO == "GRANITES"),]

aggregate(df.lito.4groups.2[,1:16], list(df.lito.4groups.2$LITO), mean)


#special multivariante plot
install.packages('tree')
library(tree)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,c("Co","Cu","Ni","Rb","Bi","Na","Sr")])
data(kola.background)
sel <- c(3,8,22, 29, 32, 35, 43, 69, 73 ,93,109,129,130,134,168,181,183,205,211,
         218,237,242,276,292,297,298,345,346,352,372,373,386,408,419,427,441,446,490,
         516,535,551,556,558,564,577,584,601,612,617)
x=el[sel,]
dimnames(x)[[1]]=ohorizon[sel,1]
xwid=diff(range(X))/12e4
ywid=diff(range(Y))/12e4
par(mfrow=c(1,2),mar=c(1.5,1.5,1.5,1.5))
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n",
     xlim=c(360000,max(X)))
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=T)
tree(x,locations=cbind(X[sel],Y[sel]),len=700,key.loc=c(793000,7760000),leglen=1500,
     cex=0.75, add=T, leglh=6,lh=30,wmax=120,wmin=30,labels=NULL)
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=37e3,sizetext=0.8)
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)
par(mar=c(.1,.1,.1,.5))
tree(x,key.loc=c(15,0),len=0.022, lh=30,leglh=4,
     wmax=120,wmin=30, leglen=0.05, ncol=8, cex=0.75)

############################## End of Part1, other approaches
# 
# 
# 
# 
# #split the df to the important elements
# df.h.metal <-data.frame("Ni"=df$Ni, "Cu"=df$Cu, "Cd"=df$Cd, "As"=df$As, "Zn"=df$Zn, "Pb"=df$Pb, "Hg"=df$Hg)
# 
# par(mfrow=c(2,2))
# plot(df$U)
# hist(df$U)
# hist(log10(df$U))
# hist(df$Th)
# hist(log10(df$Th))
# Th<-log10(df$Th)
# U<-log10(df$U)
# df.radioactive <- data.frame("Th"=Th, "U"=U)
# df.radioactive.veg.zone <- data.frame("VEG_ZONE"=df$VEG_ZONE, "Th"=Th, "U"=U)
# df.radioactive.zone <- data.frame("COUN"=df$COUN, "GROUNDVEG"=df$GROUNDVEG, "VEG_ZONE"=df$VEG_ZONE, "Th"=Th, "U"=U)
# df.radioactive
# #make plot of the important element
# plot(df.h.metal)
# 
# #### data Test#####
# shapiro.test(ohorizon$Cd)
# shapiro.test(log10(ohorizon$Cd))
# 
# ks.test(log10(ohorizon$Zn),"pnorm", mean(log10(ohorizon$Zn)), sd(log10(ohorizon$Zn)))
# ks.test(log10(ohorizon$Zn),log10(ohorizon$Ca))
# ### end ####
# 
# #first method of showing correlation
# df.cor <- cor(log10(df.h.metal))
# 
# #install.packages("GGally")
# library(GGally)
# ggcorr(df.cor)
# #secont method of visualizing correlation
# #install.packages("PerformanceAnalytics")
# library(PerformanceAnalytics)
# 
# chart.Correlation(log10(df.h.metal))
# chart.Correlation(df.radioactive)
# 
# 
# #going through the correlations
# # Boxplot
# qplot(log10(As), log10(Cu), geom="boxplot", fill=COUN, data = df)
# qplot(log10(As), log10(Cu), geom="boxplot", facets = COUN ~ ., data = df) + coord_flip()
# #Ni vs Cu
# qplot(log10(Ni), log10(Cu), geom="boxplot", fill=COUN, data = df)
# qplot(log10(Ni), log10(Cu), geom="boxplot", facets = COUN ~ ., data = df) + coord_flip()
# # plot of Zn and CU
# plot(log10(df$Zn), log10(df$Cu), pch = 19)
# abline(lm(log10(df$Cu) ~ log10(df$Zn)), col = "red")
# 
# 
# #plot of Zn vs Cd without transformation
# plot(df$Zn, df$Cd, pch = 19)
# abline(lm(df$Cd ~ df$Zn), col = "red")
# 
# #plot of Zn vs Cd woth transformation
# plot(log10(df$Zn), log10(df$Cd), pch = 19)
# abline(lm(log10(df$Cd)~ log10(df$Zn)), col = "red")
# 
# # corolation of all data
# #the data is too larg to make a pairs plot with it
# pairs(df[10:14])
# #we must search a way to find correlation between elemnt and the categoritical data
# 
# #pie chart of ni in all three countries
# #install.packages('plotrix')
# par(mfrow=c(1,1))
# library(plotrix)
# ni.rus <- subset(df$Ni, df$COUN == 'RUS' )
# ni.nor <- subset(df$Ni, df$COUN == 'NOR' )
# ni.fin <- subset(df$Ni, df$COUN == 'FIN' )
# ni.rus.sum <- sum(ni.rus)
# ni.nor.sum <- sum(ni.nor)
# ni.fin.sum <- sum(ni.fin)
# ni.all <- data.frame( name= c("RUS", "FIN", "NOR"),Ni = c(ni.rus.sum, ni.fin.sum, ni.nor.sum) )
# pie(ni.all$Ni,ni.all$name)
# #3d pie chart
# pie3D(ni.all$Ni, labels = ni.all$name,explide = 0.1, main = "Pie Chart of Ni in Countries ")
# #3d scaterplot
# #install.packages("scatterplot3d")
# #install.packages('GGally')
# library(scatterplot3d)
# colors <- c("#999999", "#E69F00")
# 
# 
# #### multivariant scaterplot ########
# 
# #install.packages("GGally")
# library(ggplot2)
# #make a dataframe with country and our consideration of important elements
# df.coun.el<-data.frame("COUN" = df$COUN, "Ni"= df$Ni, "Cu"= df$Cu, "Cd"= df$Cd,
#                        "As"= df$As, "Zn"= df$Zn, "Pb"= df$Pb, "Hg"= df$Hg)
# ggpairs(df.coun.el)
# ggpairs(df.radioactive.zone)
# ggpairs(df.radioactive.veg.zone)
# 
# 
# ####TEST-scotter3d####
# #install.packages("scatterplot3d")
# library(scatterplot3d)
# shapes = c(16, 17, 18) 
# shapes <- shapes[as.numeric(df.coun.el$COUN)]
# par(mfrow=c(1,1))
# scatterplot3d(df.coun.el$Ni, df.coun.el$Cu, pch = shapes)
# scatterplot3d(log10(df.coun.el$Ni), log10(df.coun.el$Cu), pch = shapes)
# #the title of lab must be changed
# #install.packages('shapes')
# library(shapes)
# shapes = c(16, 17, 18)
# shapes <- shapes[as.numeric(df$COUN)]
# colors <- c("#999999", "#E69F00", '#E500aa')
# colors <- colors[as.numeric(df$COUN)]
# length(colors)
# length(shapes)
# length(df$COUN)
# scatterplot3d(df[1:4], angle = 60,main="3D Scatter Plot",
#               xlab = "Sepal Length (cm)",
#               ylab = "Sepal Width (cm)",
#               zlab = "Petal Length (cm)", color = colors, pch = shapes )
# 
# 
# 
# #####
# boxplot(df$Ni ~ df$COUN)
# boxplot(log10(df$Ni) ~ df$COUN)
# plot(log10(df$Ni) ~ df$VEG_ZONE)
# plot(U ~ df$VEG_ZONE)
# plot(Th ~ df$VEG_ZONE)
# #install.packages('lattice')
# library(lattice)
# xyplot(log10(df$Ni) ~ log10(df$Zn) | df$COUN)
# xyplot(log10(df$Ni) ~ log10(df$Cu) | df$VEG_ZONE)
# xyplot(U ~ Th |df$VEG_ZONE)
# ########average concentration#########
# # a) countries
# #install.packages("PerformanceAnalytics")
# library(PerformanceAnalytics)
# df.coun.el
# aggregate(df.coun.el[,2:7], list(df.coun.el$COUN), mean)
# 
# # b) vegetation
# df.veg_zone.el <- data.frame("VEG_ZONE"=df$VEG_ZONE,
#                              "Ni"=df$Ni, "Cu"=df$Cu, 
#                              "Cd"=df$Cd, "As"=df$As,
#                              "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
# df.veg_zone.el
# aggregate(df.veg_zone.el[,2:7], list(df.veg_zone.el$VEG_ZONE), mean)
# 
# # c) lithologies
# df.lito.el <- data.frame("LITO"=df$LITO,
#                              "Ni"=df$Ni, "Cu"=df$Cu, 
#                              "Cd"=df$Cd, "As"=df$As,
#                              "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
# length(df.lito.el$LITO)
# df.lito.el <- df.lito.el[!is.na(df.lito.el$LITO), ]
# df.lito.el
# length(df.lito.el$LITO)
# 
# #change the class number according to the project sheet to 4 group
# df.lito.el$LITO[df.lito.el$LITO=="51"]<-"A"
# df.lito.el$LITO[df.lito.el$LITO=="52"]<-"B"
# df.lito.el$LITO[df.lito.el$LITO=="81"]<-"C"
# df.lito.el$LITO[df.lito.el$LITO=="82"]<-"C"
# df.lito.el$LITO[df.lito.el$LITO=="83"]<-"C"
# df.lito.el$LITO[df.lito.el$LITO == "7"]<-"D"
# is.numeric(df.lito.el[1, 1])
# df.lito.len <- length(df.lito.el$LITO)
# df.lito.len
# #split out the other LITOS if they are numeric
# df.lito.4groups<-df.lito.el[(df.lito.el$LITO == "A" | df.lito.el$LITO == "B" | df.lito.el$LITO == "C" | df.lito.el$LITO == "D"),]
# df.lito.4groups
# 
# #i <- 1
# #for (i in 1:df.lito.len ){
#   #if (df.lito.el[i, 1] != "A" & df.lito.el[i, 1] != "B" & df.lito.el[i, 1] != "C" & df.lito.el[i, 1] != "D"){
#     #df.lito.el <- df.lito.el[-i, ]
#   #}
#   #i = i + 1
# #}
# #print(df.lito.el)
# #df.lito.el
# aggregate(df.lito.4groups[,2:7], list(df.lito.4groups$LITO), mean)
# 
# # c) GROUNGVEG
# df.graoundveg.el <- data.frame("GROUNDVEG"=df$GROUNDVEG,
#            "Ni"=df$Ni, "Cu"=df$Cu, 
#            "Cd"=df$Cd, "As"=df$As,
#            "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
# df.graoundveg.el
# aggregate(df.graoundveg.el[,2:7], list(df.graoundveg.el$GROUNDVEG), mean)
# 
# ########---- END ---- C --#########
# 
# ######consentration#####TEST####
# install.packages('spatialrisk')
# library(spatialrisk)
# df <- data.frame(location = c("p1", "p2"), lon = c(6.561561, 6.561398), lat = c(53.21369, 53.21326))
# concentration(df, Groningen, value = amount, radius = 100)
# 
# 
# 
# 
# ##(3) regrassion###
# #train the data for corolation## split out all numeruc data
# library(ggplot2)
# #install.packages('ggthemes')
# library(ggthemes)
# library(dplyr)
# install.packages("corrgram")
# library(corrgram)
# install.packages("corrplot")
# library(corrplot)
# #check all the numeric cols
# num.df.cols <- sapply(df, is.numeric)
# #check the corolation of all numeric data
# num.df <- cor(df[,num.df.cols])
# library(tidyverse)
# df %>% select_if(is.numeric)->df.num
# #install.packages("corrr")
# head(df.num)
# library(corrr)
# library(ggplot2)
# x <- df.num %>% correlate() %>% focus(Bi)
# x %>% 
#   mutate(rowname = factor(rowname, levels = rowname[order(Bi)])) %>%  # Order by correlation strength
#   ggplot(aes(x = rowname, y = Bi)) +
#   geom_bar(stat = "identity") +
#   ylab("Correlation with Bi") +
#   xlab("Numeric Variables")
# ###############
# hist(df$Ni)
# hist(log10(ohorizon$Ni))
# head(df)
# qplot(Cu,data=df,geom='histogram',binwidth=0.1,alpha=0.8)
# hist(ohorizon$Cu)
# hist(log10(ohorizon$Cu))
# shapiro.test(log10(ohorizon$Cu))
# shapiro.test(ohorizon$Cu)
# summary(ohorizon$Ni)
# sd(ohorizon$Ni)
# boxplot(log10(ohorizon$Ni))
# plot(density(ohorizon$Ni))
# plot(ecdf(ohorizon$Ni))
# plot(ohorizon$Ni,ohorizon$Cu)
# plot(log10(ohorizon$Ni),log10(ohorizon$Cu))
# pairs(ohorizon[36:40])
# 
# #install.packages("PerformanceAnalytics")
# #library(PerformanceAnalytics)
# chart.Correlation(ohorizon[25:40])
# split.ohorizon <-data.frame("Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
# chart.Correlation(split.ohorizon)
# split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE, "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG, "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
# mean(ohorizon$Ni)
# mean(split.ohorizon1$Ni)
# 
# aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$COUN), mean)
# aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$VEG_ZONE), mean)
# aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$GROUNDVEG), mean)
# ###############