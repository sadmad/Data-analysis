#install.packages("StatDA")
library(StatDA)
data(ohorizon)

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages(c("rgl", "car"))
library(car)
library(rgl)
split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE,
                             "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG,
                             "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd,
                             "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb,
                             "Hg"=ohorizon$Hg, "U"=ohorizon$U, "Bi"=ohorizon$Bi,
                             "Co"=ohorizon$Co, "Cr"=ohorizon$Cr, "Th"=ohorizon$Th,
                             "S"=ohorizon$S, "SO4"=ohorizon$SO4, "Ti"=ohorizon$Ti,
                             "Au"=ohorizon$Au,  "Sb"=ohorizon$Sb,  "V"=ohorizon$V,
                             "Pd"=ohorizon$Pd, "Pt"=ohorizon$Pt, 
                             "pH"=ohorizon$pH, "XCOO"=ohorizon$XCOO,
                             "YCOO"=ohorizon$YCOO, "ELEV"=ohorizon$ELEV)


#Bi with Pb, Cd, As, Ti
#Pd, Au, Ti, SO4 many NaNs
Ti <- log10(split.ohorizon1$Ti)
shapiro.test(Ti)
U <- log10(split.ohorizon1$U)
shapiro.test(U)
Bi <- log10(split.ohorizon1$Bi)
shapiro.test(Bi)
Co <- (split.ohorizon1$Co)^-0.28
shapiro.test(Co)
Cr <- (split.ohorizon1$Cr)^-0.26
shapiro.test(Cr)
Th <- log10(split.ohorizon1$Th)
shapiro.test(Th)
S <- log10(split.ohorizon1$S)
shapiro.test(S)
Ni <- (split.ohorizon1$Ni)^-0.379
shapiro.test(Ni)
Cu <- (split.ohorizon1$Cu)^-0.7
shapiro.test(Cu)
Cd <- log10(split.ohorizon1$Cd)
shapiro.test(Cd)
As <- log10(split.ohorizon1$As)
shapiro.test(As)
Zn <- log10(split.ohorizon1$Zn)
shapiro.test(Zn)
Pb <- log10(split.ohorizon1$Pb)
shapiro.test(Pb)
Hg <- log10(split.ohorizon1$Hg)
shapiro.test(Hg)
Pd <- (split.ohorizon1$Pd)^-0.18
shapiro.test(Pd)
pH <- split.ohorizon1$pH
XCOO <- split.ohorizon1$XCOO
YCOO <- split.ohorizon1$YCOO
ELEV <- split.ohorizon1$ELEV


def <- data.frame(Ti,U,Bi,Co,Cr,Th,S,Ni,Cu,Cd,As,Zn,Pb,Hg,Pd,Pd.log,pH,XCOO,YCOO,ELEV)
chart.Correlation(def)

#chart.Correlation(split.ohorizon1[5:22])
#chart.Correlation(log10(split.ohorizon1[5:24]))
new <- data.frame("Bi" = log10(ohorizon$Bi), log10(ohorizon[,65:85]))
new
chart.Correlation(new)

#scatter3d(Ni,Cu,Pb,point.col = "red", surface.alpha= 1)
#Possible values for fit are "linear", "quadratic", "smooth" and "additive"
#scatter3d(Ni,Cu,Pb, groups = split.ohorizon1$VEG_ZONE, grid = FALSE, fit = "linear", surface = FALSE,
#          ellipsoid = TRUE, ellipsoid.alpha= 0.6)
#scatter3d(Pb,Hg,Zn)
#install.packages("RColorBrewer")
library(RColorBrewer)
hist(Ni)
hist(Cu)
hist(Co)
hist(Pd)
plot(Pd,Ni)

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
split.ohorizon1$order = findInterval(split.ohorizon1$Pd, sort(split.ohorizon1$Pd))

scatter3d(Ni,Cu,Co, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)


cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
split.ohorizon1$order = findInterval(split.ohorizon1$As, sort(split.ohorizon1$As))

scatter3d(Ni,Cu,Zn, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
split.ohorizon1$order = findInterval(split.ohorizon1$As, sort(split.ohorizon1$As))

scatter3d(Ni, Cu, Co, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
split.ohorizon1$order = findInterval(split.ohorizon1$Cd, sort(split.ohorizon1$Cd))

scatter3d(Ni, Cu, As, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

scatter3d(Cd, Zn, Pb, grid = FALSE, grid.col = "black", fit = "linear", surface = FALSE, fill = TRUE,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

scatter3d(Hg, Zn, Cd, grid = FALSE, grid.col = "black", fit = "linear", surface = TRUE, fill = TRUE,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)
###Bi testing
#Bi with Pb, Cd, As, scheinbar doch nicht Ti

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
split.ohorizon1$order = findInterval(split.ohorizon1$Cd, sort(split.ohorizon1$Cd))

scatter3d(As, Bi, Pb, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

#box cox transform

library(MASS)

############(Ti,U,Bi,Co,Cr,Th,S,Ni,Cu,Cd,As,Zn,Pb,Hg,Pd,Pd.log,pH,XCOO,YCOO,ELEV)

#Ni

linearmodel = lm(Ni ~ Cu, data = ohorizon)
plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Ni)^-.5 ~ Cu, data = ohorizon)
plot(fullmodel.inv)
#0.379

Ni.0.3 <- (ohorizon$Ni)^-0.3
#hist(Ni.0.3)
shapiro.test((Ni.0.3))
qqnorm(Ni.0.3)
qqline(Ni.0.3)
Ni.boxcox <- (ohorizon$Ni)^-0.379
#hist(Ni.boxcox)
shapiro.test((Ni.boxcox))
qqnorm(Ni.boxcox)
qqline(Ni.boxcox)
Ni.0.4 <- (ohorizon$Ni)^-0.4
#hist(Ni.0.4)
shapiro.test((Ni.0.4))
qqnorm(Ni.0.4)
qqline(Ni.0.4)
Ni.0.5 <- (ohorizon$Ni)^-0.5 
hist(Ni.0.5)
shapiro.test((Ni.0.5))

#install.packages("nortest")
library(nortest)
ad.test(Ni.0.4)

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
linearmodel = lm(Cr ~ Ti, data = ohorizon)
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
#boxplot(Hg.boxcox)

#Ti
#df.Ti <- data.frame(ohorizon$Ti)

df.boxcox <- data.frame(Ti,U.boxcox,Bi,Co.boxcox,Cr.boxcox,Th.boxcox,S.boxcox,Ni.boxcox,Cu.boxcox,Cd.boxcox,As.boxcox,Zn.boxcox,Pb.boxcox,Hg.boxcox,Pd.boxcox,pH,XCOO,YCOO,ELEV)
chart.Correlation(df.boxcox)
#3D/4D Scatterplot
library(car)
library(rgl)
library(RColorBrewer)

#Ni,Cu,Co,Pd
hist(Ni.boxcox)
hist(Cu.boxcox)
hist(Co.boxcox)
hist(Pd.boxcox)
plot(Co.boxcox,Cu.boxcox)
plot(Ni.boxcox,Cu.boxcox)
plot(Ni.boxcox,Co.boxcox)



cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.boxcox$order = findInterval(df.boxcox$Pd.boxcox, sort(df.boxcox$Pd.boxcox))

scatter3d(Ni.boxcox,Cu.boxcox,Co.boxcox, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

#As, Bi, Pb, Cd
hist(As.boxcox)
hist(Bi)
hist(Pb.boxcox)
hist(Cd.boxcox)
plot(Bi,As.boxcox)
plot(Bi,Cd.boxcox)
plot(Bi,Pb.boxcox)
plot(Ti,Bi)


cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.boxcox$order = findInterval(df.boxcox$Cd.boxcox, sort(df.boxcox$Cd.boxcox))

scatter3d(As.boxcox, Pb.boxcox, Bi, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(split.ohorizon1))[split.ohorizon1$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)
