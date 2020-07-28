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
U <- log10(split.ohorizon1$U)
Bi <- log10(split.ohorizon1$Bi)
Co <- (split.ohorizon1$Co)^-0.28
Cr <- log10(split.ohorizon1$Cr)
Th <- log10(split.ohorizon1$Th)
S <- log10(split.ohorizon1$S)
Ni <- (split.ohorizon1$Ni)^-0.379
Cu <- (split.ohorizon1$Cu)^-0.7
Cd <- log10(split.ohorizon1$Cd)
As <- log10(split.ohorizon1$As)
Zn <- log10(split.ohorizon1$Zn)
Pb <- log10(split.ohorizon1$Pb)
Hg <- log10(split.ohorizon1$Hg)
Pd <- (split.ohorizon1$Pd)^-0.18
pH <- split.ohorizon1$pH
XCOO <- split.ohorizon1$XCOO
YCOO <- split.ohorizon1$YCOO
ELEV <- split.ohorizon1$ELEV

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
linearmodel = lm(Ni ~ Cu, data = ohorizon)
plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Ni)^-.5 ~ Cu, data = ohorizon)
plot(fullmodel.inv)
#0.379

qqnorm(log10(ohorizon$Ni))
qqline(log10(ohorizon$Ni))


Ni.0.3 <- (ohorizon$Ni)^-0.3
#hist(Ni.0.3)
shapiro.test((Ni.0.3))
qqnorm(Ni.0.3)
qqline(Ni.0.3)
Ni.0.379 <- (ohorizon$Ni)^-0.379
#hist(Ni.0.379)
shapiro.test((Ni.0.379))
qqnorm(Ni.0.379)
qqline(Ni.0.379)
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

##############Cu
library(MASS)
linearmodel = lm(Cu ~ Ni, data = ohorizon)
#plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Cu)^-.5 ~ Ni, data = ohorizon)
#plot(fullmodel.inv)
#0.379


Cu.0.55 <- (ohorizon$Cu)^-0.7
hist(Cu.0.55)
shapiro.test((Cu.0.55))
qqnorm(Cu.0.55)
qqline(Cu.0.55)

##############Co
library(MASS)
linearmodel = lm(Co ~ Pd, data = ohorizon)
#plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]


fullmodel.inv = lm((Co)^-.5 ~ Pd, data = ohorizon)
#plot(fullmodel.inv)
#0.379


Co.0.26 <- (ohorizon$Co)^-0.28
hist(Co.0.26)
shapiro.test((Co.0.26))
qqnorm(Co.0.26)
qqline(Co.0.26)


##############Pd
library(MASS)
linearmodel = lm(Pd ~ Co, data = ohorizon)
#plot(linearmodel)

bc <- boxcox(linearmodel, lambda = seq(-2,2))
best.lam = bc$x[which(bc$y == max(bc$y))]
fullmodel.inv = lm((Pd)^-.5 ~ Co, data = ohorizon)
#plot(fullmodel.inv)
#0.379


Pd.0.26 <- (ohorizon$Pd)^-0.18
hist(Pd.0.26)
shapiro.test((Pd.0.26))
qqnorm(Pd.0.26)
qqline(Pd.0.26)



