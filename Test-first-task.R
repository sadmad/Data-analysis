library(StatDA)
library(ggplot2)
library(PerformanceAnalytics)
df <- ohorizon
data(ohorizon)
#add the data to variable
summary(df)
head(df)
#split the df to the important elements
df.elem <-data.frame("Ni"=df$Ni, "Cu"=df$Cu, "Cd"=df$Cd, "As"=df$As, "Zn"=df$Zn, "Pb"=df$Pb, "Hg"=df$Hg)
#make plot of the important element
plot(df.elem)
cor(df.elem)
# corolation of all data
#the data is too larg to make a pairs plot with it
pairs(df[10:14])
#we must search a way to find correlation between elemnt and the categoritical data
chart.Correlation(df.elem)
#pie chart of ni in all three countries
#install.packages('plotrix')
library(plotrix)
ni.rus <- subset(df$Ni, df$COUN == 'RUS' )
ni.nor <- subset(df$Ni, df$COUN == 'NOR' )
ni.fin <- subset(df$Ni, df$COUN == 'FIN' )
ni.rus.sum <- sum(ni.rus)
ni.nor.sum <- sum(ni.nor)
ni.fin.sum <- sum(ni.fin)
ni.all <- data.frame( name= c("RUS", "FIN", "NOR"),Ni = c(ni.rus.sum, ni.fin.sum, ni.nor.sum) )
pie(ni.all$Ni,ni.all$name)
#3d pie chart
pie3D(ni.all$Ni, labels = ni.all$name,explide = 0.1, main = "Pie Chart of Ni in Countries ")
#3d scaterplot
#install.packages("scatterplot3d")
library(scatterplot3d)
colors <- c("#999999", "#E69F00")

ggpairs(df.coun.el)

#### multivariant scaterplot ########

#install.packages("GGally")
library(GGally)
library(ggplot2)
#make a dataframe with country and our consideration of important elements
df.coun.el<-data.frame("COUN" = df$COUN, "Ni"= df$Ni, "Cu"= df$Cu, "Cd"= df$Cd,
                       "As"= df$As, "Zn"= df$Zn, "Pb"= df$Pb, "Hg"= df$Hg)
ggpairs(df.coun.el)


#the title of lab must be changed
#install.packages('shapes')
library(shapes)
shapes = c(16, 17)
shapes <- shapes[as.numeric(df$Ni)]
colors <- c("#999999", "#E69F00")
colors <- colors[as.numeric(log10(df$Cu))]
scatterplot3d(log10(df$Ni),log10(df$Cu), angle = 60,main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)", color = colors )
#####
boxplot(df$Ni ~ df$COUN)
boxplot(log10(df$Ni) ~ df$COUN)
plot(log10(df$Ni) ~ df$VEG_ZONE)
xyplot(log10(df$Ni) ~ log10(df$Zn) | df$COUN)
xyplot(log10(df$Ni) ~ log10(df$Cu) | df$VEG_ZONE)


####TEST-scotter3d####
data(iris)
tail(iris)
shapes = c(16, 17, 18) 
shapes <- shapes[as.numeric(iris$Species)]
shapes
scatterplot3d(iris[,1:3], pch = shapes)
###############
hist(df$Ni)
hist(log10(ohorizon$Ni))
head(df)
qplot(Cu,data=df,geom='histogram',binwidth=0.1,alpha=0.8)
hist(ohorizon$Cu)
hist(log10(ohorizon$Cu))
shapiro.test(log10(ohorizon$Cu))
shapiro.test(ohorizon$Cu)
summary(ohorizon$Ni)
sd(ohorizon$Ni)
boxplot(log10(ohorizon$Ni))
plot(density(ohorizon$Ni))
plot(ecdf(ohorizon$Ni))
plot(ohorizon$Ni,ohorizon$Cu)
plot(log10(ohorizon$Ni),log10(ohorizon$Cu))
pairs(ohorizon[36:40])
library("PerformanceAnalytics")
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(ohorizon[25:40])
split.ohorizon <-data.frame("Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
library("PerformanceAnalytics")
chart.Correlation(split.ohorizon)
split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE, "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG, "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
mean(ohorizon$Ni)
mean(split.ohorizon1$Ni)

aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$COUN), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$VEG_ZONE), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$GROUNDVEG), mean)
###############