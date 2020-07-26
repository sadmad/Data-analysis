library(StatDA)
library(ggplot2)
library(PerformanceAnalytics)
data(ohorizon)
df <- ohorizon

#add the data to variable
summary(df)
head(df)

##### (a) ##### 
#split the df to the important elements
df.elem <-data.frame("Ni"=df$Ni, "Cu"=df$Cu, "Cd"=df$Cd, "As"=df$As, "Zn"=df$Zn, "Pb"=df$Pb, "Hg"=df$Hg)
#make plot of the important element
plot(df.elem)

#first method of showing correlation
df.cor <- cor(df.elem)

#install.packages("GGally")
library(GGally)
ggcorr(df.cor)
#secont method of visualizing correlation
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df.elem)
# plot of Zn and CU
plot(log10(df$Zn), log10(df$Cu), pch = 19)
abline(lm(log10(df$Cu) ~ log10(df$Zn)), col = "red")

#plot of Zn vs Cd without transformation
plot(df$Zn, df$Cd, pch = 19)
abline(lm(df$Cd ~ df$Zn), col = "red")

#plot of Zn vs Cd woth transformation
plot(log10(df$Zn), log10(df$Cd), pch = 19)
abline(lm(log10(df$Cd)~ log10(df$Zn)), col = "red")

# corolation of all data
#the data is too larg to make a pairs plot with it
pairs(df[10:14])
#we must search a way to find correlation between elemnt and the categoritical data

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
#install.packages('GGally')
library(scatterplot3d)
colors <- c("#999999", "#E69F00")


#### multivariant scaterplot ########

#install.packages("GGally")
library(ggplot2)
#make a dataframe with country and our consideration of important elements
df.coun.el<-data.frame("COUN" = df$COUN, "Ni"= df$Ni, "Cu"= df$Cu, "Cd"= df$Cd,
                       "As"= df$As, "Zn"= df$Zn, "Pb"= df$Pb, "Hg"= df$Hg)
ggpairs(df.coun.el)
abline(df.coun.el, col = "red")

####TEST-scotter3d####
#install.packages("scatterplot3d")
library(scatterplot3d)
shapes = c(16, 17, 18) 
shapes <- shapes[as.numeric(df.coun.el$COUN)]
par(mfrow=c(1,1))
scatterplot3d(df.coun.el$Ni, df.coun.el$Cu, pch = shapes)
scatterplot3d(log10(df.coun.el$Ni), log10(df.coun.el$Cu), pch = shapes)
#the title of lab must be changed
#install.packages('shapes')
library(shapes)
shapes = c(16, 17, 18)
shapes <- shapes[as.numeric(df$COUN)]
colors <- c("#999999", "#E69F00", '#E500aa')
colors <- colors[as.numeric(df$COUN)]
length(colors)
length(shapes)
length(df$COUN)
scatterplot3d(df[1:4], angle = 60,main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)", color = colors, pch = shapes )



#####
boxplot(df$Ni ~ df$COUN)
boxplot(log10(df$Ni) ~ df$COUN)
plot(log10(df$Ni) ~ df$VEG_ZONE)
#install.packages('lattice')
library(lattice)
xyplot(log10(df$Ni) ~ log10(df$Zn) | df$COUN)
xyplot(log10(df$Ni) ~ log10(df$Cu) | df$VEG_ZONE)

########average concentration#########
# a) countries
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
df.coun.el
aggregate(df.coun.el[,2:7], list(df.coun.el$COUN), mean)

# b) vegetation
df.veg_zone.el <- data.frame("VEG_ZONE"=df$VEG_ZONE,
                             "Ni"=df$Ni, "Cu"=df$Cu, 
                             "Cd"=df$Cd, "As"=df$As,
                             "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
df.veg_zone.el
aggregate(df.veg_zone.el[,2:7], list(df.veg_zone.el$VEG_ZONE), mean)

# c) lithologies
df.lito.el <- data.frame("LITO"=df$LITO,
                             "Ni"=df$Ni, "Cu"=df$Cu, 
                             "Cd"=df$Cd, "As"=df$As,
                             "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
length(df.lito.el$LITO)
df.lito.el <- df.lito.el[!is.na(df.lito.el$LITO), ]

length(df.lito.el$LITO)


df.lito.el$LITO[df.lito.el$LITO=="51"]<-"A"
df.lito.el$LITO[df.lito.el$LITO=="52"]<-"B"
df.lito.el$LITO[df.lito.el$LITO=="81"]<-"C"
df.lito.el$LITO[df.lito.el$LITO=="82"]<-"C"
df.lito.el$LITO[df.lito.el$LITO=="83"]<-"C"
df.lito.el$LITO[df.lito.el$LITO == "7"]<-"D"

length(df.lito.el$LITO)

aggregate(df.lito.el[,2:7], list(df.lito.el$LITO), mean)

# c) GROUNGVEG
df.graoundveg.el <- data.frame("GROUNDVEG"=df$GROUNDVEG,
           "Ni"=df$Ni, "Cu"=df$Cu, 
           "Cd"=df$Cd, "As"=df$As,
           "Zn"=df$Zn,"Pb"=df$Pb, "Hg"=df$Hg)
df.graoundveg.el
aggregate(df.graoundveg.el[,2:7], list(df.graoundveg.el$GROUNDVEG), mean)

########---- END ---- C --#########

######consentration#####TEST####
install.packages('spatialrisk')
library(spatialrisk)
df <- data.frame(location = c("p1", "p2"), lon = c(6.561561, 6.561398), lat = c(53.21369, 53.21326))
concentration(df, Groningen, value = amount, radius = 100)




##(3) regrassion###
#train the data for corolation## split out all numeruc data
library(ggplot2)
#install.packages('ggthemes')
library(ggthemes)
library(dplyr)
install.packages("corrgram")
library(corrgram)
install.packages("corrplot")
library(corrplot)
#check all the numeric cols
num.df.cols <- sapply(df, is.numeric)
#check the corolation of all numeric data
num.df <- cor(df[,num.df.cols])
library(tidyverse)
df %>% select_if(is.numeric)->df.num
#install.packages("corrr")
head(df.num)
library(corrr)
library(ggplot2)
x <- df.num %>% correlate() %>% focus(Bi)
x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(Bi)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = Bi)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Bi") +
  xlab("Numeric Variables")
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

#install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
chart.Correlation(ohorizon[25:40])
split.ohorizon <-data.frame("Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
chart.Correlation(split.ohorizon)
split.ohorizon1 <-data.frame("COUN"= ohorizon$COUN,"VEG_ZONE"=ohorizon$VEG_ZONE, "LITO"=ohorizon$LITO,"GROUNDVEG"=ohorizon$GROUNDVEG, "Ni"=ohorizon$Ni, "Cu"=ohorizon$Cu, "Cd"=ohorizon$Cd, "As"=ohorizon$As, "Zn"=ohorizon$Zn, "Pb"=ohorizon$Pb, "Hg"=ohorizon$Hg)
mean(ohorizon$Ni)
mean(split.ohorizon1$Ni)

aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$COUN), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$VEG_ZONE), mean)
aggregate(split.ohorizon1[, 5:11], list(split.ohorizon1$GROUNDVEG), mean)
###############