
##(3) regrassion###
#train the data for corolation## split out all numeruc data
library(ggplot2)
#install.packages('ggthemes')
library(ggthemes)
library(dplyr)
#install.packages("corrgram")
library(corrgram)
#install.packages("corrplot")
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