
##(3) regrassion###
#train the data for corolation## split out all numeruc data
library(ggplot2)
#install.packages('ggthemes')
library(ggthemes)
#install.packages("dplyr")
library(dplyr)
#install.packages("corrgram")
library(corrgram)
#install.packages("corrplot")
library(corrplot)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("corrr")
library(corrr)
#install.packages("car")
library(car)
#install.packages("rgl")
library(rgl)
#install.packages("RColorBrewer")
library(RColorBrewer)




df <- df.trans.1
#check all the numeric cols
num.df.cols <- sapply(df, is.numeric)
#check the corolation of all numeric data
num.df <- cor(df[,num.df.cols])
df %>% select_if(is.numeric)->df.num
x <- df.num %>% correlate() %>% focus(log10.Bi)
x %>% 
  mutate(rowname = factor(rowname, levels = rowname[order(log10.Bi)])) %>%  # Order by correlation strength
  ggplot(aes(x = rowname, y = log10.Bi)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with Bi") +
  xlab("Numeric Variables")


#3D/4D Scatterplot

#As, Bi, Pb, Cd

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.trans$order = findInterval(df.trans$Cd.boxcox, sort(df.trans$Cd.boxcox))

scatter3d(As.boxcox, df.trans$log10.Bi, Pb.boxcox, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(df.trans))[df.trans$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

#Ni,Cu,Co,Pd

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.trans$order = findInterval(df.trans$Pd.boxcox, sort(df.trans$Pd.boxcox))

scatter3d(Ni.boxcox,Cu.boxcox,Co.boxcox, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(df.trans))[df.trans$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

#U, Th, Ti, YCOO

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.trans$order = findInterval(df.trans$Th.boxcox, sort(df.trans$Th.boxcox))

scatter3d(U.boxcox, df.trans$log10.Ti,df.trans$YCOO, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "white", surface.alpha = 0.2,
          point.col = pal(nrow(df.trans))[df.trans$order], bg.col = "black",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = "gray", fov = 60)

#As, Bi, Pb, Cd

cols = brewer.pal(8, "RdYlGn")
pal = colorRampPalette(cols)
df.trans$order = findInterval(df.trans$Cd.boxcox, sort(df.trans$Cd.boxcox))

scatter3d(As.boxcox, df.trans$log10.Bi, Pb.boxcox, grid = FALSE, grid.col = "black", fit = "linear", surface = T, fill = TRUE,
          surface.col = "black", surface.alpha = 0.1,
          point.col = pal(nrow(df.trans))[df.trans$order], bg.col = "white",
          sphere.size = 1.5, axis.scales = TRUE, axis.ticks = TRUE, axis.col = c("black","black","black"), fov = 60)

#rgl.snapshot(filename = "AsBiPbCd.png")

