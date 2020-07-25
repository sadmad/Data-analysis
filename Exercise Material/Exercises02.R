# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 2                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------




# -------- Exercise 6 ----------------------------------------------------------


# Enter data into R
x <- c("E", "D", "A", "D", "C", "D", "B", "E", "C", "E", "D", "B", "B", "C",
"B", "A", "B", "C", "C", "C", "A", "B", "C", "A", "A", "C", "A", "E", "E", "A")

# or (for more advanced R users)
help("strsplit")
x <- "E D A D C D B E C E D B B C B A B C C C A B C A A C A E E A"
x <- strsplit(x, split=" ")
x <- x[[1]]
# or
x <- unlist(x)

# or
x <- "EDADCDBECEDBBCBABCCCABCAACAEEA"
x <- unlist(strsplit(x, split=""))


# as factor
x <- factor(x)
x


# (a)

# Frequency table
table(x) # would also work without conversion to factor 

# or
summary(x) # works only with conversion to factor in advance 

# Relative frequencies
table(x)/length(x)


# (b)

# Barplot
help(barplot)
barplot(table(x))

# or e.g. 
barplot(table(x), horiz=TRUE, col=c(3,4,2,1,6), las=1)

# pie chart
pie(table(x), col=c(3,4,2,1,6))


# (c)

# with ggplot2 possible without using table, as respective plot function
# automatically uses frequencies in case a factor variable is handed over

library(ggplot2)
qplot(x)
# or
qplot(x, fill=x)



# -------- Exercise 7 ----------------------------------------------------------


# dataset
help(HairEyeColor)
HairEyeColor

# Men and women jointly 
HairEyeColor[,,1] + HairEyeColor[,,2]
HEC.all <- HairEyeColor[,,1] + HairEyeColor[,,2]

# Barplot
cx <- c(rgb(0/255,0/255,0/255), rgb(163/255,118/255,67/255),
rgb(235/255,88/255,14/255), rgb(247/255,239/255,186/255))  # nicer colors

barplot(HEC.all, beside=TRUE, legend=TRUE,
col=cx)
# or
barplot(HEC.all, col=cx, legend=TRUE)

# separatly for eye color and gender 
par(mfrow=c(2,2))
barplot(HairEyeColor[,1,], beside=TRUE, main="Brown", col=cx)
barplot(HairEyeColor[,2,], beside=TRUE, main="Blue", col=cx)
barplot(HairEyeColor[,3,], beside=TRUE, main="Hazel", col=cx)
barplot(HairEyeColor[,4,], beside=TRUE, main="Green", col=cx)
par(mfrow=c(1,1))

# or
par(mfrow=c(1,2))
barplot(HairEyeColor[,,1], beside=FALSE, main="Male", legend=TRUE, col=cx)
barplot(HairEyeColor[,,2], beside=FALSE, main="Female", legend=TRUE, col=cx)
par(mfrow=c(1,1))



# -------- Exercise 8 ----------------------------------------------------------

# Package
library(cluster)


# (a)

# Helping pages
help(agriculture)

# dataset
agriculture

# Make variable names "visible", that is 
# attach data frame to R search path 
# but before attaching it into search path delete previous variable x to avoid clashes
# with the column-name x in the data frame agriculture
rm(x)
attach(agriculture)


# (b)


# Scatter plot
plot(x, y, xlab="per capita GNP", ylab="perct. pop. in agriculture")

# or
library(ggplot2)
qplot(x, y, xlab="per capita GNP", ylab="perct. pop. in agriculture") + stat_smooth()

# or
library(lattice)
xyplot(y ~ x, xlab="per capita GNP", ylab="perct. pop. in agriculture")


# (c)

# new variable
detach(agriculture)
n <- nrow(agriculture)
region <- c("north", "north", "north", "south", "south", "south", 
            "north", "south" , "north", "north", "south" ,"north")

# or
region <- rep("north",n)
region[rownames(agriculture)%in%c("P","E","I","GR","F")] <- "south"
region

# add the new column to data frame
agriculture$region <- region
agriculture
summary(agriculture)

# better
agriculture$region <- factor(region)
summary(agriculture)

# remove old variable region from workspace 
rm(region)

# Attach data frame to search path/make it "visible"
attach(agriculture)
summary(region)


# (d)

par(mfrow=c(1,2))
boxplot(x ~ region, main="per capita GNP")
boxplot(y ~ region, main="perct. pop. in agriculture")
par(mfrow=c(1,1))

# or with lattice
# par() does not work for lattice, use lattice graphics-layout facilities instead 

?plot.trellis

bxp.x <- bwplot(x ~ region, main="per capita GNP")
bxp.y <- bwplot(y ~ region, main="perct. pop. in agriculture")


plot(bxp.x, split=c(1,1,2,1), more=TRUE)
plot(bxp.y, split=c(2,1,2,1), more=FALSE)


# or with ggplot2
# par() does not work either
# Several packages available that provide graphics-layout facilities for ggplot2
# graphics, e.g. 
library(gridExtra)

plot.x <- qplot(region, x, geom="boxplot", ylab="per capita GNP")
plot.y <- qplot(region, y, geom="boxplot", ylab="perct. pop. in agriculture")

grid.arrange(plot.x, plot.y, ncol=2)



# (e)

# "complicated" version
par(mfrow=c(1,2))
plot(x[region=="north"],y[region=="north"], xlab="per capita GNP", 
     ylab="perct. pop. in agriculture")
title("North")
plot(x[region=="south"],y[region=="south"], xlab="per capita GNP", 
     ylab="perct. pop. in agriculture")
title("South")
par(mfrow=c(1,1))


# simple approach with ggplot2 and/or lattice
qplot(x, y, color=region, xlab="per capita GNP", ylab="perct. pop. in agriculture")

# or
xyplot(y ~ x | region, xlab="per capita GNP", ylab="perct. pop. in agriculture")

# Arrangement of panels with argument layout=c(#cols, #rows, #pages)
# default arrangement
xyplot(y ~ x | region, xlab="per capita GNP", ylab="perct. pop. in agriculture", layout=c(2,1,1))
# or other way around
xyplot(y ~ x | region, xlab="per capita GNP", ylab="perct. pop. in agriculture", layout=c(1,2,1))



# -------- Exercise 9 ----------------------------------------------------------


# Detach data frame from search path
# Delete region from data frame
detach(agriculture)
agriculture <- agriculture[,-3]
agriculture
# or
agriculture$region <- NULL

# Change (col-) names
names(agriculture) <- c("GNP","PopAgri")
agriculture

# plot() function in R
help(plot)

# text() function
help(text)

# call plot() without actually plotting something
attach(agriculture)
plot(GNP, PopAgri, type="n")

# Add text 
text(GNP, PopAgri, labels=rownames(agriculture))

# or
plot(GNP, PopAgri, ylim = c(0,25))
text(GNP, PopAgri + 1.5, labels=rownames(agriculture))


