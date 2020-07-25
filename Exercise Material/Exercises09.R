# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 9                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 29 ---------------------------------------------------------


# Data
yA <- c(7, 10, 10, 12, 13, 14, 14, 14, 17, 20, 20, 23)
yB <- c(7, 11, 11, 13, 14, 16, 17, 17, 17, 19, 21, 21)
yC <- c(0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 7)
yD <- c(2, 3, 3, 4, 4, 5, 5, 5, 5, 5, 6, 12)
yE <- c(1, 1, 2, 3, 3, 3, 3, 4, 5, 5, 6, 6)
yF <- c(9, 10, 11, 13, 13, 15, 15, 16, 22, 24, 26, 26)

# Response-Vector
y <- c(yA, yB, yC, yD, yE, yF)

# Factor
spray <- rep(LETTERS[1:6],rep(12,6))
spray <- as.factor(spray)


# (a)

# Standard Boxplot
plot(spray, y)

# or
library(ggplot2)
qplot(spray, y, geom = "boxplot")

# or Dotplot/Jitter
qplot(spray, y, geom = "jitter", color = spray, shape = spray)

# Combination
qplot(spray, y, geom = c("boxplot","jitter"), color = spray, shape = spray)

# Violin plot
qplot(spray, y, geom = "violin", fill = spray)



# (b)

# ANOVA
anova(lm(y ~ spray))


# (c)

# Model diagnosis
plot(lm(y ~ spray))

# log 1 +
plot(spray, log(1+y))
anova(lm(log(1+y) ~ spray))
plot(lm(log(1+y) ~ spray))



# -------- Exercise 30 ---------------------------------------------------------



library(agridat)

help(shafii.rapeseed)
data(shafii.rapeseed)


# (a)

# Visualization
library(ggplot2)
qplot(loc, yield, geom = "boxplot", data = shafii.rapeseed, 
      fill = gen)


library(car)
leveneTest(yield~loc*gen, data=shafii.rapeseed)


# 2-way ANOVA
anova(lm(yield ~ loc*gen, data=shafii.rapeseed))


# (b)

# separated by year
qplot(loc, yield, geom = "boxplot", data = shafii.rapeseed, 
      fill = gen, facets = year ~ .)

# or
library(lattice)
dotplot(loc ~ yield | gen, groups=factor(year), auto.key = list(space = "right"),
        data=shafii.rapeseed)


# 3-way ANOVA model with all interaction terms
anova(lm(yield ~ factor(year)*loc*gen, data=shafii.rapeseed))




# -------- Exercise 31 ---------------------------------------------------------


# Data used in the output
library(agridat)

mcconway.turnip
str(mcconway.turnip)

# Planting density as factor 
mcconway.turnip$density <- factor(mcconway.turnip$density)

# 2-way Anova
anova(lm(yield ~ density*gen, data=mcconway.turnip))


# Plot
library(ggplot2)

coplot(yield ~ density | gen, data = mcconway.turnip, panel = panel.smooth,
       xlab = "turnip field trial: yield vs planting density, given genotype")

