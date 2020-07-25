# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 8                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 26 ---------------------------------------------------------

# Data 
x <- c(1, 5, 3, 8, 2, 2, 10, 8, 7, 4)
y <- c(0.5, 2.9, 0.6, 3.0, 1.5, 1.1, 3.9, 2.5, 3.1, 1.2)


# (a)

# Scatter plot
plot(x,y, xlim = c(0,10), ylim = c(0,4))

# Correlation
cor(x,y)


# (b)

# Linear Regression
lm26 <- lm(y ~ x)
lm26



# Regression line
abline(lm26$coef, lty = 2, col = 2)


# (c)

# more details on regression fit
summary(lm26)



# (d)

# manually, e.g. 
lm26$coef[1] + lm26$coef[2]*5.5

# or simpler/preferable
pred <- predict(lm26, newdata = data.frame(x=c(5.5, 6, 0, 2)))
pred

abline(v = c(5.5, 6, 0, 2), lty = 3)
abline(h = pred, lty = 3)
points(c(5.5, 6, 0, 2), pred, pch = 3, lwd = 3)


# (e)

# Specifically the first three plots are of interest/most relevant 
plot(lm26)



# -------- Exercise 27 ---------------------------------------------------------


help(trees)
trees


# (a)

# The relationship of Girth and Volume is apparently much stronger 
# than the association of Height and Volume.
par(mfrow=c(1,2))
plot(Volume ~ Girth, data=trees)
plot(Volume ~ Height, data=trees)
par(mfrow=c(1,1))

cor(trees)


# (b)

# The models
mG <- lm(Volume ~ Girth, data=trees)
mH <- lm(Volume ~ Height, data=trees)

# # coefficients, tests, R-squared, etc.
summary(mG)
summary(mH)


# Not reasonable to interpret intercept (Girth and/or Height =0 not appropriate)

# When girth is increased by one unit, i.e., one inch, Volume is expected to
# increase/increases on average by 5 units, i.e., 5 cubic ft.

# When Height is increased by one unit, i.e., one ft, Volume is expected to
# increase/increases on average by 1.5 units, i.e., 1.5 cubic ft.

# Coefficients of determination:
# 93.5% of the variation of Volume can be explained by Girth, but just 35.8%
# by Height.




# (c)

par(mfrow=c(1,2))
plot(Volume ~ Girth, data=trees)
abline(mG$coef, lty=2)
plot(Volume ~ Height, data=trees)
abline(mH$coef, lty=2)


# (d)

plot(trees$Volume, mG$fitted, xlab="observed", ylab="fitted",
main="Predictor Girth")
abline(c(0,1),lty=3)
plot(trees$Volume, mH$fitted, xlab="observed", ylab="fitted",
main="Predictor Height")
abline(c(0,1),lty=3)
par(mfrow=c(1,1))


# (e)

mGH <- lm(Volume ~ Girth + Height, data=trees)
summary(mGH)

# When Height is kept constant and Girth in increased by one unit, Volume
# is expected to increase/increases on average by 4.7 cubic ft.

# When Girth is kept constant and Height in increased by one unit, Volume
# is expected to increase/increases on average by 0.34 cubic ft.




# (f)

# Girth and Height combined explain more than 94% of the variation of Volume, 
# see value of R^2 in above output.
# Furthermore, the model is significantly better than the model from (c);
# see t-tests above, and F-tests below:
anova(mG, mGH)
anova(mH, mGH)

# We test the hypotheses
# H0: beta-height=0

# and in the second call
# H0: beta-girth=0 



# (g)

# The first three plots are particularly useful:
plot(mGH)


# (h)

mGHlog <- lm(log(Volume) ~ log(Girth) + log(Height), data=trees)

# The model fit is even better than in (e):
summary(mGHlog)

# Model assumptions are fulfilled:
plot(mGHlog)



# -------- Exercise 28 ---------------------------------------------------------


# Data set used for the output
library(MASS)

whiteside

# Linear model to produce full output
linmod <- lm(Gas ~ Temp + Insul, data=whiteside)
summary(linmod)



# (a)

# beta-hat-temp: Reading values manually from the output
(beta.temp <- 0.01776 * (-18.95))



# t-value insul: Manually
(t.insul <- (-1.56520) / 0.09705)




# (b)  + (c)

# See discussion in the lesson.  





