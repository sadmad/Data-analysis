# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 7                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 22 ---------------------------------------------------------

library(MASS)
data(cabbages)
plot(VitC ~ Cult, data = cabbages)


# split data according to cultivar
VitC39 <- cabbages$VitC[cabbages$Cult == "c39"]
VitC52 <- cabbages$VitC[cabbages$Cult == "c52"]

# t-test
t.test(VitC39, VitC52)

# or better
t.test(VitC ~ Cult, data=cabbages)

# with assumption of equal variances
t.test(VitC ~ Cult, data=cabbages, var.equal=TRUE)




# -------- Exercise 23 ---------------------------------------------------------

knie <- read.table("C:/Datenanalyse/Übung/knie.txt", header=T)
knie

attach(knie)

# Possibility 1: 2-sample t-Test
table(TH)
t.test(PAIN[TH==0], PAIN[TH==1])
t.test(PAIN ~ TH, data = knie)

# Possibility 2: chi2 test for homogeneity (or for independence)
table(TH,PAIN)
mosaicplot(table(TH,PAIN)[,4:1], col = heat.colors(4), main = "Pain vs. Therapy")
barplot(t(table(TH,PAIN)), col = heat.colors(4)[4:1], horiz = TRUE, 
        main = "Pain vs. Therapy")

chisq.test(table(TH,PAIN))




# -------- Exercise 24 ---------------------------------------------------------

# Running times before additional training 
before <- c(15.1, 14.3, 14.4, 13.1, 12.9, 13.8, 11.7, 12.8, 14.1, 13.6, 14.2)

# Running times after additional training
after <- c(14.9, 14.2, 14.5, 13.1, 12.6, 14.0, 11.7, 12.3, 13.8, 13.7, 14.0)

boxplot(cbind(before,after))

# t-Test dependent measurements (paired t-test)
help("t.test")
t.test(before, after, paired=TRUE, alternative="greater", conf.level = 0.9)
mean(before)
mean(after)

# or (equivalently) as 1-sample t-test on differences
diffs <- before - after
boxplot(diffs)
abline(h=0, lty=3)
t.test(diffs, alternative="greater")



# -------- Exercise 25 ---------------------------------------------------------


# Enter the data as matrix and perform chi2 test for homogeneity
A <- matrix(c(21,30,25,29,19,24),3,2)
barplot(t(A), beside = TRUE, col = 3:2, names.arg = c("A","B","C"))
chisq.test(A)




