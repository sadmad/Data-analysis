# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 1                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 1 ----------------------------------------------------------

# Enter data into R
dauer <- c(8, 17, 2, 3, 31, 7, 14, 1, 1, 11)

# Costs of the individual calls 
dauer*49

# Overall costs
sum(dauer*49)

# in Euro
dauer*49/100
# Overall costs in Euro
sum(dauer*49)/100



# -------- Exercise 2 ----------------------------------------------------------

# Calls longer than 10 minutes? 
which(dauer > 10)

# How many in total?
sum(dauer > 10)

# Mean duration of calls
mean(dauer)

# Median duration of calls
median(dauer)

# Variance and standard deviation
var(dauer)
sd(dauer)



# -------- Exercise 3 ----------------------------------------------------------

# Generate data
daten <- rnorm(10, mean = 10, sd = 1)
daten

# logarithmize
log(daten)

# larger 2.3?
log(daten) > 2.3

# only third, fifth and ninth observation?
log(daten[c(3,5,9)]) > 2.3

# all 3 larger 2.3?
sum(log(daten[c(3,5,9)]) > 2.3) == 3

# What is rnorm() doing?
help(rnorm)



# -------- Exercise 4 ----------------------------------------------------------


# (a)

# Name and vegetarian (yes/no)
vegi <- c(Andrea = "yes", Eike = "no", Julia = "yes", Linda = "no",
Tobi = "no")
vegi
str(vegi)

# as factor
vegi.f <- factor(vegi)
vegi.f
str(vegi.f)

# or numeric/binary as 0/1 variable 
# Underlying integer coding is 1,2
vegi.n <- as.numeric(vegi.f)
vegi.n
# Change to coding 0,1, e.g. by
vegi.n <- abs(vegi.n - 2)
vegi.n

# as data.frame
vegiDF <- data.frame(Name = c("Andrea","Eike", "Julia", "Linda", "Tobi"), 
                   Vegetarian = c("yes", "no", "yes", "no", "no"))
vegiDF
str(vegiDF)
summary(vegiDF)


# (b)

# Julia changed her mind and eats meat 
vegi["Julia"]
vegi["Julia"] <- "no"
vegi

# or
vegiDF[3,2] <- "no"

# or even better
vegiDF[vegiDF$Name == "Julia", "Vegetarian"] <- "no"
vegiDF



# -------- Exercise 5 ----------------------------------------------------------


# (a)

setwd("C:/Datenanalyse/Übung/")

vegi.txt <- read.table("Vegi.txt", header=TRUE)
vegi.txt
summary(vegi.txt)


# (b)

vegi.csv <- read.csv2("Vegi.csv", header=TRUE)
vegi.csv
summary(vegi.csv)


# (c)

# Change Vegi for Julia 
vegi.csv[3,2] <- "no"
vegi.csv
summary(vegi.csv)

# or (compare to above)
vegi.csv[vegi.csv$Name == "Julia", "Vegi"] <- "no"
vegi.csv
summary(vegi.csv)

# save as *.csv 
write.csv2(vegi.csv, file="Vegi2.csv")
vegi.new <- read.csv()