# ------------------------------------------------------------------------------
# -------- Data Analysis and Statistical Learning ------------------------------
# -------- Exercise Sheet 4                      -------------------------------
# -------- Dr. Annette Möller                    -------------------------------
# -------- TU Clausthal                          ------------------------------- 
# ------------------------------------------------------------------------------



# -------- Exercise 12 ---------------------------------------------------------

# Dataset
headsize <-
  matrix(c(191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190,
           188, 163, 195, 186, 181, 175, 192, 174, 176, 197, 190, 155, 149, 148,
           153, 144, 157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155,
           153, 145, 140, 154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192,
           190, 189, 197, 187, 186, 174, 185, 195, 187, 161, 183, 173, 182, 165,
           185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 152, 149, 152, 159,
           151, 148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 147, 143,
           158, 150), nrow = 25, ncol = 4, 
         dimnames = list(character(0), 
                         c("head1", "breadth1", "head2", "breadth2")))
headsize <- as.data.frame(headsize)
headsize


# PCA with all variables
head_pca <- princomp(x = headsize)
head_pca
summary(head_pca, loadings = TRUE)

# Visualization of explained variance 
plot(head_pca)
screeplot(head_pca, type="l")



# -------- Exercise 13 ---------------------------------------------------------


# Enter correlation matrix 
R <- matrix(c(1.000, 0.402, 0.396, 0.301, 0.305, 0.339, 0.340,
              0.402, 1.000, 0.618, 0.150, 0.135, 0.206, 0.183,
              0.396, 0.618, 1.000, 0.321, 0.289, 0.363, 0.345,
              0.301, 0.150, 0.321, 1.000, 0.846, 0.759, 0.661,
              0.305, 0.135, 0.289, 0.846, 1.000, 0.797, 0.800,
              0.339, 0.206, 0.363, 0.759, 0.797, 1.000, 0.736,
              0.340, 0.183, 0.345, 0.661, 0.800, 0.736, 1.000), 
            nrow = 7, ncol = 7, byrow = TRUE)

rownames(R) <- colnames(R) <- c("Head length", "Head breadth", 
                                "Face breadth", "Left finger length", 
                                "Left forearm length", "Left foot length", 
                                "Height")
R


# Principal Component Analysis
PCA <- princomp(covmat = R)

# Result
summary(PCA, loadings = TRUE)

# Plot of eigenvalues/variances (Scree-Plot)
plot(PCA$sdev^2, xlab = "Component number",
     ylab = "Component variance", type = "b", main = "Scree diagram")
# or
screeplot(PCA, type="lines")
abline(h=1, lty=3)


# -------- Exercise 14 ---------------------------------------------------------


# Dataset
setwd("C:/Datenanalyse/Übung/")
measure <- read.table("measure.txt", header=T)
measure

# PCA e.g. by using
body_pc1 <- princomp(measure[, c("chest", "waist", "hips")], cor = FALSE)
# or
body_pc2 <- prcomp(measure[, c("chest", "waist", "hips")], scale = FALSE)

# Result
summary(body_pc1, loadings=TRUE)
# or
body_pc2

# Extract Principal Components and/or scores 
names(summary(body_pc2))
summary(body_pc2)$rotation
summary(body_pc2)$x

# Interpretation:
# Die erste Hauptkomponente ist im Grunde ein (gewichtetes) Mittel aus chest,
# waist und hips.
# Die zweite Hauptkomponente beschreibt im Wesentlichen den Unterschied
# zwischen Bauchumfang (waist) und Hüftumfang (hips). An einen Ende der
# Skala liegen hier Menschen mit "dickem Bauch" und "schmalen Hüften",
# am anderen Ende Menschen "ohne Bauch" aber mit "breiten Hüften".

# Interpretation:
# The first PC is simply a (weighted) average of chest, waist and hips.
# The second PC describes the difference between waist and hip girth:
# the two extremes are "people with large waist girth but small hip girth",
# and "people with small waist girth but large hip girth", with all other
# people lying inbetween these two extremes. 



# Plot of Scores
plot(body_pc2$x[,1:2], pch = as.numeric(measure$gender),
     col = as.numeric(measure$gender), xlab = "PC1", ylab = "PC2")
legend("bottomleft", levels(measure$gender), pch=1:2, col=1:2, bty="n")



# -------- Exercise 15 ---------------------------------------------------------


# Dataset
jet <- read.table("jet.txt", header=T)
jet
summary(jet)


# (a)

# reduced dataset (without indicator variable for being able to land on carrier yes/no) 
X <- jet[, c("FFD", "SPR", "RGF", "PLF", "SLF")]

# Scatter Plot Matrix
plot(X, pch = as.numeric(jet$CAR), col = as.numeric(jet$CAR))
cor(X)


# (b)

# (further) reduced dataset (for PCA)
X <- jet[, c("SPR", "RGF", "PLF", "SLF")]

# PCA
jet_pca <- prcomp(X, scale=TRUE)
jet_pca
summary(jet_pca)

# Screeplot
plot(jet_pca)
# or
screeplot(jet_pca, type="l")
abline(h=1, lty=3)



# (c)

# Scatterplot of scores on the first to principal components
plot(jet_pca$x[,1:2], pch = as.numeric(jet$CAR), col = as.numeric(jet$CAR))

# Legend
legend("bottomleft", levels(jet$CAR), pch=1:2, col=1:2)

