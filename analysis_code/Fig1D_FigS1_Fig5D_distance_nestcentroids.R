setwd("/Users/matteorossi/Desktop/")

library(dplyr)
library(usedist)



#### Fig1D #5min transfer

# Read table
centroids <- read.csv(
  "Fig1D_centroids_5mintransfer.txt",
  header = TRUE,
  sep = ""
)

# Make matrix of spatial coordinates (x, y)
centroids_ma <- as.matrix(centroids[, c(3:4)])

# Calculate distances
distances <- dist(
  centroids_ma,
  method = "euclidean",
  upper = TRUE,
  diag = TRUE
)

# Calculate distances between specific rows
origin_row <- c(1, 3)
destination_row <- c(2, 4)

dist_get(distances, origin_row, destination_row)

# Extract centroids from the same arena
origin_row <- seq(1, 71, by = 2)
destination_row <- seq(2, 72, by = 2)

distancestrue <- dist_get(
  distances,
  origin_row,
  destination_row
)

# Get all permutations between the two sets
b <- expand.grid(origin_row, destination_row)

origin_row2 <- b$Var1
destination_row2 <- b$Var2

distancesperm <- dist_get(
  distances,
  origin_row2,
  destination_row2
)



# PLOT: % OBSERVATIONS

# Remove missing and non-finite values
distancestrue <- distancestrue[is.finite(distancestrue)]
distancesperm <- distancesperm[is.finite(distancesperm)]

# Define common bin boundaries
bin_width <- 100

breaks_common <- seq(
  0,
  ceiling(max(c(distancestrue, distancesperm)) / bin_width) * bin_width,
  by = bin_width
)

# Calculate histograms using identical bins
hist_true <- hist(
  distancestrue,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

hist_perm <- hist(
  distancesperm,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

# Calculate relative frequencies (%)
relfreq_true <- hist_true$counts / sum(hist_true$counts) * 100
relfreq_perm <- hist_perm$counts / sum(hist_perm$counts) * 100

# Check that frequencies sum to 100%
cat("Observed relative frequencies sum to:",
    sum(relfreq_true), "%\n")

cat("Permutation relative frequencies sum to:",
    sum(relfreq_perm), "%\n")


# Plot observed relative frequencies
plot(
  NA,
  xlim = c(0, 1300),
  ylim = c(0, 60),
  xlab = "Distance (cm)",
  ylab = "% observations",
  main = "",
  xaxt = "n",
  yaxt = "n"
)

# X-axis: pixels displayed as centimeters
axis(
  side = 1,
  at = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
  labels = c("0", "2.5", "5", "7.5")
)

# Y-axis: fixed ticks at 0%, 25%, and 50%
axis(
  side = 2,
  at = c(0, 25, 50),
  labels = c("0", "25", "50"),
  las = 1
)

# Draw observed histogram bars
rect(
  xleft = hist_true$breaks[-length(hist_true$breaks)],
  ybottom = 0,
  xright = hist_true$breaks[-1],
  ytop = relfreq_true,
  col = "black",
  border = "white"
)

# Overlay permutation bars
rect(
  xleft = hist_perm$breaks[-length(hist_perm$breaks)],
  ybottom = 0,
  xright = hist_perm$breaks[-1],
  ytop = relfreq_perm,
  col = adjustcolor("gray", alpha.f = 0.4),
  border = "white"
)


#stat compare observed and permuted
wilcox.test(distancesperm,distancestrue)







#####Fig 1d #24h transfer


# Read table
centroids <- read.csv(
  "Fig1D_centroids_24htransfer.txt",
  header = TRUE,
  sep = ""
)
# Make matrix of spatial coordinates (x, y)
centroids_ma <- as.matrix(centroids[, c(3:4)])

# Calculate distances 
distances <- dist(
  centroids_ma,
  method = "euclidean",
  upper = TRUE,
  diag = TRUE
)

# Calculate distances between specific rows
origin_row <- c(1, 3)
destination_row <- c(2, 4)

dist_get(distances, origin_row, destination_row)

# Extract centroids from the same arena
origin_row <- seq(1, 71, by = 2)
destination_row <- seq(2, 72, by = 2)

distancestrue <- dist_get(
  distances,
  origin_row,
  destination_row
)

# Get all permutations between the two sets
b <- expand.grid(origin_row, destination_row)

origin_row2 <- b$Var1
destination_row2 <- b$Var2

distancesperm <- dist_get(
  distances,
  origin_row2,
  destination_row2
)



# PLOT: % OBSERVATIONS

# Remove missing and non-finite values
distancestrue <- distancestrue[is.finite(distancestrue)]
distancesperm <- distancesperm[is.finite(distancesperm)]

# Define common bin boundaries
bin_width <- 100

breaks_common <- seq(
  0,
  ceiling(max(c(distancestrue, distancesperm)) / bin_width) * bin_width,
  by = bin_width
)

# Calculate histograms using identical bins
hist_true <- hist(
  distancestrue,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

hist_perm <- hist(
  distancesperm,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

# Calculate relative frequencies (%)
relfreq_true <- hist_true$counts / sum(hist_true$counts) * 100
relfreq_perm <- hist_perm$counts / sum(hist_perm$counts) * 100

# Check that frequencies sum to 100%
cat("Observed relative frequencies sum to:",
    sum(relfreq_true), "%\n")

cat("Permutation relative frequencies sum to:",
    sum(relfreq_perm), "%\n")

# Plot observed relative frequencies
plot(
  NA,
  xlim = c(0, 1300),
  ylim = c(0, 60),
  xlab = "Distance (cm)",
  ylab = "% observations",
  main = "",
  xaxt = "n",
  yaxt = "n"
)

# X-axis: pixels displayed as centimeters
axis(
  side = 1,
  at = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
  labels = c("0", "2.5", "5", "7.5")
)

# Y-axis: fixed ticks at 0%, 25%, and 50%
axis(
  side = 2,
  at = c(0, 25, 50),
  labels = c("0", "25", "50"),
  las = 1
)


# Draw observed histogram bars
rect(
  xleft = hist_true$breaks[-length(hist_true$breaks)],
  ybottom = 0,
  xright = hist_true$breaks[-1],
  ytop = relfreq_true,
  col = "black",
  border = "white"
)

# Overlay permutation bars
rect(
  xleft = hist_perm$breaks[-length(hist_perm$breaks)],
  ybottom = 0,
  xright = hist_perm$breaks[-1],
  ytop = relfreq_perm,
  col = adjustcolor("gray", alpha.f = 0.4),
  border = "white"
)



#stat compare observed and permuted
wilcox.test(distancesperm,distancestrue)






########Fig S1 ##with brood/pupae

# Read table
centroids <- read.csv(
  "FigS1_centroids_withbrood.txt",
  header = TRUE,
  sep = ""
)

# Make matrix of spatial coordinates (x, y)
centroids_ma <- as.matrix(centroids[, c(3:4)])

# Calculate distances between all rows
distances <- dist(
  centroids_ma,
  method = "euclidean",
  upper = TRUE,
  diag = TRUE
)

# Calculate distances between specific rows
origin_row <- c(1, 3)
destination_row <- c(2, 4)

dist_get(distances, origin_row, destination_row)

# Extract centroids from the same arena
origin_row <- seq(1, 71, by = 2)
destination_row <- seq(2, 72, by = 2)

distancestrue <- dist_get(
  distances,
  origin_row,
  destination_row
)

# Get all permutations between the two sets
b <- expand.grid(origin_row, destination_row)

origin_row2 <- b$Var1
destination_row2 <- b$Var2

distancesperm <- dist_get(
  distances,
  origin_row2,
  destination_row2
)



# PLOT: % OBSERVATIONS

# Remove missing and non-finite values
distancestrue <- distancestrue[is.finite(distancestrue)]
distancesperm <- distancesperm[is.finite(distancesperm)]

# Define common bin boundaries
bin_width <- 100

breaks_common <- seq(
  0,
  ceiling(max(c(distancestrue, distancesperm)) / bin_width) * bin_width,
  by = bin_width
)

# Calculate histograms using identical bins
hist_true <- hist(
  distancestrue,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

hist_perm <- hist(
  distancesperm,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

# Calculate relative frequencies (%)
relfreq_true <- hist_true$counts / sum(hist_true$counts) * 100
relfreq_perm <- hist_perm$counts / sum(hist_perm$counts) * 100

# Check that frequencies sum to 100%
cat("Observed relative frequencies sum to:",
    sum(relfreq_true), "%\n")

cat("Permutation relative frequencies sum to:",
    sum(relfreq_perm), "%\n")

# Plot observed relative frequencies
plot(
  NA,
  xlim = c(0, 1300),
  ylim = c(0, 60),
  xlab = "Distance (cm)",
  ylab = "% Observations",
  main = "",
  xaxt = "n",
  yaxt = "n"
)

# X-axis: pixels displayed as centimeters
axis(
  side = 1,
  at = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
  labels = c("0", "2.5", "5", "7.5")
)

# Y-axis: fixed ticks at 0%, 25%, and 50%
axis(
  side = 2,
  at = c(0, 25, 50),
  labels = c("0", "25", "50"),
  las = 1
)


# Draw observed histogram bars
rect(
  xleft = hist_true$breaks[-length(hist_true$breaks)],
  ybottom = 0,
  xright = hist_true$breaks[-1],
  ytop = relfreq_true,
  col = "black",
  border = "white"
)

# Overlay permutation bars
rect(
  xleft = hist_perm$breaks[-length(hist_perm$breaks)],
  ybottom = 0,
  xright = hist_perm$breaks[-1],
  ytop = relfreq_perm,
  col = adjustcolor("gray", alpha.f = 0.4),
  border = "white"
)


#stat compare observed and permuted
wilcox.test(distancesperm,distancestrue)





#### Fig S1 ##without brood/pupae


# Read table
centroids <- read.csv(
  "FigS1_centroids_withoutbrood.txt",
  header = TRUE,
  sep = ""
)

# Make matrix of spatial coordinates (x, y)
centroids_ma <- as.matrix(centroids[, c(3:4)])

# Calculate distances between all rows
distances <- dist(
  centroids_ma,
  method = "euclidean",
  upper = TRUE,
  diag = TRUE
)

# Calculate distances between specific rows
origin_row <- c(1, 3)
destination_row <- c(2, 4)

dist_get(distances, origin_row, destination_row)

# Extract centroids from the same arena
origin_row <- seq(1, 69, by = 2)
destination_row <- seq(2, 70, by = 2)

distancestrue <- dist_get(
  distances,
  origin_row,
  destination_row
)

# Get all permutations between the two sets
b <- expand.grid(origin_row, destination_row)

origin_row2 <- b$Var1
destination_row2 <- b$Var2

distancesperm <- dist_get(
  distances,
  origin_row2,
  destination_row2
)



# PLOT: % OBSERVATIONS

# Remove missing and non-finite values
distancestrue <- distancestrue[is.finite(distancestrue)]
distancesperm <- distancesperm[is.finite(distancesperm)]

# Define common bin boundaries
bin_width <- 100

breaks_common <- seq(
  0,
  ceiling(max(c(distancestrue, distancesperm)) / bin_width) * bin_width,
  by = bin_width
)

# Calculate histograms using identical bins
hist_true <- hist(
  distancestrue,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

hist_perm <- hist(
  distancesperm,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

# Calculate relative frequencies (%)
relfreq_true <- hist_true$counts / sum(hist_true$counts) * 100
relfreq_perm <- hist_perm$counts / sum(hist_perm$counts) * 100

# Check that frequencies sum to 100%
cat("Observed relative frequencies sum to:",
    sum(relfreq_true), "%\n")

cat("Permutation relative frequencies sum to:",
    sum(relfreq_perm), "%\n")

# Plot observed relative frequencies
plot(
  NA,
  xlim = c(0, 1300),
  ylim = c(0, 60),
  xlab = "Distance (cm)",
  ylab = "% observations",
  main = "",
  xaxt = "n",
  yaxt = "n"
)

# X-axis: pixels displayed as centimeters
axis(
  side = 1,
  at = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
  labels = c("0", "2.5", "5", "7.5")
)

# Y-axis: fixed ticks at 0%, 25%, and 50%
axis(
  side = 2,
  at = c(0, 25, 50),
  labels = c("0", "25", "50"),
  las = 1
)


# Draw observed histogram bars
rect(
  xleft = hist_true$breaks[-length(hist_true$breaks)],
  ybottom = 0,
  xright = hist_true$breaks[-1],
  ytop = relfreq_true,
  col = "black",
  border = "white"
)

# Overlay permutation bars
rect(
  xleft = hist_perm$breaks[-length(hist_perm$breaks)],
  ybottom = 0,
  xright = hist_perm$breaks[-1],
  ytop = relfreq_perm,
  col = adjustcolor("gray", alpha.f = 0.4),
  border = "white"
)


#stat compare observed and permuted
wilcox.test(distancesperm,distancestrue)






#### Fig 5d # different clonal lines

# Read table
centroids <- read.csv(
  "Fig5D_centroids_clonal_lines.txt",
  header = TRUE,
  sep = ""
)

# Make matrix of spatial coordinates (x, y)
centroids_ma <- as.matrix(centroids[, c(3:4)])

# Calculate distances between all rows
distances <- dist(
  centroids_ma,
  method = "euclidean",
  upper = TRUE,
  diag = TRUE
)

# Calculate distances between specific rows
origin_row <- c(1, 3)
destination_row <- c(2, 4)

dist_get(distances, origin_row, destination_row)

# Extract centroids from the same arena
origin_row <- seq(1, 71, by = 2)
destination_row <- seq(2, 72, by = 2)

distancestrue <- dist_get(
  distances,
  origin_row,
  destination_row
)

# Get all permutations between the two sets
b <- expand.grid(origin_row, destination_row)

origin_row2 <- b$Var1
destination_row2 <- b$Var2

distancesperm <- dist_get(
  distances,
  origin_row2,
  destination_row2
)


# PLOT: % OBSERVATIONS


# Remove missing and non-finite values
distancestrue <- distancestrue[is.finite(distancestrue)]
distancesperm <- distancesperm[is.finite(distancesperm)]

# Define common bin boundaries
bin_width <- 100

breaks_common <- seq(
  0,
  ceiling(max(c(distancestrue, distancesperm)) / bin_width) * bin_width,
  by = bin_width
)

# Calculate histograms using identical bins
hist_true <- hist(
  distancestrue,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

hist_perm <- hist(
  distancesperm,
  breaks = breaks_common,
  plot = FALSE,
  include.lowest = TRUE
)

# Calculate relative frequencies (%)
relfreq_true <- hist_true$counts / sum(hist_true$counts) * 100
relfreq_perm <- hist_perm$counts / sum(hist_perm$counts) * 100

# Check that frequencies sum to 100%
cat("Observed relative frequencies sum to:",
    sum(relfreq_true), "%\n")

cat("Permutation relative frequencies sum to:",
    sum(relfreq_perm), "%\n")

# Plot observed relative frequencies
plot(
  NA,
  xlim = c(0, 1300),
  ylim = c(0, 60),
  xlab = "Distance (cm)",
  ylab = "% observations",
  main = "",
  xaxt = "n",
  yaxt = "n"
)

# X-axis: pixels displayed as centimeters
axis(
  side = 1,
  at = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
  labels = c("0", "2.5", "5", "7.5")
)

# Y-axis: fixed ticks at 0%, 25%, and 50%
axis(
  side = 2,
  at = c(0, 25, 50),
  labels = c("0", "25", "50"),
  las = 1
)


# Draw observed histogram bars
rect(
  xleft = hist_true$breaks[-length(hist_true$breaks)],
  ybottom = 0,
  xright = hist_true$breaks[-1],
  ytop = relfreq_true,
  col = "black",
  border = "white"
)

# Overlay permutation bars
rect(
  xleft = hist_perm$breaks[-length(hist_perm$breaks)],
  ybottom = 0,
  xright = hist_perm$breaks[-1],
  ytop = relfreq_perm,
  col = adjustcolor("gray", alpha.f = 0.4),
  border = "white"
)


#stat compare observed and permuted
wilcox.test(distancesperm,distancestrue)


