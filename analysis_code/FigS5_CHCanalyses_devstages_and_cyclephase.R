library(tidyverse)
library(vegan)
library(ggplot2)


#### REPROD VS FORAG
### in metadata C2 = Foraging, C5 = Reproductive

setwd("/Users/matteorossi/Desktop/")


# Load GC-MS data
chc <- read.table(
  "FigS5B_table_CHC_reproductiveVSforaging.txt",
  sep = "\t",
  header = TRUE,
  check.names = FALSE
)

# Remove the six columns containing compound information
chc2 <- chc[, -c(1:6)]


# Load metadata
id <- read.table(
  "FigS5B_metadata_reproductiveVSforaging.txt",
  sep = "\t",
  header = TRUE,
  check.names = FALSE
)


# Colony
Colony <- as.factor(id$Colony)

# Genotype
Genotype <- as.factor(id$Genotype)


# NMDS
mds1 <- metaMDS(
  t(chc2),
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = TRUE,
  noshare = 0.1,
  expand = TRUE,
  trace = 1
)

ordiplot(mds1, type = "text", display = "species")
ordiplot(mds1, type = "text", display = "sites")


# Extract NMDS scores
data.scores <- as.data.frame(scores(mds1, display = "sites"))
data.scores$ID <- rownames(data.scores)

species.scores <- as.data.frame(scores(mds1, display = "species"))
species.scores$species <- rownames(species.scores)


# Combine NMDS scores with metadata
plot.data <- data.scores %>%
  left_join(id, by = c("ID" = "ID"))


# NMDS plot
ggplot(plot.data, aes(x = NMDS1, y = NMDS2, color = Colony)) +
  geom_point(size = 3, alpha = 0.9) +
  coord_equal() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c("C2" = "blue", "C5" = "orange"),
    name = "Colony"
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  )


## Adonis stats
adonis2(
  t(chc2) ~ Colony,
  data = id,
  method = "bray"
)







#### LARVAE AND PUPAE
### in metadata C2 = Adults (foraging), C7 = Larvae, C8 = Pupae


# Load GC-MS data
chc <- read.table(
  "FigS5A_table_CHC_devstages.txt",
  sep = "\t",
  header = TRUE,
  check.names = FALSE
)

# Remove the four columns containing compound information
chc2 <- chc[, -c(1:4)]


# Load metadata
id <- read.table(
  "FigS5A_metadata_devstages.txt",
  sep = "\t",
  header = TRUE,
  check.names = FALSE
)


# Match metadata to the order of samples in the GC-MS table
id <- id[match(colnames(chc2), id$ID), ]

# Check that all samples have matching metadata
stopifnot(!any(is.na(id$ID)))


# Colony
Colony <- as.factor(id$Colony)

# Genotype
Genotype <- as.factor(id$Genotype)


# NMDS
mds1 <- metaMDS(
  t(chc2),
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = TRUE,
  noshare = 0.1,
  expand = TRUE,
  trace = 1
)

ordiplot(mds1, type = "text", display = "species")
ordiplot(mds1, type = "text", display = "sites")


# Extract NMDS scores
data.scores <- as.data.frame(scores(mds1, display = "sites"))
data.scores$ID <- rownames(data.scores)

species.scores <- as.data.frame(scores(mds1, display = "species"))
species.scores$species <- rownames(species.scores)


# Combine NMDS scores with metadata
plot.data <- data.scores %>%
  left_join(id, by = c("ID" = "ID"))


# NMDS plot
ggplot(plot.data, aes(x = NMDS1, y = NMDS2, color = Colony)) +
  geom_point(size = 3, alpha = 0.9) +
  coord_equal() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c("C2" = "blue", "C7" = "gray", "C8" = "red"),
    name = "Colony"
  ) +
  labs(
    x = "NMDS1",
    y = "NMDS2"
  )





## Pairwise PERMANOVA

# All pairwise comparisons between colonies
pairs <- list(
  c("C2", "C7"),
  c("C2", "C8"),
  c("C7", "C8")
)

# Run pairwise PERMANOVA
pairwise_results <- lapply(pairs, function(pair) {
  
  # Select samples belonging to the two colonies
  keep <- id$Colony %in% pair
  
  # Subset metadata and GC-MS data
  id_pair <- droplevels(id[keep, ])
  chc_pair <- t(chc2)[keep, , drop = FALSE]
  
  # PERMANOVA
  adonis_result <- adonis2(
    chc_pair ~ Colony,
    data = id_pair,
    method = "bray"
  )
  
  # Extract results
  data.frame(
    Comparison = paste(pair, collapse = " vs "),
    R2 = adonis_result$R2[1],
    F = adonis_result$F[1],
    P = adonis_result$`Pr(>F)`[1]
  )
})

# Combine results
pairwise_results <- do.call(rbind, pairwise_results)

# Bonferroni-Holm correction
pairwise_results$P_adjusted <- p.adjust(
  pairwise_results$P,
  method = "holm"
)

# Print results
print(pairwise_results)

