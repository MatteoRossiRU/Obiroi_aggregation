# Supplementary Figure 4A

library(dplyr)
library(vegan)
library(ggplot2)

setwd("/Users/matteorossi/Desktop/")

chc <- read.table(
  "Fig5A_FigS7A_table_CHC_clonal_lines_and_nest.txt",
  sep = "\t",
  header = TRUE,
  row.names = 1,
  check.names = FALSE
)

id <- read.table(
  "Fig5A_FigS7A_metadata_clonal_lines_and_nest.txt",
  sep = "\t",
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)


clonal_ids <- id$ID[id$Nestorcuticle != "Nest"]


# Keep only the clonal-line sample columns, in metadata order.
clonal_ids <- clonal_ids[clonal_ids %in% colnames(chc)]
id_clonal <- id %>%
  filter(ID %in% clonal_ids) %>%
  mutate(Genotype = factor(Genotype, levels = c("A", "B", "D", "M"))) %>%
  slice(match(clonal_ids, ID))

# Check that the metadata and CHC data contain the same samples in the same order.
stopifnot(identical(id_clonal$ID, clonal_ids))
stopifnot(!any(id_clonal$Nestorcuticle == "Nest"))

# CHC table: rows = compounds, columns = clonal-line samples.
# Convert the selected sample columns to numeric explicitly.
clonal_chc <- as.data.frame(
  lapply(chc[, clonal_ids, drop = FALSE], as.numeric),
  check.names = FALSE
)
rownames(clonal_chc) <- rownames(chc)

clonal_samples <- t(as.matrix(clonal_chc))

set.seed(123)  

mds1 <- metaMDS(
  clonal_samples,
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = TRUE,
  noshare = 0.1,
  expand = TRUE,
  trace = 1
)

# Extract sample scores and attach the matching clonal-line metadata.
data.scores <- as.data.frame(scores(mds1, display = "sites"))
data.scores$ID <- rownames(data.scores)

data.scores <- data.scores %>%
  left_join(id_clonal, by = c("ID" = "ID"))

# Colors
# A = green, B = dark grey, D = purple, M = pink.
cols <- c(
  A = "#009E73",
  B = "#4D4D4D",
  D = "#7B3294",
  M = "#CC79A7"
)

p_nmds <- ggplot(
  data.scores,
  aes(x = NMDS1, y = NMDS2, color = Genotype)
) +
  geom_point(size = 3) +
  scale_color_manual(
    values = cols,
    breaks = c("A", "B", "D", "M"),
    drop = FALSE,
    name = "Clonal line"
  ) +
  coord_equal() +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

p_nmds



# Overall PERMANOVA: ----------

# Bray-Curtis distances among the clonal-line samples only.
vegchc <- vegdist(clonal_samples, method = "bray")

# Overall test for differences among the four clonal lines.
permanova_overall <- adonis2(
  vegchc ~ Genotype,
  data = id_clonal,
  permutations = 999
)

print(permanova_overall)



#Pairwise PERMANOVA among clonal lines

genotypes <- levels(droplevels(id_clonal$Genotype))
pair_list <- combn(genotypes, 2, simplify = FALSE)

pairwise_pvals <- do.call(
  rbind,
  lapply(pair_list, function(pair) {

    keep <- id_clonal$Genotype %in% pair
    pair_meta <- droplevels(id_clonal[keep, , drop = FALSE])
    pair_samples <- clonal_samples[pair_meta$ID, , drop = FALSE]

    pair_dist <- vegdist(pair_samples, method = "bray")

    pair_result <- adonis2(
      pair_dist ~ Genotype,
      data = pair_meta,
      permutations = 999
    )

    data.frame(
      Comparison = paste(pair, collapse = " vs "),
      R2 = pair_result$R2[1],
      P_value = pair_result$`Pr(>F)`[1],
      stringsAsFactors = FALSE
    )
  })
)

pairwise_pvals$P_adjusted <- p.adjust(
  pairwise_pvals$P_value,
  method = "holm"
)

print(pairwise_pvals)

