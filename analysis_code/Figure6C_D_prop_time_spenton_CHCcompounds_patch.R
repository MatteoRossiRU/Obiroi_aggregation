library(ggplot2)
library(dplyr)


setwd("/Users/matteorossi/Desktop/")
df <- read.table("Fig5C_6C_6D_compounds.txt", sep="", header = TRUE)


###KRUSKAL WALLIS DOSE TESTING
## 11,15-DiMeC27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("11_15_DimeC27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "11_15_DimeC27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                    
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +    
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.88



###3,9_DiMeC27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("3_9_DimeC27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "3_9_DimeC27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                    
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +    
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.57



#####13-MeC27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("13_MeC27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "13_MeC27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                  
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +   
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.24



#### C27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("C27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "C27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                   
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +     
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.08



####11Me-C27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("11_MeC27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "11_MeC27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                    
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +    
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.09


###2Me-C25

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("2_MeC25"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "2_MeC25"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                   
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +    
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.41


###3Me-C27

# Filter for only the group of interest
df_subset <- subset(df, exp %in% c("3_MeC27"))

# Set factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "3_MeC27"))

# Make sure dose_mg is treated as a factor
df_subset$dose_mg <- as.factor(df_subset$dose_mg)

# ---- Boxplot of prop_time_in_circle by dose_mg ----
library(ggplot2)

ggplot(df_subset, aes(x = dose_mg, y = prop_time_in_circle)) +
  geom_boxplot(outlier.shape = NA) +                 
  geom_jitter(width = 0.1, alpha = 0.6, size = 1) +   
  labs(
    x = "dose_mg",
    y = "prop_time_in_circle",
    title = "prop_time_in_circle by dose_mg"
  ) +
  theme_classic()

# ---- Kruskal–Wallis test ----
kruskal_result <- kruskal.test(prop_time_in_circle ~ dose_mg, data = df_subset)
kruskal_result
#p-value = 0.67







###### FIG 6C


df <- read.table("Fig5C_6C_6D_compounds.txt", sep="", header = TRUE)

# Filter for only the groups of interest
df_subset <- subset(df, exp %in% c(
  "CHCextract",
  "11_15_DimeC27",
  "3_9_DimeC27",
  "13_MeC27",
  "C27",
  "2_MeC25",
  "3_MeC27",
  "11_MeC27",
  "hexane"
))

#Order
df_subset$exp <- factor(df_subset$exp, levels = c(
  "CHCextract",
  "11_15_DimeC27",
  "3_9_DimeC27",
  "13_MeC27",
  "C27",
  "2_MeC25",
  "3_MeC27",
  "11_MeC27",
  "hexane"
))


ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  # Boxplot 
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # Jittered raw data points 
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  
  # Overlay faint median line (above jittered dots)
  stat_summary(fun = median, geom = "crossbar",
               width = 0.65, fatten = 0, color = "gray70", linewidth = 1.3) +
  
  # Mean point overlay
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  
  # Custom color palette (red tone)
  scale_fill_manual(values = c(
    "CHCextract" = "red",
    "11_15_DimeC27" = "red",
    "hexane" = "red",
    "3_9_DimeC27" = "red",
    "13_MeC27" = "red",
    "C27" = "red",
    "11_MeC27" = "red",
    "2_MeC25" = "red",
    "3_MeC27" = "red"
  )) +
  scale_color_manual(values = c(
    "CHCextract" = "red",
    "11_15_DimeC27" = "red",
    "hexane" = "red",
    "3_9_DimeC27" = "red",
    "13_MeC27" = "red",
    "C27" = "red",
    "11_MeC27" = "red",
    "2_MeC25" = "red",
    "3_MeC27" = "red"
  )) +
  
  # Labels
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time in the patch"
  ) +
  
  # Clean theme: no grating, slim axes, enhanced readability
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.4, color = "black"),  # thinner axes
    axis.ticks = element_line(size = 0.3, color = "black"), # thinner ticks
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 13),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # centered, bold title
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


#STATS
kruskal_result <- kruskal.test(prop_time_in_circle~exp , data = df_subset)
kruskal_result


# Pairwise comparisons of CHC extract and individual compounds
# against hexane using Wilcoxon rank-sum tests
# Bonferroni-Holm correction

# Groups to compare against hexane
groups_to_test <- c(
  "CHCextract",
  "11_15_DimeC27",
  "3_9_DimeC27",
  "13_MeC27",
  "C27",
  "2_MeC25",
  "3_MeC27",
  "11_MeC27"
)

# Run one Wilcoxon test for each group against hexane
wilcox_results <- lapply(groups_to_test, function(group) {
  
  df_group <- subset(df, exp %in% c(group, "hexane"))
  
  test <- wilcox.test(
    prop_time_in_circle ~ exp,
    data = df_group,
    exact = FALSE
  )
  
  data.frame(
    comparison = paste(group, "vs hexane"),
    W = unname(test$statistic),
    p_value = test$p.value
  )
})

# Combine results
wilcox_results <- bind_rows(wilcox_results)

# Apply Bonferroni-Holm correction
wilcox_results$p_adjusted <- p.adjust(
  wilcox_results$p_value,
  method = "holm"
)

# Display results
wilcox_results






###### FIG 6D


######## 11,15-DimeC7 vs mix 3 most abundant
df <- read.table("Fig5C_6C_6D_compounds.txt", sep="", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, exp %in% c("mix_ratiobiroi", "11_15_DimeC27"))


# Set the desired factor level order
df_subset$exp <- factor(df_subset$exp, levels = c( "mix_ratiobiroi", "11_15_DimeC27"))


ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  # Boxplot
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black") +
  
  # Raw data points
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  
  # Overlay a faint median (gray line) above the dots
  stat_summary(fun = median, geom = "crossbar",
               width = 0.65, fatten = 0, color = "gray70", linewidth = 1.3) +
  
  # Mean point overlay
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  
  # Custom fill and color schemes
  scale_fill_manual(values = c(
    "mix_ratiobiroi" = "red", 
    "11_15_DimeC27" = "red"
  )) +
  scale_color_manual(values = c(
    "mix_ratiobiroi" = "red", 
    "11_15_DimeC27" = "red"
  )) +
  
  ylim(0, 0.9) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion Time in the patch"
  ) +
  
  # Clean theme: no grating, slimmer axes
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.4),  # thinner axes
    axis.ticks = element_line(color = "black", linewidth = 0.3), # thinner ticks
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)




