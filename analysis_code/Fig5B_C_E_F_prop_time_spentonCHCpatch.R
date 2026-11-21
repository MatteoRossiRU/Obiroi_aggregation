library(ggplot2)
library(dplyr)


setwd("/Users/matteorossi/Desktop/")


###Fig 5B #YoungF ants CHC Obiroi vs CHCApicea

df <- read.table("Fig5B_CHC_Obiroi_vs_Apicea.txt", sep = "", header = TRUE)

df_subset <- subset(df, exp %in% c("Obiroi_CHC",
                                   "Apicea_CHC"))

df_subset$exp <- factor(
  df_subset$exp,
  levels = c(
    "Obiroi_CHC",
    "Apicea_CHC"
  )
)

# calculate group means
means <- df_subset %>%
  group_by(exp) %>%
  summarise(mean_val = mean(as.numeric(prop_time_in_circle), na.rm = TRUE))

# assign diamond color depending on group
means$diamond_col <- ifelse(means$exp %in% c("Obiroi_CHC","Apicea_CHC"),
                            "black", "red")
                            
ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # add group mean diamonds
  geom_point(data = means,
             aes(x = exp, y = mean_val),
             shape = 23, size = 1.8,
             fill = "black", color = means$diamond_col) +
  scale_fill_manual(values = c(
    "Obiroi_CHC" = "#e41a1c",
    "Apicea_CHC" = "#e41a1c"
  )) +
  scale_color_manual(values = c(
    "Obiroi_CHC" = "#e41a1c",
    "Apicea_CHC" = "#e41a1c"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time in the patch"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(size = 0.4),
    axis.ticks        = element_line(size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(face = "bold")
  )


wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)




####Fig 5C #mix unfamiliar vs hexane

df <- read.table("Fig5C_6C_6D_compounds.txt", sep = "", header = TRUE)

df_subset <- subset(df, exp %in% c("mix_unfamiliar","hexane"))

df_subset$exp <- factor(
  df_subset$exp,
  levels = c(
    "mix_unfamiliar",
    "hexane"
  )
)


# calculate group means
means <- df_subset %>%
  group_by(exp) %>%
  summarise(mean_val = mean(as.numeric(prop_time_in_circle), na.rm = TRUE))

# assign diamond color depending on group
means$diamond_col <- ifelse(means$exp %in% c("mix_unfamiliar","hexane"),
                            "black", "red")
                            
ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # add group mean diamonds
  geom_point(data = means,
             aes(x = exp, y = mean_val),
             shape = 23, size = 1.8,
             fill = "black", color = means$diamond_col) +
  scale_fill_manual(values = c(
    "mix_unfamiliar" = "#e41a1c",
    "hexane" = "#e41a1c"
  )) +
  scale_color_manual(values = c(
    "mix_unfamiliar" = "#e41a1c",
    "hexane" = "#e41a1c"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time in the patch"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(size = 0.4),
    axis.ticks        = element_line(size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(face = "bold")
  )


wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)




#########Fig 5E

df <- read.table("Fig5E_CHClineBvsM.txt", sep = "", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, which_patch %in% c("CHC_b","CHC_m"))

# Order factor levels
df_subset$which_patch <- factor(
  df_subset$which_patch,
  levels = c("CHC_b", "CHC_m")
)

ggplot(df_subset, aes(x = which_patch, y = as.numeric(prop_time_in_circle), fill = which_patch)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = which_patch), width = 0.2, size = 1.5, alpha = 0.8) +
  # mean per group: smaller red diamond
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8,   # reduced size
    fill = "black", color = "black"
  ) +
  scale_fill_manual(values = c(
    "CHC_b" = "#4D4D4D",
    "CHC_m"  = "#CC79A7"
  )) +
  scale_color_manual(values = c(
    "CHC_b" = "#4D4D4D",
    "CHC_m"  = "#CC79A7"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Which patch",
    y = "Time Both"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),             # no grid
    axis.line         = element_line(size = 0.4),    # lighter axes
    axis.ticks        = element_line(size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(face = "bold")
  )


#Wilcoxon paired test
df <- read.table("Fig5E_CHClineBvsM.txt",
                 sep = "", header = TRUE)

# Keep only the two groups
df_subset <- subset(df, which_patch %in% c("CHC_b", "CHC_m"))

# Convert measurements to numeric (non-numeric values become NA)
df_subset$prop_time_in_circle <- as.numeric(
  as.character(df_subset$prop_time_in_circle)
)

df_wide <- reshape(
  df_subset,
  idvar = "Trialnum_1montholdRnum",
  timevar = "which_patch",
  direction = "wide"
)

#Remove incomplete pairs
df_test <- na.omit(df_wide)

# Paired Wilcoxon test
wilcox.test(
  df_test$prop_time_in_circle.CHC_b,
  df_test$prop_time_in_circle.CHC_m,
  paired = TRUE,
  exact = FALSE
)





###Fig  5F

df <- read.table("Fig5F_CHClineBvsA.txt", sep = "", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, which_patch %in% c("CHC_b","CHC_a"))

# Order factor levels
df_subset$which_patch <- factor(
  df_subset$which_patch,
  levels = c("CHC_b", "CHC_a")
)


ggplot(df_subset, aes(x = which_patch, y = as.numeric(prop_time_in_circle), fill = which_patch)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = which_patch), width = 0.2, size = 1.5, alpha = 0.8) +
  # mean per group: smaller diamond
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8,   # reduced size
    fill = "black", color = "black"
  ) +
  scale_fill_manual(values = c(
    "CHC_b" = "#4D4D4D",
    "CHC_a"  = "#009E73"
  )) +
  scale_color_manual(values = c(
    "CHC_b" = "#4D4D4D",
    "CHC_a"  = "#009E73"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Which patch",
    y = "Prop time in circle"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),             # no grid
    axis.line         = element_line(size = 0.4),    # lighter axes
    axis.ticks        = element_line(size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(face = "bold")
  )



# Paired Wilcoxon test
df_wide <- reshape(
  df_subset,
  idvar = "Trialnum_1montholdRnum",
  timevar = "which_patch",
  direction = "wide"
)

wilcox.test(
  df_wide$prop_time_in_circle.CHC_b,
  df_wide$prop_time_in_circle.CHC_a,
  paired = TRUE,
  exact = FALSE
)
