library(ggplot2)

setwd("/Users/matteorossi/Desktop/")



####Fig 4B
#### young vs old CHC
setwd("/Users/matteorossi/Desktop/")
df <- read.table("Fig4B_FigS6B_youngvsold_CHC_hexane.txt", sep = "", header = TRUE)

df_subset <- subset(df, exp %in% c("CHC_youngF","CHC_oldF"))


# Order factor levels
df_subset$exp <- factor(
  df_subset$exp,
  levels = c("CHC_youngF", "CHC_oldF")
)

library(ggplot2)
library(dplyr)
library(grid)

# calculate group means
means <- df_subset %>%
  group_by(exp) %>%
  summarise(mean_val = mean(as.numeric(prop_time_in_circle), na.rm = TRUE))

# assign diamond color (mean)
means$diamond_col <- ifelse(means$exp %in% c("CHC_youngF","CHC_oldF"),
                            "black", "black")
                            
ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # add group mean diamonds
  geom_point(data = means,
             aes(x = exp, y = mean_val),
             shape = 23, size = 1.8,
             fill = "black", color = means$diamond_col) +
  scale_fill_manual(values = c(
    "CHC_oldF"    = "#377eb8",
    "CHC_youngF" = "#e41a1c"
  )) +
  scale_color_manual(values = c(
    "CHC_oldF"    = "#377eb8",
    "CHC_youngF" = "#e41a1c"
  )) +
  labs(
    title = "Time comparison",
    x = "Experimental Group",
    y = "Proportion time spent in patch"
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
#pval= 0.0002 



####Fig S6B
####young vs old hexane
df <- read.table("Fig4B_FisS6B_youngvsold_CHC_hexane.txt", sep = "", header = TRUE)

df_subset <- subset(df, exp %in% c("hex_oldF","hex_youngF"))


# Order factor levels
df_subset$exp <- factor(
  df_subset$exp,
  levels = c("hex_youngF", "hex_oldF")
)


# calculate group means
means <- df_subset %>%
  group_by(exp) %>%
  summarise(mean_val = mean(as.numeric(prop_time_in_circle), na.rm = TRUE))

# assign diamond color depending on group
means$diamond_col <- ifelse(means$exp %in% c("hex_oldF","hex_youngF"),
                            "black", "black")
                            
ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # add group mean diamonds
  geom_point(data = means,
             aes(x = exp, y = mean_val),
             shape = 23, size = 1.8,
             fill = "black", color = means$diamond_col) +
  scale_fill_manual(values = c(
    "hex_oldF"    = "#377eb8",
    "hex_youngF" = "#e41a1c"
  )) +
  scale_color_manual(values = c(
    "hex_oldF"    = "#377eb8",
    "hex_youngF" = "#e41a1c"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time spent in the patch"
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
#pval = 0.12







#Fig 4C
#CHC  #oldR vs oldF

df <- read.table("Fig4C_FigS6C_oldRvsoldF_CHC_hexane.txt", sep = "", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, exp %in% c("CHC_forag","CHC_reprod"))

# Order factor levels
df_subset$exp <- factor(
  df_subset$exp,
  levels = c("CHC_reprod", "CHC_forag")
)

library(ggplot2)
library(grid)

ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # mean per group: smaller red diamond
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8,   # reduced size
    fill = "black", color = "black"
  ) +
  scale_fill_manual(values = c(
    "CHC_reprod" = "orange",
    "CHC_forag"  = "#377eb8"
  )) +
  scale_color_manual(values = c(
    "CHC_reprod" = "orange",
    "CHC_forag"  = "#377eb8"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time in spent in circle"
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

wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)






####Fig S6C
####oldR vs oldF hexane

df <- read.table("Fig4C_FigS6C_oldRvsoldF_CHC_hexane.txt", sep = "", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, exp %in% c("hex_forag","hex_reprod"))

# Order factor levels
df_subset$exp <- factor(
  df_subset$exp,
  levels = c("hex_reprod", "hex_forag")
)

library(ggplot2)
library(grid)   # for unit()

ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # mean per group: smaller red diamond
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8,   # reduced size
    fill = "black", color = "black"
  ) +
  scale_fill_manual(values = c(
    "hex_forag"  = "#377eb8",
    "hex_reprod" = "orange"
  )) +
  scale_color_manual(values = c(
    "hex_forag"  = "#377eb8",
    "hex_reprod" = "orange"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time spent in circle"
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


wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)








#Fig 4D
#CHC #Aged matched R vs F
df <- read.table("Fig4D_agematched_RvsF_CHC.txt", sep = "", header = TRUE)

# Filter for only the two groups of interest
df_subset <- subset(df, exp %in% c("CHC_forag","CHC_reprod"))

# Order factor levels
df_subset$exp <- factor(
  df_subset$exp,
  levels = c("CHC_reprod", "CHC_forag")
)

library(ggplot2)
library(grid)   # for unit()

ggplot(df_subset, aes(x = exp, y = as.numeric(prop_time_in_circle), fill = exp)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(color = exp), width = 0.2, size = 1.5, alpha = 0.8) +
  # mean per group: smaller red diamond
  stat_summary(
    fun = mean, geom = "point",
    shape = 23, size = 1.8,   # reduced size
    fill = "black", color = "black"
  ) +
  scale_fill_manual(values = c(
    "CHC_reprod" = "gold3",
    "CHC_forag"  = "cadetblue3"
  )) +
  scale_color_manual(values = c(
    "CHC_reprod" = "gold3",
    "CHC_forag"  = "cadetblue3"
  )) +
  labs(
    title = "Time Spent Comparison",
    x = "Experimental Group",
    y = "Proportion time in circle"
  ) +
  ylim(0, 1) +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),             # no grid
    axis.line         = element_line(size = 0.4),    # lighter axes
    axis.ticks        = element_line(size = 0.3),
    axis.ticks.length = unit(2, "pt"),
    axis.title        = element_text(face = "bold")
  )

wilcox.test(as.numeric(prop_time_in_circle) ~ exp, data = df_subset, exact = FALSE)



