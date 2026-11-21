library(readxl)
library(dplyr)
library(ggplot2)


input_file <- "/Users/matteorossi/Desktop/FigS7B_aggression_counts_clonal_lines.xlsx"

df <- read_excel(input_file)

# Clean column names, because "host " has a trailing space
names(df) <- trimws(names(df))


# Pull host B data and treat focal as factor
plot_df <- df %>%
  mutate(
    host = trimws(host),
    focal = trimws(focal),
    focal = factor(focal, levels = c("B", "A", "M", "D")),
    aggressions = as.numeric(`nb of aggressions`)
  ) %>%
  filter(
    host == "B",
    focal %in% c("B", "A", "M", "D"),
    !is.na(aggressions)
  )

# Check sample sizes
plot_df %>%
  count(focal)


# Plot: 4 boxplots in one graph


my_palette <- c(
  "B" = "#4D4D4D",  # charcoal gray
  "A" = "#009E73",  # green
  "D" = "#7B3294",  # purple
  "M" = "#CC79A7"   # pink / magenta
)

p <- ggplot(plot_df, aes(x = focal, y = aggressions, color = focal)) +
  geom_boxplot(
    fill = "white",
    linewidth = 0.8,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.12,
    size = 2,
    alpha = 0.65
  ) +
  scale_color_manual(values = my_palette, drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  labs(
    x = "Focal type",
    y = "Aggression count",
    title = "Aggression of host B toward focal B, A, M, and D"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

print(p)






#STATISTICS as in Bailly et al. 2026, Curr Bio
library(lme4)
library(glmmTMB)

colnames(df)[colnames(df) == "nb of aggressions"] <- "total"
glm <- glmer(`nb of aggressions` ~ focal + colony + (1|date), family=poisson, data=df)
summary(glm)
plot(glm)

# check overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(glm)

#need to use family nbinom1
glm <- glmmTMB(total ~ focal + camera + (1|date), family='nbinom1', data=df)
summary(glm)
overdisp_fun(glm)

# effect colony
glm_colony <- glmmTMB(total ~ focal + (1|date), family='nbinom1', data=df)
anova(glm,glm_colony,test='F') # p = 0.0522 

# effect date
glm_date <- glmmTMB(total ~ focal + colony, family='nbinom1', data=df)
anova(glm,glm_date,test='F') # p = 0.003678 

#keep model glm with date but without colony as fixed factor. Model to use:
glm2 <- glmmTMB(total ~ focal + (1|date) , family='nbinom1', data=df)
summary(glm2)
plot(glm2)
comp2 <- glht(glm2, linfct = mcp(focal = "Tukey"))
summary(comp2)



# Pairwise Wilcoxon rank-sum tests with Holm-Bonferroni correction
#wilcox_results <- pairwise.wilcox.test(
#  x = plot_df$aggressions,
#  g = plot_df$focal,
#  p.adjust.method = "holm",
#  exact = FALSE
#)
#wilcox_results
