library(ggplot2)
library(dplyr)


###Fig 3B

solvent <- "CHC"
condition <- c("CHC", "Hexane", "Neither")
value <- c(11, 1, 1)

data <- data.frame(solvent, condition, value)

data$condition <- factor(data$condition,
                         levels = c("CHC", "Hexane", "Neither"))

data <- data %>%
  mutate(frequency = value / sum(value))

ggplot(data, aes(x = solvent, y = frequency, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("CHC" = "darkorchid1",
                               "Hexane" = "gray",
                               "Neither" = "white")) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.2)) +
  ylab("% observations") +
  theme_classic()



# Chi-square test
CHCvshexane <- chisq.test(x = c(11, 1),
                          p = c(0.5, 0.5))

CHCvsneither <- chisq.test(x = c(11, 1),
                           p = c(0.5, 0.5))

# Extract p-values
p_values <- c(CHCvshexane$p.value,
              CHCvsneither$p.value)

# Bonferroni-Holm correction
p_adjusted <- p.adjust(p_values, method = "holm")

# Results
data.frame(
  comparison = c("CHC vs Hexane", "CHC vs Neither"),
  p_value = p_values,
  p_adjusted = p_adjusted
)







###Fig S2B

#Dataset
solvent <- c(rep("Hexane", 3),
             rep("Dichloromethane", 3),
             rep("Acetonitrile", 3),
             rep("Ethanol_Methanol", 3))

condition <- rep(c("Nest", "Outside", "Neither"), 4)

value <- c(18, 8, 4,
           15, 10, 4,
           14, 13, 2,
           11, 15, 5)


data <- data.frame(solvent, condition, value)

data$solvent <- factor(data$solvent,
                       levels = c("Hexane", "Dichloromethane",
                                  "Acetonitrile", "Ethanol_Methanol"))

data$condition <- factor(data$condition,
                         levels = c("Nest", "Outside", "Neither"))

# Convert counts to frequencies within each solvent
data <- data %>%
  group_by(solvent) %>%
  mutate(frequency = value / sum(value)) %>%
  ungroup()

# Plot
ggplot(data, aes(x = solvent, y = frequency, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Nest" = "green3",
                               "Outside" = "gray",
                               "Neither" = "white")) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  ylab("% observations") +
  theme_classic()




# Chi-square test: Nest vs Outside
hexane <- chisq.test(x = c(18, 8),
                     p = c(0.5, 0.5))

dichloromethane <- chisq.test(x = c(15, 10),
                              p = c(0.5, 0.5))

acetonitrile <- chisq.test(x = c(14, 13),
                           p = c(0.5, 0.5))

ethanol_methanol <- chisq.test(x = c(11, 15),
                               p = c(0.5, 0.5))


