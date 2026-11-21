setwd("/Users/matteorossi/Desktop/")

library(ggplot2)
library(dplyr)
library(usedist)


# Helper function to extract distances between specified rows
dist_get <- function(dist_matrix, origin, destination) {
  distances <- numeric(length(origin))
  for (i in seq_along(origin)) {
    distances[i] <- dist_matrix[origin[i], destination[i]]
  }
  return(distances)
}

origin_row <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27)
destination_row <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)


# Read datasets
centroids_2 <- read.csv("Fig6F_centroids_11_15_DiMeC27.txt", header = TRUE, sep = "")
centroids_ma_2 <- as.matrix(centroids_2[, 2:3])
distances_2 <- as.matrix(dist(centroids_ma_2, method = "euclidean"))
distancestrue2 <- dist_get(distances_2, origin_row, destination_row)

centroids_3 <- read.csv("Fig6F_centroids_hexane.txt", header = TRUE, sep = "")
centroids_ma_3 <- as.matrix(centroids_3[, 2:3])
distances_3 <- as.matrix(dist(centroids_ma_3, method = "euclidean"))
distancestrue3 <- dist_get(distances_3, origin_row, destination_row)

# Combine data
plot_data <- data.frame(
  Condition = factor(
    c(
      rep("11_15_DimeC27", length(distancestrue2)),
      rep("Hexane", length(distancestrue3))
    ),
    levels = c("11_15_DimeC27", "Hexane")
  ),
  Distance = c(distancestrue2, distancestrue3)
)


# Calculate medians for the black median bars
median_data <- plot_data %>%
  group_by(Condition) %>%
  summarise(median_value = median(Distance, na.rm = TRUE), .groups = "drop") %>%
  mutate(x = as.numeric(Condition))



## Conversion factor: pixels to centimeters
pixel_to_cm <- 7.6 / 1300


# Boxplot
ggplot(plot_data, aes(x = Condition, y = Distance)) +
  geom_boxplot(
    fill = "red",
    color = "black",
    width = 0.5,
    outlier.shape = NA,
    alpha = 0.65,
    linewidth = 0.7
  ) +
  # Overlay only the median line in black
  geom_segment(
    data = median_data,
    aes(
      x = x - 0.25,
      xend = x + 0.25,
      y = median_value,
      yend = median_value
    ),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  # Individual observations
  geom_jitter(
    aes(color = Condition),
    width = 0.12,
    height = 0,
    size = 2.5,
    alpha = 0.9
  ) +
  # Black diamond for the mean
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 4.5,
    color = "black"
  ) +
  scale_color_manual(
    values = c("Hexane" = "red", "11_15_DimeC27" = "red")
  ) +
  # Display pixel distances as centimeters on the y-axis
  scale_y_continuous(
    name = "Euclidean distance (cm)",
    breaks = c(0, 2.5, 5, 7.5) * 1300 / 7.6,
    labels = c("0", "2.5", "5", "7.5"),
    limits = c(0, 1300),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    axis.ticks.y = element_line(color = "black")
  )


# Statistical comparison
wilcox.test(distancestrue2, distancestrue3)