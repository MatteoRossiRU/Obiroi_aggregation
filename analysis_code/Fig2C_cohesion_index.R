setwd("/Users/matteorossi/Desktop/")

library(gplots) 
library(ggplot2)

#read table
cohesion<-read.csv("Fig2C_cohesion_index.txt",header=TRUE,sep="")

#
p <- ggplot(cohesion) +
  geom_boxplot(aes(x = group, y = cohesion_index), width = 0.4, color=c("cornflowerblue","orange","red")) +  
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_jitter(aes(x = group, y = cohesion_index, color = group), width = 0.04, size = 2) +  
  scale_color_manual(values = c("cornflowerblue", "orange", "red")) +  
  ylim(0, 1)
p


#KRUSKAL-WALLIS TEST
kruskal_result <- kruskal.test(cohesion_index~group, data = cohesion)
kruskal_result


#PAIRWISE WILCOXON TEST
pairwise_result <- pairwise.wilcox.test(
  cohesion$cohesion_index,
  cohesion$group,
  p.adjust.method = "holm",
  paired = FALSE
)

pairwise_result
