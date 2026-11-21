library(tidyr)
library(dplyr)
library(GCalignR)
library(readxl)
library(tidyverse)
library(vegan)
library(ggplot2)
library(reshape2)
library(MASS)
library(adegenet)
library(dendextend)
library(colorspace)
library(gplots)
library(RColorBrewer)
library("readxl")

setwd("/Users/matteorossi/Desktop/")

#### Data analysis for Filterpaper####
chc<-read.table("Fig5A_S6A_table_clonal_lines_and_nest.txt", sep = "\t", header = T,row.names=1)
chc2<-chc[,-c(1:4)]
id <- read.table("Fig5A_S6A_metadata_clonal_lines_and_nest.txt", sep = "\t", header = T)


#Factors
#Colony/Paper
Colony<-as.factor(id$Colony)
#Genotype
Genotype<-as.factor(id$Genotype)
#Paperanalysis
paper<-as.factor(id$Nestorcuticle)



# Histogram
# Transpose so samples are rows and compounds are columns
chc3 <- t(chc2)



# Assign group colors
# B cuticle and B nest have the same color
group_colors <- c(
  "A"    = "#009E73",  
  "B"    = "#4D4D4D",
  "M" = "#CC79A7",
  "D"    = "#7B3294",
  "Nest" = "gray"
)


# Heatmap color scale
nb.cols <- 100
mycolors <- colorRampPalette(
  RColorBrewer::brewer.pal(9, "Oranges")
)(nb.cols)

# Plot heatmap without a dendrogram
gplots::heatmap.2(
  as.matrix(chc3),
  Rowv = FALSE,                  
  Colv = FALSE,                 
  dendrogram = "none",           
  trace = "none",
  srtCol = 90,
  margins = c(10, 6),
  key.xlab = "Relative Percentage on Cuticle",
  denscol = "grey",
  density.info = "density",
  col = mycolors,
  labRow = rownames(chc3)
)
