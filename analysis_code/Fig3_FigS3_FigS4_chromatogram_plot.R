setwd("/Users/matteorossi/Desktop/")

library(tidyverse)


########

### ---- LOAD FIRST CHROMATOGRAM ---- #nest area
df1 <- read_csv("Fig3_FigS3_nest_chromatogram.csv", col_names = FALSE)

df1_clean <- df1 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)


### ---- LOAD SECOND CHROMATOGRAM ---- #non-nest area/control
df2 <- read_csv("Fig3_FigS3_control_chromatogram.csv", col_names = FALSE)

df2_clean <- df2 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)



###FULL CHROMATOGRAM NEST - Fig S2
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) +  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 5),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Ion Count",
    title="GC–MS Total Ion Chromatograms"
  )


#Zoom in #Fig 3A
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) +  
  coord_cartesian(xlim = c(35, 45)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(35, 45, 1),
    expand = c(0.01, 0)  
  ) +
  labs(
    x="Retention Time (min)",
    y="Ion Count",
    title="GC–MS Total Ion Chromatograms"
  )



###FULL CHROMATOGRAM Cuticle #FigS2

### ---- LOAD SECOND CHROMATOGRAM ----
df3 <- read_csv("Fig3_FigS3_cuticle_chromatogram.csv", col_names = FALSE)

df3_clean <- df3 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)



ggplot() +
  geom_line(data=df3_clean, aes(x=time, y=intensity), color="purple", linewidth=0.7) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 5),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Ion Count",
    title="GC–MS Total Ion Chromatograms"
  )





#Zoom in cuticle peaks #Fig 3A and 6A

#df3 <- read_csv("Fig3_FigS3_cuticle_chromatogram.csv", col_names = FALSE)

df3_clean <- df3 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)



ggplot(df3_clean, aes(x = time, y = intensity)) +
  geom_line(color = "purple", linewidth = 0.7) +
  coord_cartesian(xlim = c(35, 45)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(35, 45, 1),
    expand = c(0.01, 0)
  ) +
  labs(
    x = "Retention Time (min)",
    y = "Ion Count",
    title = "GC–MS TIC (35–45 min)"
  )
#






###FigS4


### ---- LOAD FIRST CHROMATOGRAM ---- #nest area (colony only young ants)
df1 <- read_csv("FigS4_nest_chromatogram_youngF.csv", col_names = FALSE)

df1_clean <- df1 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)



### ---- LOAD SECOND CHROMATOGRAM ---- #control area (colony only young ants)
df2 <- read_csv("FigS4_control_chromatogram_youngF.csv", col_names = FALSE)

df2_clean <- df2 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)


###FULL CHROMATOGRAM
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) + 
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 5),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Ion count",
    title="GC–MS Total Ion Chromatograms"
  )

#ZOOM IN
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) + 
  coord_cartesian(xlim = c(37, 47)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(37, 47, 1),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Intensity",
    title="GC–MS Total Ion Chromatograms"
  )






### ---- LOAD FIRST CHROMATOGRAM ----#nest area (colony only old ants)
df1 <- read_csv("FigS4_nest_chromatogram_oldR.csv", col_names = FALSE)

df1_clean <- df1 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)


### ---- LOAD SECOND CHROMATOGRAM ---- #control area (colony only old ants)
df2 <- read_csv("FigS4_control_chromatogram_oldR.csv", col_names = FALSE)

df2_clean <- df2 %>%
  filter(str_detect(X1, ",")) %>%
  separate(X1, into=c("time", "intensity"), sep=",", convert=TRUE)


###FULL CHROMATOGRAM
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) +  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 5),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Intensity",
    title="GC–MS Total Ion Chromatograms"
  )

###ZOOM IN
ggplot() +
  geom_line(data=df1_clean, aes(x=time, y=intensity), color="#2ECC40", linewidth=0.7) +
  geom_line(data=df2_clean, aes(x=time, y=intensity), color="gray60", linewidth=0.6) + 
  coord_cartesian(xlim = c(35, 45)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(
    breaks = seq(35, 45, 1),
    expand = c(0.01, 0)   # <-- small margin added
  ) +
  labs(
    x="Retention Time (min)",
    y="Intensity",
    title="GC–MS Total Ion Chromatograms"
  )






