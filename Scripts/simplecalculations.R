#size calculations

library(dplyr)
sizes <- read.csv("Clean Data/video_larreadimensions.csv")
str(sizes)
mean(sizes$width, na.rm = TRUE)
mean(sizes$height, na.rm = TRUE)
