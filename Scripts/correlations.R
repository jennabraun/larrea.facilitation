#the script is for EDA, and correlation tests to support modelling for pantrap data
library(dplyr)
library(tidyr)

meta <- read.csv("Data/outputmetadata.csv")

abund <- dplyr::select(meta, plant.id, treatment, blooming, abun)
abund$ID <- paste(abund$plant.id, abund$blooming)
abund <- dplyr::select(abund, -plant.id, -blooming)

abund <- spread(abund, treatment, abun)

cor.test(abund$open, abund$shrub)

div <- dplyr::select(meta, plant.id, treatment, blooming, H)
div$ID <- paste(div$plant.id, div$blooming)
div <- dplyr::select(div, -plant.id, -blooming)


div <- dplyr::select(meta, plant.id, treatment, blooming, H)
div$ID <- paste(div$plant.id, div$treatment)
div <- dplyr::select(div, -plant.id, -treatment)
div <- spread(div, blooming, H)
cor.test(div$pre, div$post)


abund <- dplyr::select(meta, plant.id, treatment, blooming, abun)
abund$ID <- paste(abund$plant.id, abund$treatment)
abund <- dplyr::select(abund, -plant.id, -treatment)
abund <- spread(abund, blooming, abun)
cor.test(abund$pre, abund$post)
abund <- na.omit(abund)
cov(abund$pre, abund$post)
