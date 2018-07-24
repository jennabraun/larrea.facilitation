#the script is for EDA, and correlation tests to support modelling for pantrap data
library(dplyr)
library(tidyr)

meta <- read.csv("Clean Data/metadata_nobeetle.csv")

#abundance between microsites
abund <- dplyr::select(meta, plant.id, treatment, blooming, abun)
abund$ID <- paste(abund$plant.id, abund$blooming)
abund <- dplyr::select(abund, -plant.id, -blooming)

abund <- spread(abund, treatment, abun)

cor.test(abund$open, abund$shrub)


div <- dplyr::select(meta, plant.id, treatment, blooming, H)
div$ID <- paste(div$plant.id, div$blooming)
div <- dplyr::select(div, -plant.id, -blooming)
div <- spread(div, treatment, H)
cor.test(div$open, div$shrub)

spe <- dplyr::select(meta, plant.id, treatment, blooming, Species)
spe$ID <- paste(spe$plant.id, spe$blooming)
spe <- dplyr::select(spe, -plant.id, -blooming)
spe <- spread(spe, treatment, Species)
cor.test(spe$open, spe$shrub)


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

#try to do a correlation between pans and visitation
pans <- read.csv("F:/School/Chapter 2/MojaveInsectDiversity/data/metadata_nobeetle.csv")
pans$joinID <- paste(pans$plant.id, pans$blooming, pans$treatment)
byrep$joinID <- paste(byrep$PlantID, byrep$flowering, byrep$treatment)
pansjoin <- left_join(pans, byrep, by = "joinID")

cor.test(pansjoin$abun, pansjoin$flowers.per.hour)
cor.test(pansjoin$abun, pansjoin$visits.per.hour)
cor.test(pansjoin$H, pansjoin$flowers.per.hour)
cor.test(pansjoin$H, pansjoin$visits.per.hour)

#visits per hour
reps <- byrep
reps$flowering <- gsub("blooming", "post", reps$flowering)
reps$flowering <- gsub("bloom", "post", reps$flowering)
reps$repID <- paste(reps$PlantID, reps$flowering)


reps <- dplyr::select(reps, PlantID, treatment, flowering, flowers.per.hour)
reps$ID <- paste(reps$PlantID, reps$flowering)
reps <- dplyr::select(reps, -PlantID, -flowering)
reps <- filter(reps, ID != "198 post")
reps <- spread(reps, treatment, flowers.per.hour)



cor.test(reps$open, reps$shrub)
cor.test(visitsjoin$per.hour, visitsjoin$flowers.per.hour)
