#visitation exploration
library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
library(sjPlot)
library(jtools)
source(system.file("utils", "allFit.R", package="lme4"))
library(glmmTMB)

byobs <- read.csv("byobs_cleaned.csv")
byrep <- read.csv("byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")
pans <- read.csv("Clean Data/metadata_nobeetle.csv")
pans$uniID <- paste(pans$plant.id, pans$treatment, pans$blooming)


byrep$repID <- paste(byrep$PlantID, byrep$treatment)
byrep$treatment <- relevel(byrep$treatment, "open")
byrep$flowering <- relevel(byrep$flowering, "pre")
byrep$flowering <- gsub("bloom", "post", byrep$flowering)
byrep$uniID <- paste(byrep$PlantID, byrep$treatment, byrep$flowering)
all <- left_join(byrep, pans, by = "uniID")


m1 <- glmmTMB(total.flowers ~ treatment.x + understory.richness + flowering + flowers.pot + flowers.shrub + (1|repID), ziformula=~0,family="nbinom2", data = all)
summary(m1)

m1 <- glmmTMB(abun ~ treatment.x  * flowering + (1|repID), ziformula=~0,family="nbinom2", data = all)
summary(m1)
lsmeans(m1, pairwise~treatment.x|flowering)
