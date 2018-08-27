#rtu modelling: look at species-specificity

library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
library(sjPlot)
library(jtools)
source(system.file("utils", "allFit.R", package="lme4"))
library(glmmTMB)
library(tidyr)

byrtu <- read.csv("rtu_by_rep.csv")

byrtu$repID <- paste(byrtu$PlantID, byrtu$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")


#Full model
mFullPQ <- MASS::glmmPQL(total.visits~flowering * rtu * treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
car::Anova(mFullPQ, type = 3)


mFullPQv <- MASS::glmmPQL(total.flowers~flowering * rtu * treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
car::Anova(mFullPQv, type = 3)



mQ <- MASS::glmmPQL(total.flowers~flowering * rtu + treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
summary(mQ)
lsmeans(mQ, pairwise~flowering|rtu)
car::Anova(mQ, type = 3)


mQt <- MASS::glmmPQL(total.flowers~treatment * rtu + flowering + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
summary(mQt)

#RTU by microsite not significant

#lsmeans(mQt, pairwise~treatment|rtu)
#lsmeans on insignificant interaction shows syrphid flies as significant though

#total plant visits
mPQ <- MASS::glmmPQL(total.visits~flowering * rtu + treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
summary(mPQ)
car::Anova(mPQ, type = 3)
lsmeans(mPQ, pairwise~flowering|rtu)


mPt <- MASS::glmmPQL(total.visits~treatment * rtu + flowering + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
summary(mPt)
car::Anova(mPt, type = 3)
#lsmeans(mPt, pairwise~treatment|rtu)
#treatment not significant, pairwaise constrast for syrphids is



