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
library(MASS)

byrtu <- read.csv("rtu_by_rep.csv")

byrtu$repID <- paste(byrtu$PlantID, byrtu$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")



bees <-filter(byrtu, rtu == "bee" | rtu == "honeybee")  
bees <- select(bees, uniID, everything())
bees <- select(bees, -total.visits, -visits.per.hour, -flowers.per.hour, -X)
bees <- spread(bees,rtu, total.flowers)
bees <- mutate(bees, total.flowers = bee + honeybee) %>% select(-bee, -honeybee)                                                                



bees %>% group_by(., treatment, flowering) %>% summarise(n = mean(total.flowers))                                                                

sum(bees$total.flowers)  
sum(bees$total.visits)









#syrphids

syrphids <-filter(byrtu, rtu == "syrphid")  
m1 <- glmer(total.flowers ~ treatment * flowering  + offset(log(dec.Length)) + (1|repID), family="poisson", data = syrphids) 

summary(m1)
overdisp_fun(m1)

m2 <- glmer(total.flowers ~ treatment + flowering  + offset(log(dec.Length)) + (1|repID), family="poisson", data = syrphids) 
summary(m2)
AIC(m1, m2)

m3 <- glmer.nb(total.flowers ~ treatment * flowering  + offset(log(dec.Length)) + (1|repID),data = syrphids) 


m4 <- glmer.nb(total.flowers ~ treatment + flowering  + offset(log(dec.Length)) + (1|repID),data = syrphids) 





AIC(m1, m2, m3, m4)

summary(m4)

summary(m2)

AIC(m1, m2)

overdisp_fun(m2)
mean(syrphids$total.flowers)
var(syrphids$total.flowers)
sd(syrphids$total.flowers)

m6 <- glmer(total.flowers ~ rtu*flowering  + offset(log(dec.Length)) + (1|repID), family = "poisson", data = byrtu) 

summary(m6)
lsmeans(m6, pairwise~flowering|rtu)

mQ <- glmmPQL(total.flowers~flowering * rtu + treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)

summary(mQ)
lsmeans(mQ, pairwise~flowering|rtu)

car::Anova(mQ, type = 3)


#total plant visits
mPQ <- glmmPQL(total.visits~flowering * rtu + treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)
summary(mPQ)
car::Anova(mPQ, type = 3)
lsmeans(mPQ, pairwise~flowering|rtu)


#Full model
mFullPQ <- glmmPQL(total.visits~flowering * rtu * treatment + flowers.pot + offset(log(dec.Length)), random = ~1|repID, family="quasipoisson", data = byrtu)

car::Anova(mFullPQ, type = 3)




