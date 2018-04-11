library(dplyr)
library(ggplot2)
library(MASS)

byobs <- read.csv("byobs_cleaned.csv")
byrep <- read.csv("byrep_cleaned.csv")
str(byrep)

shapiro.test(byrep$visits.per.hour)
ggplot(data = byrep, aes(visits.per.hour)) + geom_freqpoly()
#not normal, so many zeros

shapiro.test(byrep$mean.visit.length)
ggplot(data = byrep, aes(mean.visit.length)) + geom_freqpoly()

ggplot(data = byrep, aes(Date, visits.per.hour)) + geom_boxplot() + facet_grid(.~treatment)

visit.data


a1 <- aov(visits.per.hour ~ treatment + flowering, data = byrep)
summary(a1)
TukeyHSD(a1)

m1 <- lm(visits.per.hour ~ treatment + flowering + Date, data = byrep) 
summary(m1)
aov(m1)



#all in
m2 <- lm(visits.per.hour ~ treatment + flowering + flowers.shrub + flowers.pot + het.annual.floral.density, data = byrep)
summary(m2)

#EDA

ggplot(data = byrep, aes(microsite, mean.prop.fl)) + geom_boxplot() + xlab("Microsite") + ylab("Mean proportion of flowers visited") + theme_Publication() + facet_grid(~flowering)



ggplot(data = counts, aes(microsite, mean.prop.fl)) + geom_boxplot() + xlab("Microsite") + ylab("Mean proportion of flowers visited") + theme_Publication() + facet_grid(~flowering)

ggplot(data = counts, aes(microsite, flowers.visited)) + geom_boxplot() + xlab("Microsite") + ylab("Flowers visited per hour") + theme_Publication() + facet_grid(~flowering)

ggplot(data = all.data, aes(treatment, visits.per.hour)) + geom_boxplot() + xlab("Microsite") + ylab("Flower visits per hour")+ facet_grid(~flowering)

#EDA
dotchart(byrep$visits.per.hour)
pairs(byrep)

#try out glm
g1 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment + flowering + Date)
summary(g1)
plot(g1)

E <- resid(g1)
hist(E)
plot(resid(g1) ~ byrep$treatment)
plot(resid(g1) ~ byrep$flowering)
plot(resid(g1) ~ byrep$Date)

g2 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment * flowering)
summary(g2)
g2

g3 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment + flowering + understory.richness)
summary(g3)
          
g4 <- glmmPQL(visits.per.hour ~ treatment + flowering + understory.richness + mean.Temp,  random = ~1|PlantID, family = quasipoisson(link = "log"), data = byrep)
summary(g4)




