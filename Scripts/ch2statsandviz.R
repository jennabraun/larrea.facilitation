library(dplyr)
library(ggplot2)

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

m1 <- lm(visits.per.hour ~ treatment + flowering, data = byrep) 
summary(m1)
aov(m1)


a2 <- aov(visits.per.hour ~ t.group, data = byrep)
summary(a2)
TukeyHSD(a2)

#all in
m2 <- lm(visits.per.hour ~ treatment + flowering + flowers.shrub + flowers.pot + het.annual.floral.density, data = byrep)
summary(m2)

#EDA

ggplot(data = byrep, aes(microsite, mean.prop.fl)) + geom_boxplot() + xlab("Microsite") + ylab("Mean proportion of flowers visited") + theme_Publication() + facet_grid(~flowering)



ggplot(data = counts, aes(microsite, mean.prop.fl)) + geom_boxplot() + xlab("Microsite") + ylab("Mean proportion of flowers visited") + theme_Publication() + facet_grid(~flowering)

ggplot(data = counts, aes(microsite, flowers.visited)) + geom_boxplot() + xlab("Microsite") + ylab("Flowers visited per hour") + theme_Publication() + facet_grid(~flowering)

ggplot(data = all.data, aes(treatment, visits.per.hour)) + geom_boxplot() + xlab("Microsite") + ylab("Flower visits per hour")+ facet_grid(~flowering)


#try out glm
g1 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment + flowering)
g1
summary(g1)

g2 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment * flowering)
summary(g2)
g2

g3 <- glm(data = byrep, family = quasipoisson (link = "log"), visits.per.hour ~ treatment + flowering + understory.richness + het.annual.floral.density + het.shrub.blooming.neighbours + understory.assoc)
summary(g3)
g3          
