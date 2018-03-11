a1 <- aov(visits.per.hour ~ treatment + flowering, data = all.data)
summary(a1)
TukeyHSD(a1)

m1 <- lm(visits.per.hour ~ treatment + flowering, data = all.data)
summary(m1)
aov(m1)


a2 <- aov(visits.per.hour ~ t.group, data = all.data)
summary(a2)
TukeyHSD(a2)

#all in
m2 <- lm(visits.per.hour ~ treatment + flowering + flowers.shrub + flowers.pot + het.annual.floral.density, data = all.data)
summary(m2)

#EDA



ggplot(data = counts, aes(microsite, mean.prop.fl)) + geom_boxplot() + xlab("Microsite") + ylab("Mean proportion of flowers visited") + theme_Publication() + facet_grid(~flowering)

ggplot(data = counts, aes(microsite, flowers.visited)) + geom_boxplot() + xlab("Microsite") + ylab("Flowers visited per hour") + theme_Publication() + facet_grid(~flowering)

ggplot(data = all.data, aes(treatment, visits.per.hour)) + geom_boxplot() + xlab("Microsite") + ylab("Flower visits per hour")+ facet_grid(~flowering)