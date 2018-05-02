#veg stats
library(dplyr)
library(ggplot2)

byrep <- read.csv("byrep_cleaned.csv")
insect <- read.csv("F:/School/Chapter 2/MojaveInsectDiversity/Data/metadata_nobeetle.csv")
boxplot(byrep$understory.richness ~ byrep$Date)

boxplot(insect$percent.cover ~ insect$treatment + insect$blooming)
boxplot(insect$percent.cover ~ insect$treatment)
boxplot(insect$rich ~ insect$treatment + insect$blooming)
boxplot(byrep$understory.richness ~ byrep$treatment + byrep$flowering)

boxplot(byrep$het.annual.floral.density~ byrep$treatment)
boxplot(insect$abun ~ insect$treatment + insect$blooming)
a1 <- aov(insect$percent.cover ~ insect$treatment + insect$blooming)
summary(a1)
a12 <- aov(insect$rich ~ insect$treatment + insect$blooming)
summary(a12)
TukeyHSD(a12)
a3 <- aov(byrep$understory.richness ~ byrep$treatment + byrep$flowering)
summary(a3)
a3 <- aov(byrep$het.annual.floral.density ~ byrep$treatment + byrep$flowering)
summary(a3)
boxplot(byrep$het.annual.floral.density ~ byrep$treatment + byrep$flowering)
3 <- aov(byrep$het.annual.floral.density ~ byrep$treatment + byrep$flowering)
summary(a3)
boxplot(byrep$het.shrub.blooming.neighbours ~ byrep$treatment + byrep$flowering)
boxplot(byrep$het.cactus.blooming.neighbours ~ byrep$treatment + byrep$flowering)
?wilcox.test

w1 <- wilcox.test(abun~blooming, data = insect)
w1
