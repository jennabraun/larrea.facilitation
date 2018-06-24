#veg EDA
library(dplyr)
library(ggplot2)

byrep <- read.csv("byrep_cleaned.csv")
insect <- read.csv("Clean Data/metadata_nobeetle.csv")
boxplot(byrep$understory.richness ~ byrep$Date)

boxplot(insect$percent.cover ~ insect$treatment + insect$blooming)
boxplot(insect$percent.cover ~ insect$treatment)
boxplot(insect$rich ~ insect$treatment + insect$blooming)
boxplot(byrep$understory.richness ~ byrep$treatment + byrep$flowering)

boxplot(byrep$het.annual.floral.density~ byrep$treatment)
boxplot(insect$abun ~ insect$treatment + insect$blooming)

boxplot(byrep$het.shrub.blooming.neighbours ~ byrep$treatment + byrep$flowering)
boxplot(byrep$het.cactus.blooming.neighbours ~ byrep$treatment + byrep$flowering)


w1 <- wilcox.test(abun~blooming, data = insect)
w1
