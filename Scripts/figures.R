#figures
library(ggplot2)
library(jtools)
visits <- read.csv("byrep_cleaned.csv")
metadata <- read.csv("Clean Data/metadata_nobeetle.csv")
nobeetle <- read.csv("Clean Data/metadata_nobeetle.csv")
incbeetle <- read.csv("Clean Data/metadata_yesbeetle.csv")
onlybeetle <- read.csv("Clean Data/metadata_onlybeetle.csv")

labels <- c(pre = "Pre-blooming", post = "Blooming", bloom = "Blooming")

ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + theme_Publication() + ylab("Percent Annual Vegetation Cover") + xlab("Microsite")
ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + theme_Publication() + ylab("Percent Annual Vegetation Cover") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(metadata, aes(treatment, rich)) + geom_boxplot() + theme_Publication() + ylab("Annual Species Richness") + xlab("Microsite")                                     

ggplot(visits, aes(treatment, understory.richness)) + geom_boxplot() + theme_Publication() + ylab("Annual Species Richness") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))                                             


ggplot(visits, aes(treatment, het.annual.floral.density)) + geom_boxplot() + theme_Publication() + ylab("Annual Floral Density") + xlab("Microsite")   
ggplot(visits, aes(treatment, het.annual.floral.density)) + geom_boxplot() + theme_Publication() + ylab("Annual Floral Density") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))  

ggplot(visits, aes(treatment, het.shrub.blooming.neighbours)) + geom_boxplot() + theme_Publication() + ylab("Blooming Shrubs within 2 m") + xlab("Microsite")   
ggplot(visits, aes(treatment, het.shrub.blooming.neighbours)) + geom_boxplot() + theme_Publication() + ylab("Blooming Shrubs within 2 m") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))  


ggplot(visits, aes(treatment, het.cactus.blooming.neighbours)) + geom_boxplot() + theme_Publication() + ylab("Blooming Cactus within 2 m") + xlab("Microsite")   
ggplot(visits, aes(treatment, het.cactus.blooming.neighbours)) + geom_boxplot() + theme_Publication() + ylab("Blooming Cactus within 2 m") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))  

ggplot(visits, aes(treatment, flowers.per.hour)) + geom_boxplot() + theme_Publication() + ylab("Flowers visited per hour") + xlab("Microsite")   
ggplot(visits, aes(treatment, flowers.per.hour)) + geom_boxplot() + theme_Publication() + ylab("Flowers visited per hour") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))  

ggplot(visits, aes(treatment, visits.per.hour)) + geom_boxplot() + theme_Publication() + ylab("Plant visits per hour") + xlab("Microsite")   
ggplot(visits, aes(treatment, visits.per.hour)) + geom_boxplot() + theme_Publication() + ylab("Plant visits per hour") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels)) 



#rtu related figures
byrtu <- read.csv("rtu_by_rep.csv")
byobs <- read.csv("byobs_cleaned.csv")
rtu.key <- read.csv("Clean Data/video_rtu_key.csv")
rtu.data <- left_join(rtu.key, byobs, by = "highest.rtu")
#byrtu$treatment <- relevel(byrtu$treatment, "open")
byobs$flowering <- relevel(byobs$flowering, "pre")

ggplot(byobs, aes(microsite, dec.total.time)) + geom_boxplot() + facet_grid(~flowering) + theme_Publication()  + ylab("Time spent per visit") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels)) 

ggplot(byobs, aes(microsite, prop.un.fl.visited)) + geom_boxplot() + facet_grid(~flowering) + theme_Publication()  + ylab("Proportion of flowers visited per visit") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels)) 

ggplot(rtu.data, aes(rtu.ag, dec.total.time)) + geom_boxplot() + facet_grid(flowering~microsite, labeller=labeller(flowering = labels)) + theme_Publication() + xlab("RTU") + ylab("Time spent per visit")




ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite")

ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite")

ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Insect Species Richness") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Insect Species Richness") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))



#visitation
ggplot(byrep, aes(treatment, flowers.per.hour)) + geom_bar(stat ="identity") + facet_grid(~flowering)
ggplot(byrep, aes(treatment, flowers.per.hour)) + geom_boxplot() + facet_grid(~flowering, scales = "free") 

boxplot(byrep$flowers.per.hour~byrep$treatment+byrep$flowering, notch = TRUE)
summary(fm1.restart)
car::Anova(fm1.restart, type = 2)
