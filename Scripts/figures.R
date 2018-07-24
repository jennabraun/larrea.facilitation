#figures
library(ggplot2)
library(jtools)
library(dplyr)
visits <- read.csv("byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")
metadata <- read.csv("Clean Data/metadata_nobeetle.csv")
nobeetle <- read.csv("Clean Data/metadata_nobeetle.csv")
incbeetle <- read.csv("Clean Data/metadata_yesbeetle.csv")
onlybeetle <- read.csv("Clean Data/metadata_onlybeetle.csv")

labels <- c(pre = "Pre-blooming", post = "Blooming", bloom = "Blooming")
visits$flowering <- relevel(visits$flowering, "pre")
metadata$blooming <- relevel(metadata$blooming, "pre")
incbeetle$blooming <- relevel(incbeetle$blooming, "pre")
onlybeetle$blooming <- relevel(onlybeetle$blooming, "pre")

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

ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Insect Abundance") + xlab("Microsite")

ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Melyridae Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


#Diversity figures


ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Insect Species Richness") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Arthropod Species Richness") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, H)) + geom_boxplot() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


#visitation


ggplot(visits, aes(treatment, flowers.per.hour)) + geom_boxplot() + facet_grid(~flowering)  + coord_flip()


ggplot(byrtu, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~rtu, scales= "free")  

byrtu.fil <- filter(byrtu, rtu == "syrphid" | rtu == "lep" | rtu == "other")

ggplot(byrtu.fil, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~rtu, scales= "free")  + coord_flip()

library(caret)

ggplot(byrtu, aes(treatment, flowers.per.hour, fill = rtu)) + geom_bar(stat = "identity") + facet_grid(~flowering, labeller=labeller(flowering = labels)) + scale_fill_brewer(palette= "Spectral") + theme_Publication() + xlab("Microsite") + ylab("Flowers visited (weighted by video length") + labs(fill="") + theme(legend.text = element_text(size = 16))




+ theme_Publication()




test_down <- downSample(byrtu, byrtu$treatment)
byrtu %>% group_by(treatment, flowering, rtu) %>% summarise(n())





library(ggplot2)
ggplot(test_down, aes(treatment, flowers.per.hour)) +
  geom_bar(aes(fill = rtu), 
           position = position_fill(reverse = F))
#here



ggplot(visits, aes(treatment, total.flowers)) + geom_bar(stat = "identity", weight = dec.Length) + facet_grid(~flowering,) + coord_flip()





bees <-filter(byrtu, rtu == "bee" | rtu == "honeybee")  
bees <- select(bees, uniID, everything())
bees <- select(bees, -total.visits, -visits.per.hour, -flowers.per.hour, -X)
bees <- spread(bees,rtu, total.flowers)
bees <- mutate(bees, total.flowers = bee + honeybee) %>% select(-bee, -honeybee)                                                                




boxplot(visits$flowers.per.hour~visits$treatment+visits$flowering, notch = TRUE)
summary(fm1.restart)
car::Anova(fm1.restart, type = 2)
