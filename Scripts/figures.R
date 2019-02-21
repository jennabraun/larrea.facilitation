#figures
library(ggplot2)
library(jtools)
library(dplyr)
library(cowplot)
source("Scripts/functions.R")
visits <- read.csv("Output Data/byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")
metadata <- read.csv("Output Data/metadata_nobeetle.csv")
nobeetle <- read.csv("Output Data/metadata_nobeetle.csv")
incbeetle <- read.csv("Output Data/metadata_yesbeetle.csv")
onlybeetle <- read.csv("Output Data/metadata_onlybeetle.csv")

labels <- c(pre = "Pre-blooming", post = "Blooming", bloom = "Blooming")
visits$flowering <- relevel(visits$flowering, "pre")
metadata$blooming <- relevel(metadata$blooming, "pre")
incbeetle$blooming <- relevel(incbeetle$blooming, "pre")
onlybeetle$blooming <- relevel(onlybeetle$blooming, "pre")
byrtu$flowering <- relevel(byrtu$flowering, "pre")

ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + theme_Publication() + ylab("Percent Annual Vegetation Cover") + xlab("Microsite")

p1 <- ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + theme_Publication() + ylab("Percent Annual Vegetation Cover") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(metadata, aes(treatment, rich)) + geom_boxplot() + theme_Publication() + ylab("Annual Species Richness") + xlab("Microsite")                                     

p2 <- ggplot(visits, aes(treatment, understory.richness)) + geom_boxplot() + theme_Publication() + ylab("Annual Species Richness") + xlab("Microsite") + facet_grid(~flowering,labeller=labeller(flowering = labels))                                             


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

p3 <- ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Arthopod Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

#Just Melyridae
ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Melyridae Abundance") + xlab("Microsite")

ggplot(onlybeetle, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("Melyridae Abundance") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


#Diversity figures


ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Insect Species Richness") + xlab("Microsite")

p4 <- ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("Arthropod Species Richness") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite")

ggplot(incbeetle, aes(treatment, H)) + geom_violin() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))

ggplot(incbeetle, aes(treatment, H)) + geom_boxplot() + theme_Publication() + ylab("Shannon Diversity Index") + xlab("Microsite") + facet_grid(~blooming,labeller=labeller(blooming = labels))


#visitation


ggplot(visits, aes(treatment, flowers.per.hour)) + geom_boxplot() + facet_grid(~flowering)  + coord_flip()


ggplot(byrtu, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~rtu, scales= "free")  

byrtu.fil <- filter(byrtu, rtu == "syrphid" | rtu == "lep" | rtu == "other")

ggplot(byrtu.fil, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~rtu, scales= "free")  + coord_flip()

library(caret)

str(byrtu)

p <- byrtu %>% group_by(treatment, flowering, rtu) %>% dplyr::summarise(visits = sum(flowers.per.hour))



ggplot(p, aes(treatment, visits)) + geom_bar(aes(fill = rtu), stat = "identity", colour = "black") + facet_grid(~flowering, labeller=labeller(flowering = labels))+ theme_Publication() + xlab("Microsite") + ylab("Total Flowers Visited") + labs(fill="") + theme(legend.text = element_text(size = 16))  + scale_fill_brewer(palette= "Spectral") + scale_fill_manual("", values = c("bee" ="#D53E4F", "bombylid"= "#FC8D59", "honeybee"= "#FEE08B", "lep"= "#D3D3D3", "other"= "#99D594", "syrphid" = "#3288BD"), labels = c("Solitary Bee", "Bombyliidae", "Honeybee", "Lepidoptera", "Other", "Syrphidae"))


p1 <- ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + theme_Publication() + ggtitle("Percent Annual Cover") + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels))  +theme(plot.title = element_text(size = (10)))+ facet_grid(~blooming,labeller=labeller(blooming = labels))

p2 <- ggplot(visits, aes(treatment, understory.richness)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + ggtitle("Annual Richness")+theme(plot.title = element_text(size = (10))) + facet_grid(~flowering,labeller=labeller(flowering = labels))

p2

p3 <- ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels)) + ggtitle("Arthropod Abundance")+theme(plot.title = element_text(size = (10)))


p4 <- ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels)) + ggtitle("Arthropod Richness")+theme(plot.title = element_text(size = (10)))

p1
plot_grid(p1, p2, p3, p4, labels = c("A", "B", "C", "D"))

p5 <- ggplot(visits, aes(treatment, understory.richness)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + ggtitle("Annual Richness")+theme(plot.title = element_text(size = (10)))  

p6 <- ggplot(visits, aes(flowering, understory.richness)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + ggtitle("Annual Richness")+theme(plot.title = element_text(size = (10)))                        
              
p7 <- ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + ggtitle("Arthropod Abundance")+theme(plot.title = element_text(size = (10)))

p8 <- ggplot(metadata, aes(blooming, abun)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("") + ggtitle("Arthropod Abundance")+theme(plot.title = element_text(size = (10)))

p9 <- ggplot(incbeetle, aes(treatment, Species)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("")  + ggtitle("Arthropod Richness")+theme(plot.title = element_text(size = (10)))

p10 <- ggplot(incbeetle, aes(blooming, Species)) + geom_boxplot() + theme_Publication() + ylab("") + xlab("")  + ggtitle("Arthropod Richness")+theme(plot.title = element_text(size = (10)))

plot_grid(p5, p6, p7, p8, p9, p10, ncol = 2)



                                                           

