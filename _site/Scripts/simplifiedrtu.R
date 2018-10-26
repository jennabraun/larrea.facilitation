#code to run analyses based on guild but need to better clean species names first 

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

long <- read.csv("pantraps_long.csv")
simp <- read.csv("sp_key.csv")
env <- read.csv("Clean Data/metadata_yesbeetle.csv")

count(simp, guild)

#sum(long$Quantity)
#counts <- count(long, highest.rtu)
#write.csv(counts, "sp_key.csv")

simp.rtu <- right_join(long, simp, by = "highest.rtu")
sum(simp.rtu$Quantity)

simp.rtu <- filter(simp.rtu, guild != "NA" & guild != "NA")
sum(simp.rtu$Quantity)

bees <- filter(simp.rtu, guild == "bee")
bees.ag <- bees %>% group_by(uniID, guild) %>% summarise(Quantity = sum(Quantity)) 
sum(bees.ag$Quantity)

bee <- right_join(bees.ag, env, by = "uniID")

bee$Quantity[is.na(bee$Quantity)] <- 0
ggplot(bee, aes(treatment, Quantity)) + geom_boxplot() + facet_grid(~blooming)
a1 <- aov(bee$Quantity ~ bee$blooming + bee$treatment)
summary(a1)
TukeyHSD(a1)

shapiro.test(bee$Quantity)

ggplot(bee, aes(Quantity)) + geom_density()
bee$repID <- paste(bee$plant.id, bee$treatment)
bee$blooming <- relevel(bee$blooming, "pre")


bee1 <- glmer(Quantity ~ blooming * treatment + (1|repID), family = poisson, data = bee)
bee2<- glmer(Quantity ~ blooming + treatment + (1|repID), family = poisson, data = bee)
beenull<- glmer(Quantity ~  (1|repID), family = poisson, data = bee)

overdisp_fun(bee1)
summary(bee1)
summary(bee2)

car::Anova(bee2, type =2)



