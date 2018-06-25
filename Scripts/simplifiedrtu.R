#code to run analyses based on guild but need to better clean species names first 

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

simp.rtu <- read.csv("pantraps_long_pol.csv")
sum(simp.rtu$Quantity)
simp.rtu <- right_join(metadata, simp.rtu, by = "uniID")
sum(simp.rtu$Quantity)

simp.rtu <- filter(simp.rtu, sp.simp == "bee" | sp.simp == "syrphid" | sp.simp == "micro beefly" | sp.simp == "beetle" | sp.simp == "fly")
count <- simp.rtu %>% group_by(., sp.simp) %>% summarise(.,sum(Quantity))


simp.rtu <- filter(simp.rtu, sp.simp != "beetle")
ggplot(simp.rtu, aes(sp.simp, Quantity)) + geom_bar(stat="identity") + facet_grid(treatment~blooming) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#maybe run analyses without beetles?
# try filtering out first and last 5 days
simp.rtu <- filter(simp.rtu, date == "4.17.2017" | date == "4.19.2017" | date == "4.20.2017" | date == "4.22.2017" | date == "4.23.2017" | date == "4.26.2017")

ggplot(simp.rtu, aes(date, Quantity)) + geom_bar(stat = "identity") + facet_grid(~treatment) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


bees <- filter(simp.rtu, sp.simp == "bee")
a1 <- aov(bees$Quantity ~ bees$blooming)


bee.gr <- bees %>% group_by(., uniID) %>% summarise(., Quantity = sum(Quantity))
bee.gr <- right_join(metadata, bee.gr, by = "uniID")
a1 <- aov(bee.gr$Quantity ~ bee.gr$blooming * bee.gr$treatment)
summary(a1)

syrphid <- filter(simp.rtu, sp.simp == "beefly")
syrphid.gr <- syrphid %>% group_by(., uniID) %>% summarise(., Quantity = sum(Quantity))
syrphid.gr <- right_join(metadata, syrphid.gr, by = "uniID")
a1 <- aov(syrphid.gr$Quantity ~ syrphid.gr$blooming + syrphid.gr$treatment)
summary(a1)
TukeyHSD(a1)
syrphid.gr %>% group_by(., treatment, blooming) %>% summarise(., Quantity = sum(Quantity))

rt <- simp.rtu %>% group_by(., uniID) %>% summarise(., Quantity = sum(Quantity))
rt <- right_join(metadata, rt, by = "uniID")
ggplot(simp.rtu, aes(sp.simp, Quantity, fill = treatment)) + geom_bar(stat="identity", position = "dodge") + facet_grid(~blooming) + coord_flip()
