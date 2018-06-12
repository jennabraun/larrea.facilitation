library(dplyr)
library(ggplot2)

insects <- read.csv("Data/pinned.csv")
insects$rep <- paste(insects$Date, insects$Microsite)
meta <- select(insects, Microsite, Flowering, rep) %>% distinct()
beecount <- filter(insects, RTU == "bee" | RTU =="syrphid") %>% group_by(.,rep) %>% tally()
beecount <- insects %>% group_by(.,rep) %>% tally()
beecount <- filter(insects, RTU =="syrphid") %>% group_by(.,rep) %>% tally()

beecount <- right_join(beecount, meta, by = "rep")
beecount$n[is.na(beecount$n)] <- 0

ggplot(beecount, (aes(Microsite, n))) + geom_boxplot() + facet_grid(.~Flowering)
beecount$t.group <- paste(beecount$Microsite, beecount$Flowering)

a1 <- aov(beecount$n ~ beecount$Microsite + beecount$Flowering)
a2 <- aov(beecount$n ~ beecount$t.group)
a2
TukeyHSD(a2)
m1 <- lm(n ~ Microsite + Flowering, data = beecount)
m1       
summary(m1)
