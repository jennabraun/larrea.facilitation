library(dplyr)
library(ggplot2)

rtu <- read.csv("rtu_by_rep.csv")
#rtu.ag <- rtu %>% group_by(treatment, flowering, rtu) %>% summarise(plant.visits = sum(total.visits), flower.visits = sum(total.flowers)) 
rtu$ID <- paste(rtu$treatment, rtu$flowering)
totals <- rtu %>% group_by(treatment, flowering) %>% summarise(total.plant.visits = sum(total.visits), total.flower.visits = sum(total.flowers)) 
totals$ID <- paste(totals$treatment, totals$flowering)
totals <-  select(totals, -flowering, -treatment)
rtu.ag <- right_join(rtu.ag, totals, by = "ID")
rtu.ag <- mutate(rtu.ag, prop.plant = plant.visits/total.plant.visits, prop.flowers = flower.visits/total.flower.visits)
rtu.ag$flowering <- relevel(rtu.ag$flowering, "pre")
ggplot(rtu.ag, aes(treatment.x, prop.plant, fill = rtu)) + geom_bar(stat = "identity") + facet_grid(~flowering)

labels <- c(pre = "Pre-blooming", post = "Blooming", bloom = "Blooming")

ggplot(rtu.ag, aes(treatment.x, prop.flowers, fill = rtu)) + geom_bar(stat = "identity", color = "black") + facet_grid(~flowering, labeller=labeller(flowering = labels)) + scale_fill_brewer(palette= "Spectral") + theme_Publication() + xlab("Microsite") + ylab("Proportion of total visits") + labs(fill="") + theme(legend.text = element_text(size = 16))

                                                                                                      