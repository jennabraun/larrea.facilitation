#making a summary graph

library(ggplot2)
library(dplyr)
source("Scripts/functions.R")
data <- read.csv("Output Data/rtu_by_rep.csv")
summary <- data %>% group_by(rtu, treatment, flowering) %>% summarise(mean.visits = mean(visits.per.hour), se.visits = se(visits.per.hour), mean.flowers = mean(flowers.per.hour), se.flowers = se(flowers.per.hour))

summary$t <- paste(summary$treatment, summary$flowering)
summary$t <- as.factor(summary$t)
summary$t <- factor(summary$t, levels = c("shrub pre", "open pre", "shrub bloom", "open bloom"))

summary$flowering <- relevel(summary$flowering, "pre")
labels <- c(pre = "Pre-blooming", post = "Blooming", bloom = "Blooming")


ggplot(summary, aes(rtu, mean.visits, group = t)) + geom_bar(aes (fill = t), stat = "identity", position = "dodge", color = "black") + geom_errorbar(aes(ymin = mean.visits - se.visits, ymax = mean.visits + se.visits), width = 0.2, position = position_dodge(0.9)) + theme_Publication()  + ylab("Mean Foraging Instance Rate") + scale_x_discrete(labels = c("Solitary Bee", "Bombyliidae", "Honeybee", "Lepidoptera", "Other", "Syrphidae")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.2)) + scale_fill_manual("", values = c("shrub pre" ="black", "open pre"= "white", "shrub bloom"= "black", "open bloom"= "white")) + theme(legend.position = "none", legend.title = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) + xlab("") + facet_grid(~flowering, labeller=labeller(flowering = labels)) 

ggplot(summary, aes(rtu, mean.flowers, group = t)) + geom_bar(aes (fill = t), stat = "identity", position = "dodge", color = "black") + geom_errorbar(aes(ymin = mean.flowers - se.flowers, ymax = mean.flowers + se.flowers), width = 0.2, position = position_dodge(0.9)) + theme_Publication() + ylab("Mean Floral Visitation Rate") + scale_x_discrete(labels = c("Solitary Bee", "Bombyliidae", "Honeybee", "Lepidoptera", "Other", "Syrphidae")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.2)) + scale_fill_manual("", values = c("shrub pre" ="black", "open pre"= "white", "shrub bloom"= "black", "open bloom"= "white")) + xlab("") + facet_grid(~flowering) + theme(strip.background = element_blank(), strip.text = element_blank()) + theme(legend.position = "none")

b


library(cowplot)
plot_grid(a, b, ncol = 1, labels = c("A", "B"), rel_heights = c(1,1.5))


grid.draw(rbind(a, b, size = "first"))
          