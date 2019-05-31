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
metadata$treatment <- relevel(metadata$treatment, "shrub")
visits$treatment <- relevel(visits$treatment, "shrub")
incbeetle$treatment <- relevel(incbeetle$treatment, "shrub")
incbeetle$blooming <- relevel(incbeetle$blooming, "pre")
onlybeetle$blooming <- relevel(onlybeetle$blooming, "pre")
byrtu$flowering <- relevel(byrtu$flowering, "pre")

a <- ggplot(metadata, aes(treatment, percent.cover)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + geom_boxplot(aes(fill = treatment)) + theme_Publication() + ggtitle("Percent Annual Cover") + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels))  +theme(plot.title = element_text(size = (10)))+ facet_grid(~blooming,labeller=labeller(blooming = labels)) + scale_fill_manual("", values = c("shrub" ="black", "open"= "white")) + theme(legend.position = "none") + stat_summary(fun.y=mean, colour="red", geom="point", shape=18, size=3,show_guide = FALSE) + theme(plot.margin=unit(c(1,0.1,0,1), "cm"), ,axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

ggplot(visits, aes(treatment, understory.richness)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + geom_boxplot(aes(fill = treatment)) + theme_Publication() + ggtitle("Annual Richness") + ylab("") + xlab("")  +theme(plot.title = element_text(size = (10)))+ facet_grid(~flowering,labeller=labeller(flowering = labels)) + scale_fill_manual("", values = c("shrub" ="black", "open"= "white")) + theme(legend.position = "none") + stat_summary(fun.y=mean, colour="red", geom="point", shape=18, size=3,show_guide = FALSE) + theme(plot.margin=unit(c(1,1,0,0.1), "cm"),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

c <- ggplot(metadata, aes(treatment, abun)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + geom_boxplot(aes(fill = treatment)) + theme_Publication() + ggtitle("Arthropod Abundance") + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels))  +theme(plot.title = element_text(size = (10)))+ facet_grid(~blooming,labeller=labeller(blooming = labels)) + scale_fill_manual("", values = c("shrub" ="black", "open"= "white")) + theme(legend.position = "none") + stat_summary(fun.y=mean, colour="red", geom="point", shape=18, size=3,show_guide = FALSE) + theme(strip.background = element_blank(), strip.text = element_blank()) + theme(plot.margin=unit(c(0.1,0.1,1,1), "cm"),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())


ggplot(incbeetle, aes(treatment, Species)) + 
  stat_boxplot(geom ='errorbar', width = 0.2) + geom_boxplot(aes(fill = treatment)) + theme_Publication() + ggtitle("Arthropod Richness") + ylab("") + xlab("") + facet_grid(~blooming,labeller=labeller(blooming = labels))  +theme(plot.title = element_text(size = (10)))+ facet_grid(~blooming,labeller=labeller(blooming = labels)) + scale_fill_manual("", values = c("shrub" ="black", "open"= "white")) + theme(legend.position = "none") + stat_summary(fun.y=mean, colour="red", geom="point", shape=18, size=3,show_guide = FALSE) + theme(strip.background = element_blank(), strip.text = element_blank()) + theme(plot.margin=unit(c(0.1,1,1,0), "cm"),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())


plot_grid(a,b,c,d)


##boot data
all.boot <-read.csv("Output Data/allboot.csv")
all.new <- read.csv("Output Data/allnewboot.csv")

all.new$metric <- factor(all.new$metric, levels = c("visits", "percent.cover", "annual.richness", "arth.abundance", "arth.richness"))

all.boot$metric <- factor(all.boot$metric, levels = c("visits", "percent.cover", "annual.richness", "abundance", "arth.rich"))

metrics <- c("Floral Visits \n per Hour", "Annual Cover", "Annual Species \n Richness", "Arthropod \n Abundance", "Arthropod \n Richness")

metrics <- c("Arthropod \n Richness", "Arthropod \n Abundance", "Annual Species \n Richness", "Annual Cover", "Floral Visits \n per Hour")

all.new$Microsite <- relevel(all.new$Microsite, "shrub")

a <- ggplot(all.boot, aes(x=metric, y=means)) + 
  geom_errorbar(aes(ymin=V5, ymax=V6), width=.1) +
  geom_line() + ylim(-0.75, 0.75) +
  geom_point(size = 5) + scale_x_discrete(limits=rev(levels(all.boot$metric)), labels = metrics) + theme_Publication() + theme(axis.text.x=element_text(angle=90, vjust=.5)) + ylab("RII Microsite") + xlab("") + geom_hline(aes(yintercept=0)) + coord_flip() + theme(axis.title.x = element_text(size =11)) +theme(plot.margin=unit(c(0,1,0.4,1), "cm"))

b <- ggplot(all.new, aes(x=metric, y=means)) + geom_errorbar(aes(ymin=V5, ymax=V6, fill = Microsite), width=.1, position=position_dodge(width=0.5)) + geom_line(aes(fill = Microsite),position=position_dodge(width=0.5)) + geom_point(aes(fill = Microsite), position=position_dodge(width=0.5), colour="black",pch=21, size=5) + ylim(-0.75, 0.75) + scale_x_discrete(limits=rev(levels(all.new$metric)), labels = metrics) + theme_Publication() + theme(axis.text.x=element_text(angle=90, vjust=.5)) + ylab("") + xlab("") + geom_hline(aes(yintercept=0)) + scale_fill_manual("",values = c("black", "lightgray")) + theme(legend.position = c(0.8, 0.05)) + coord_flip() + ylab("RII Blooming") + theme(axis.title.x = element_text(size =11)) +theme(plot.margin=unit(c(0,1,1,1), "cm"))

plot_grid(a,b, nrow = 2)
