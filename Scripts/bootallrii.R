#Bootstrapping and RII calculations for all groups
#Microsites are matched!
#rii between microsites
library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)

rii.m <- function(x, j, var)
{
  #parse out shrub and open
  s1 <- subset(x, treatment == "shrub", select=var)
  o1 <- subset(x, treatment == "open", select=var)
  return1 <- (s1 - o1) / (s1+o1)  # Rii formula
  # attach factors
  x1 <- x[seq(1, nrow(x), by = 2),]
  return2 <- cbind(x1[j], return1)
  return2[is.na(return2)] <- 0
  print(return2)
}

rii.b <- function(x, j, var)
{
  #parse out shrub and open
  s1 <- subset(x, flowering == "bloom", select=var)
  o1 <- subset(x, flowering == "pre", select=var)
  return1 <- (s1 - o1) / (s1+o1)  # Rii formula
  # attach factors
  x1 <- x[seq(1, nrow(x), by = 2),]
  return2 <- cbind(x1[j], return1)
  return2[is.na(return2)] <- 0
  print(return2)
}


byrep <- read.csv("byrep_cleaned.csv")
metadata <- read.csv("Clean Data/metadata_nobeetle.csv")
yesbeetle <- read.csv("Clean Data/metadata_yesbeetle.csv")
pre <- filter(byrep, flowering == "pre")
pre <- dplyr::select(pre, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, understory.richness, visits.per.hour, flowers.per.hour)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
pre <- arrange(pre, PlantID)
pre <- filter(pre, PlantID != 266 & PlantID != 275 & PlantID != 276 & PlantID != 196)
pre.visits <- rii.m(pre, c(1:7), 8:10)


post <- filter(byrep, flowering =="bloom")
post <- dplyr::select(post, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, understory.richness, visits.per.hour, flowers.per.hour)
post <- arrange(post, PlantID)
post <- filter(post, PlantID != 198)
post <- distinct(post)
post.visits <- rii.m(post, c(1:7), 8:10)

rii.visits <- rbind(pre.visits, post.visits)

#from melyridae excluded pan traps

prepan <- filter(metadata, blooming == "pre")
prepan <- dplyr::select(prepan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, blooming, abun, percent.cover)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
prepan <- arrange(prepan, plant.id)

#prepan <- filter(prepan, plant.id != 266 & plant.id != 275 & plant.id != 276 & plant.id != 196)

pre.pan <- rii.m(prepan, c(1:7), 8:9)

postpan <- filter(metadata, blooming =="post")
postpan <- dplyr::select(postpan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, blooming, abun, percent.cover)
postpan <- arrange(postpan, plant.id)
#postpan <- filter(post, PlantID != 198)
#postpan <- distinct(postpan)
post.pan <- rii.m(postpan, c(1:7), 8:9)

rii.pan <- rbind(pre.pan, post.pan)


pre.beetle <- filter(yesbeetle, blooming == "pre")
pre.beetle <- dplyr::select(pre.beetle, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, blooming, Species)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
pre.beetle <- arrange(pre.beetle, plant.id)

#prepan <- filter(prepan, plant.id != 266 & plant.id != 275 & plant.id != 276 & plant.id != 196)
pre.beetle <- rii.m(pre.beetle, c(1:7), 8)

post.beetle <- filter(yesbeetle, blooming == "post")
post.beetle <- dplyr::select(post.beetle, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, blooming, Species)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
post.beetle <- arrange(post.beetle, plant.id)

#prepan <- filter(prepan, plant.id != 266 & plant.id != 275 & plant.id != 276 & plant.id != 196)
post.beetle <- rii.m(post.beetle, c(1:7), 8)

rii.beetle <- rbind(pre.beetle, post.beetle)

#bootstrapping
bootmean <- function(d, i) mean(d[i])

flowers.rii <- boot(rii.visits$flowers.per.hour, bootmean, R=1000, stype="i", strata = rii.visits$PlantID)

rich.rii <- boot(rii.visits$understory.richness, bootmean, R=1000, stype="i", strata = rii.visits$PlantID)
abun.rii <- boot(rii.pan$abun, bootmean, R=1000, stype="i", strata = rii.pan$plant.id)
percent.rii <- boot(rii.pan$percent.cover, bootmean, R=1000, stype="i", strata = rii.pan$plant.id)
rich.arth.rii <- boot(rii.beetle$Species, bootmean, R=1000, stype="i", strata = rii.beetle$plant.id)

ci.flowers <- boot.ci(flowers.rii)
bca.flowers <- ci.flowers$bca

ci.rich <- boot.ci(rich.rii)
bca.rich <- ci.rich$bca

ci.abun <- boot.ci(abun.rii)
bca.abun <- ci.abun$bca

ci.percent <- boot.ci(percent.rii)
bca.percent <- ci.percent$bca

ci.arth <- boot.ci(rich.arth.rii)
bca.arth <- ci.arth$bca

ci <- rbind(bca.flowers, bca.rich, bca.abun, bca.percent, bca.arth)



means <- rbind(flowers.rii$t0, rich.rii$t0, abun.rii$t0, percent.rii$t0, rich.arth.rii$t0)
colnames(means) <- c("means")
row.names(means) <- c("visits", "annual.richness", "abundance", "percent.cover", "arth.rich")

all.boot <- cbind(means, ci )
all.boot <- as.data.frame(all.boot)
all.boot$treatment <- c("microsite")


#blooming contrast
shrub.visits <- filter(byrep, treatment == "shrub")
shrub.visits <- dplyr::select(shrub.visits, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, understory.richness, visits.per.hour, flowers.per.hour)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
shrub.visits <- arrange(shrub.visits, PlantID)
shrub.visits <- filter(shrub.visits, PlantID != 196 & PlantID != "new1" & PlantID != 303 & PlantID != 299)
shrub.visits <- subset(shrub.visits[-43,])

open.visits <- filter(byrep, treatment == "open")
open.visits <- dplyr::select(open.visits, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, understory.richness, visits.per.hour, flowers.per.hour)
open.visits <- arrange(open.visits, PlantID)
open.visits <- filter(open.visits, PlantID != 266 & PlantID != "new1" & PlantID != 303 & PlantID != 299 & PlantID != 275 & PlantID != 276)
#open.visits <- subset(open.visits[-43,]) 

rii.shrub.visits <- rii.b(shrub.visits, c(1:7), 8:10)
rii.open.visits <- rii.b(open.visits, c(1:7), 8:10)

rii.bloom.visits <- rbind(rii.shrub.visits, rii.open.visits)

t.test(rii.bloom.visits$flowers.per.hour ~ rii.bloom.visits$treatment)


#from melyridae excluded pan traps

shrub.pan <- filter(metadata, treatment == "shrub")
shrub.pan$flowering <- shrub.pan$blooming
shrub.pan$flowering <- gsub("post", "bloom", shrub.pan$flowering)
shrub.pan <- dplyr::select(shrub.pan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, flowering, abun, percent.cover)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
shrub.pan <- arrange(shrub.pan, plant.id)
#at least six mismatches
shrub.pan <- filter(shrub.pan, plant.id != 104 & plant.id != 114 & plant.id != 182 & plant.id != 261 & plant.id != 263 & plant.id != 284 & plant.id != 285& plant.id != 287& plant.id != 301& plant.id != 302& plant.id != 303& plant.id != 299)




open.pan <- filter(metadata, treatment =="open")
open.pan$flowering <- open.pan$blooming
open.pan$flowering <- gsub("post", "bloom", open.pan$flowering)
open.pan <- dplyr::select(open.pan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, flowering, abun, percent.cover)
open.pan <- arrange(open.pan, plant.id)
#postpan <- filter(post, PlantID != 198)
#postpan <- distinct(postpan)

open.pan <- filter(open.pan, plant.id != 104 & plant.id != 114 & plant.id != 182 & plant.id != 261 & plant.id != 263 & plant.id != 284 & plant.id != 285& plant.id != 287& plant.id != 301& plant.id != 302& plant.id != 303& plant.id != 299)



rii.open.pan <- rii.b(open.pan, c(1:7), 8:9)
rii.shrub.pan <- rii.b(shrub.pan, c(1:7), 8:9)

rii.bloom.pan <- rbind(rii.open.pan, rii.shrub.pan)

#species richness
shrub.beet.pan <- filter(yesbeetle, treatment == "shrub")
shrub.beet.pan$flowering <- shrub.beet.pan$blooming
shrub.beet.pan$flowering <- gsub("post", "bloom", shrub.beet.pan$flowering)
shrub.beet.pan <- dplyr::select(shrub.beet.pan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, flowering, Species)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
shrub.beet.pan <- arrange(shrub.beet.pan, plant.id)
#at least six mismatches
shrub.beet.pan <- filter(shrub.beet.pan, plant.id != 104 & plant.id != 114 & plant.id != 182 & plant.id != 261 & plant.id != 263 & plant.id != 284 & plant.id != 285& plant.id != 287& plant.id != 301& plant.id != 302& plant.id != 303& plant.id != 299)


open.beet.pan <- filter(yesbeetle, treatment =="open")
open.beet.pan$flowering <- open.beet.pan$blooming
open.beet.pan$flowering <- gsub("post", "bloom", open.beet.pan$flowering)
open.beet.pan <- dplyr::select(open.beet.pan, date, mean.Solar, mean.Wind, mean.Temp, plant.id, treatment, flowering, Species)
open.beet.pan <- arrange(open.beet.pan, plant.id)
#postpan <- filter(post, PlantID != 198)
#postpan <- distinct(postpan)

open.beet.pan <- filter(open.beet.pan, plant.id != 104 & plant.id != 114 & plant.id != 182 & plant.id != 261 & plant.id != 263 & plant.id != 284 & plant.id != 285& plant.id != 287& plant.id != 301& plant.id != 302& plant.id != 303& plant.id != 299)

rii.open.beet.pan <- rii.b(open.beet.pan, c(1:7), 8)
rii.shrub.beet.pan <- rii.b(shrub.beet.pan, c(1:7), 8)

rii.bloom.species <- rbind(rii.open.beet.pan, rii.shrub.beet.pan)


#flowers.rii <- boot(rii.bloom.visits$flowers.per.hour, bootmean, R=1000, stype="i", strata = rii.bloom.visits$PlantID)

#rich.rii <- boot(rii.bloom.visits$understory.richness, bootmean, R=1000, stype="i", strata = rii.bloom.visits$PlantID)
#abun.rii <- boot(rii.bloom.pan$abun, bootmean, R=1000, stype="i", strata = rii.bloom.pan$plant.id)
#percent.rii <- boot(rii.bloom.pan$percent.cover, bootmean, R=1000, stype="i", strata = rii.bloom.pan$plant.id)
#rich.arth.rii <- boot(rii.bloom.species$Species, bootmean, R=1000, stype="i", strata = rii.bloom.species$plant.id)

#ci.flowers <- boot.ci(flowers.rii)
#bca.flowers <- ci.flowers$bca

#ci.rich <- boot.ci(rich.rii)
#bca.rich <- ci.rich$bca

#ci.abun <- boot.ci(abun.rii)
#bca.abun <- ci.abun$bca

#ci.percent <- boot.ci(percent.rii)
#bca.percent <- ci.percent$bca
#ci.arth <- boot.ci(rich.arth.rii)
#bca.arth <- ci.arth$bca

#ci <- rbind(bca.flowers, bca.rich, bca.abun, bca.percent, bca.arth)



#means.bloom <- rbind(flowers.rii$t0, rich.rii$t0, abun.rii$t0, percent.rii$t0, rich.arth.rii$t0)
#colnames(means.bloom) <- c("means")
#row.names(means.bloom) <- c("visits", "annual.richness", "abundance", "percent.cover", "arth.rich")

#all.boot.bloom <- cbind(means.bloom, ci )
#all.boot.bloom <- as.data.frame(all.boot.bloom)
#all.boot.bloom$treatment <- c("bloom")

#all.boot <- rbind(all.boot, all.boot.bloom)

all.boot$metric <- c("visits", "annual.richness", "abundance", "percent.cover", "arth.rich")# "visits", "annual.richness", "abundance", "percent.cover", "arth.rich")

#all.boot$treatment <- as.factor(all.boot$treatment)
#all.boot$treatment <- relevel(all.boot$treatment, "microsite", "bloom")

all.boot$metric <- as.factor(all.boot$metric)
all.boot$metric <- relevel(all.boot$metric, "visits", "abundance", "arth.rich", "percent.cover", "annual.richness")

#labels
metrics <- c("Floral Visits \n per Hour", "Arthropod \n Abundance", "Arthropod \n Richness", "Percent\n Annual Cover", "Annual Species \n Richness")
labels <- c("microsite" = "Microsite","bloom" ="Blooming")



## calculate separate visitation pre/post by microsite bootstrapped


shrub.visits.rii <- boot(rii.shrub.visits$flowers.per.hour, bootmean, R=10000, stype="i")
open.visits.rii <- boot(rii.open.visits$flowers.per.hour, bootmean, R=10000, stype="i")


shrub.cover.rii <- boot(rii.shrub.pan$percent.cover, bootmean, R=10000, stype="i")
open.cover.rii <- boot(rii.open.pan$percent.cover, bootmean, R=10000, stype="i")

shrub.rich.rii <- boot(rii.shrub.visits$understory.richness, bootmean, R=10000, stype="i")
open.rich.rii <- boot(rii.open.visits$understory.richness, bootmean, R=10000, stype="i")

shrub.abun.rii <- boot(rii.shrub.pan$abun, bootmean, R=10000, stype="i")
open.abun.rii <- boot(rii.open.pan$abun, bootmean, R=10000, stype="i")

shrub.arth.rich.rii <- boot(rii.shrub.beet.pan$Species, bootmean, R=10000, stype="i")
open.arth.rich.rii <- boot(rii.open.beet.pan$Species, bootmean, R=10000, stype="i")


ci.shrub.visits <- boot.ci(shrub.visits.rii)
bca.shrub.visits <- ci.shrub.visits$bca

ci.open.visits <- boot.ci(open.visits.rii)
bca.open.visits <- ci.open.visits$bca

ci.shrub.cover <- boot.ci(shrub.cover.rii)
bca.shrub.cover <- ci.shrub.cover$bca
ci.open.cover <- boot.ci(open.cover.rii)
bca.open.cover <- ci.open.cover$bca

ci.shrub.rich <- boot.ci(shrub.rich.rii)
bca.shrub.rich <- ci.shrub.rich$bca
ci.open.rich <- boot.ci(open.rich.rii)
bca.open.rich <- ci.open.rich$bca

ci.shrub.abun <- boot.ci(shrub.abun.rii)
bca.shrub.abun <- ci.shrub.abun$bca
ci.open.abun <- boot.ci(open.abun.rii)
bca.open.abun <- ci.open.abun$bca

ci.shrub.arth.rich <- boot.ci(shrub.arth.rich.rii)
bca.shrub.arth.rich <- ci.shrub.arth.rich$bca
ci.open.arth.rich <- boot.ci(open.arth.rich.rii)
bca.open.arth.rich <- ci.open.arth.rich$bca


means.all <- rbind(shrub.visits.rii$t0, open.visits.rii$t0,shrub.cover.rii$t0, open.cover.rii$t0, shrub.rich.rii$t0, open.rich.rii$t0, shrub.abun.rii$t0, open.abun.rii$t0, shrub.arth.rich.rii$t0, open.arth.rich.rii$t0)
colnames(means.all) <- c("means")
ci.all <- rbind(bca.shrub.visits, bca.open.visits, bca.shrub.cover, bca.open.cover, bca.shrub.rich, bca.open.rich, bca.shrub.abun, bca.open.abun, bca.shrub.arth.rich, bca.open.arth.rich)


all.new <- cbind(means.all, ci.all)
all.new <- as.data.frame(all.new)

all.new$Microsite <- c("shrub", "open", "shrub", "open","shrub", "open","shrub", "open","shrub", "open")
all.new$metric <- c("visits", "visits", "percent.cover", "percent.cover", "annual.richness", "annual.richness", "arth.abundance", "arth.abundance", "arth.richness", "arth.richness")


all.new$metric <- as.factor(all.new$metric)
all.new$metric <- relevel(all.new$metric, "visits", "arth.abundance", "arth.richness", "percent.cover", "annual.richness")

metrics <- c("Floral Visits \n per Hour", "Arthropod \n Abundance", "Arthropod \n Richness", "Annual Cover", "Annual Species \n Richness")

#summary(a1)
#plot this
library(cowplot)
p1 <- ggplot(all.boot, aes(x=metric, y=means)) + 
  geom_errorbar(aes(ymin=V5, ymax=V6), width=.1) +
  geom_line() +
  geom_point() + scale_x_discrete(limits=c("visits", "abundance", "arth.rich", "percent.cover", "annual.richness"), labels = metrics) + theme_Publication() + theme(axis.text.x=element_text(angle=90, vjust=.5)) + ylab("Relative Interation Index") + xlab("") + geom_hline(aes(yintercept=0))



p2 <- ggplot(all.new, aes(x=metric, y=means, linetype = Microsite)) + geom_errorbar(aes(ymin=V5, ymax=V6), width=.1, position=position_dodge(width=0.3)) + geom_line(position=position_dodge(width=0.3)) + geom_point(position=position_dodge(width=0.3)) + scale_linetype_manual(values=c("twodash", "solid")) + scale_x_discrete(limits=c("visits", "arth.abundance", "arth.richness", "percent.cover", "annual.richness"), labels = metrics) + theme_Publication() + theme(axis.text.x=element_text(angle=90, vjust=.5)) + ylab("") + xlab("") + geom_hline(aes(yintercept=0)) + scale_color_manual(values = c("black", "gray")) + theme(legend.position = c(0.8, 0.05))




plot_grid(p1, p2, labels = c("A", "B"), align = "h")



png(file="mygraphic.png",width=1222,height=750)
plot(x=rnorm(10),y=rnorm(10),main="example")
dev.off()
