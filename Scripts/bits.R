#cut out stats for revisions


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




#relative microsite with blooming
pre.visits <- filter(byrep, treatment == "pre")
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

#rii.shrub.visits <- rii.b(shrub.visits, c(1:7), 8:10)
#rii.open.visits <- rii.b(open.visits, c(1:7), 8:10)

#rii.bloom.visits <- rbind(rii.shrub.visits, rii.open.visits)

#from melyridae excluded pan traps



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

