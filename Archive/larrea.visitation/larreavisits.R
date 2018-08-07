library(dplyr)
library(ggplot2)
#all.mag <- read.csv("Clean Data/insitu_2017.csv")
#create new csv for just larrea
#all.mag %>% filter(., s.species == "larrea") %>% write.csv("Clean Data/larreavisits.csv")
larrea <- read.csv("Clean Data/larreavisits.csv")
#v.species <- read.csv("F:/School/Chapter 2/larrea.visitation/v.species.csv")
#larrea <- left_join(larrea, v.species, by = "v.species")
cov <- read.csv("Clean Data/insitu_cov_larrea.csv")
sizes <- read.csv("Clean Data/video_larreadimensions.csv")
cov <- distinct(cov)
cov <- select(cov, -date)
cov$repID <- paste(cov$plant.id, "post")

larrea$repID <- paste(larrea$PlantID, larrea$pre.post)
cov <- inner_join(cov, sizes, by = "plant.id")
cov <- select(cov, -plant.id)
larrea <- left_join(larrea, cov, by = "repID")
#fl.num <- select(larrea, repID, n.flowers) %>% distinct()
#N.FLOWER is only applicable to post. it was just easier to join this way!

count(larrea, pre.post)

#how does floral visitation relate to #flowers
fl <- filter(larrea, part.used == "flower")
fl.count <- fl %>% count(repID)

lar.all <- left_join(cov, fl.count, by = "repID")

#attach neighbour density
pans <- read.csv("Clean Data/metadata_yesbeetle.csv")
pans$repID <- paste(pans$plant.id, pans$blooming)
pans <- filter(pans, blooming == "post" & treatment == "shrub")
lar.all <- left_join(lar.all, pans, by = "repID")

lar.all$n[is.na(lar.all$n)] <- 0

#join video covariates
vid.cov <- read.csv("Clean Data/video_cov.csv")
vid.cov$flowering <- gsub("bloom", "post", vid.cov$flowering)
vid.cov$repID <- paste(vid.cov$PlantID, vid.cov$flowering)
lar.all <- vid.cov %>% filter(flowering == "post" & treatment == "shrub") %>% left_join(lar.all, vid.cov, by = "repID")

ggplot(lar.all, aes(n)) + geom_density()
m1 <- glm(n ~ n.flowers + percent.cover, family = "poisson", data = lar.all)
summary(m1)



count(larrea, pre.post)
larrea %>% group_by(., pre.post) %>% count(., touch.plant)
visitors <- fl %>% group_by(., pre.post, part.used) %>% count(., v.species)

sum(visitors$n)

repcounts <-  fl %>% group_by(., repID) %>% count(., species.clean)
fl.count <- repcounts %>% group_by(., repID) %>% summarise(., sum = sum(n))

repcounts <- inner_join(repcounts, fl.num, by = "repID")
str(repcounts)
ggplot(repcounts, aes(n.flowers, n)) + geom_point(aes(color = species.clean))

repcounts.all <- fl %>% count(., repID)
repcounts.all <- inner_join(repcounts.all, fl.num, by = "repID")

sp.count <- count(fl, species.clean)
sum(sp.count$n)
count(fl, fun.rtu)

ggplot(repcounts.all, aes(width, n)) + geom_point()

shapiro.test(repcounts.all$n)

#modelling
cov <- left_join(cov, fl.count, by = "repID")
cov$n[is.na(cov$n)] <- 0



m1 <- glm(data = cov, family = "quasipoisson"(link="log"), n ~ n.flowers)

summary(m1)
overdisp_fun(m1)
car::Anova(m1)

ggplot(cov, aes(n.flowers, n)) + geom_point(shape = 1) + geom_smooth(color = "black", size = 0.5) + theme_Publication() + xlab("Floral abundance") + ylab("Visits per 15 min")


m2 <- glm(data = repcounts.all, family = "poisson"(link="log"), n.flowers ~ height) 
summary(m2)


cor.test(repcounts.all$n.flowers, repcounts.all$width)
cor.test(repcounts.all$n.flowers, repcounts.all$height)
cor.test(repcounts.all$width, repcounts.all$height)


ggplot(counts, (aes(date, n, fill = behaviour))) + geom_bar(stat = "identity", position = "dodge")

summary(m2)
#separate uses as follow: touches a part of a plant, flies near/through/inspects the plant or uses understory in some way - yes/no for first to and just sep out understory for other


#cleaning a bit
vouches <- count(larrea, v.species)
write.csv(vouches, "v.species.csv")

fl.count <- mutate(fl.count, per.hour = sum*4)
mean(fl.count$per.hour)
