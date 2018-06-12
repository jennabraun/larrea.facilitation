library(dplyr)
library(ggplot2)
all.mag <- read.csv("F:/School/Chapter 2/larrea.visitation/insitu2017.csv")
#create new csv for just larrea
all.mag %>% filter(., s.species == "larrea") %>% write.csv("F:/School/Chapter 2/larrea.visitation/larreavisits.csv")
larrea <- read.csv("F:/School/Chapter 2/larrea.visitation/larreavisits.csv")
v.species <- read.csv("F:/School/Chapter 2/larrea.visitation/v.species.csv")
larrea <- left_join(larrea, v.species, by = "v.species")
cov <- read.csv("F:/School/Chapter 2/larrea.visitation/visit_cov.csv")
sizes <- read.csv("F:/School/Chapter 2/larreasizes.csv")
cov <- distinct(cov)
cov <- select(cov, -date)
cov$repID <- paste(cov$plant.id, "post")

larrea$repID <- paste(larrea$PlantID, larrea$pre.post)
cov <- inner_join(cov, sizes, by = "plant.id")
cov <- select(cov, -plant.id)
larrea <- left_join(larrea, cov, by = "repID")
fl.num <- select(larrea, repID, n.flowers) %>% distinct()
#N.FLOWER is only applicable to post. it was just easier to join this way!


f <-count(fl.num, repID)



#how does floral visitation relate to #flowers
fl <- filter(larrea, part.used == "flower")

count(larrea, pre.post)
larrea %>% group_by(., pre.post) %>% count(., touch.plant)
larrea %>% group_by(., pre.post) %>% count(., part.used)

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



m1 <- glm(data = repcounts.all, family = "poisson"(link="log"), n ~ n.flowers + height)
m1
summary(m1)

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
