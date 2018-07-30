#pollen counts & wrangling

library(ggplot2)
library(dplyr)
library(lme4)
library(MASS)

counts <- read.csv("Clean Data/Pollen_Corrected.csv")
cov <- read.csv("Clean Data/pollen_cov.csv")
counts$X <- as.numeric(as.character(counts$X))   
counts <- counts %>% replace(is.na(.), 0)
counts <- counts %>% mutate(all.con = rowSums(.[5:18]))
counts <- select(counts, Rep:Het.Grains, all.con)                
counts.ag <- counts %>% group_by(Rep, Slide) %>% summarise(Het = sum(Het.Grains), Con = sum(all.con))             
counts.ag <- counts.ag %>% mutate(., microsite = ifelse(substr(Rep,start = 1, stop = 1) == "S", "shrub", "open"))
counts.ag$Flower <- substr(counts.ag$Slide, 1, 1)




counts.ag <- right_join(counts.ag, cov, by = "Rep")

ggplot(data =counts.ag,(aes(microsite.x, Con))) + geom_boxplot()
ggplot(data =counts.ag,(aes(microsite.x, Het))) + geom_boxplot()
t.test(counts.ag$Con ~ counts.ag$microsite.x)

ggplot(data = counts.ag, (aes(Con))) + geom_freqpoly()
shapiro.test(counts.ag$Con)

m1 <- glmer.nb(Con ~ d.flowers + dN1 + (1|Flower/Rep), data = counts.ag)
m2 <- glmer(Het ~ d.S + dN1 + (1|Flower/Rep), family = poisson(link = "log"), data = counts.ag)

summary(m1)


summary(m2)

counts.ag$Flower <- paste(counts.ag$Rep, counts.ag$Flower)
counts.ag$Sample <- paste(counts.ag$Slide, counts.ag$Rep)
counts.ag$d.S <- counts.ag$d.S %>% replace(is.na(.), 0)
m1 <- glmer.nb(Con ~ d.flowers + dN1 + d.S + (1|Sample/Flower/Rep), data = counts.ag)
summary(m1)

m2 <- glmmPQL(Con~dN1 + d.flowers, ~1|Sample/Flower/Rep, family = quasipoisson, data = counts.ag)
summary(m2)

cor.test(counts.ag$Con, counts.ag$Het)
m3 <- glm(Con ~ d.flowers + d.S, family = quasipoisson, data = counts.ag)
summary(m3)

ggplot(counts.ag, aes(dN1, Con)) + geom_point() + geom_smooth()
ggplot(counts.ag, aes(d.S, Het)) + geom_point() + geom_smooth()
