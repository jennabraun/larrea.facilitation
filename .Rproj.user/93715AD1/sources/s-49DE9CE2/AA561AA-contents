#EDA and stats for no beetles
library(dplyr)
library(tidyr)
library(MASS)
library(vegan)
library(ggplot2)
library(lme4)
metadata <- read.csv("Data/metadata_nobeetle.csv")
insects <- read.csv("Data/wide_nobeetle.csv", row.names = 1)

plot(ecdf(metadata$abun))
qqnorm(metadata$abun)
hist(metadata$abun, 30)
Y <- rnorm(228, mean = mean(metadata$abun),
                                  sd = sd(metadata$abun))
hist(Y, 30)
plot(density(metadata$abun))

metadata %>% group_by(.,treatment, blooming) %>% summarise(mean(rich))
sum(metadata$abun)
shapiro.test(boxcox(metadata$abun))


ggplot(metadata, aes(percent.cover)) + geom_density()


mean(metadata$abun)
var(metadata$abun)
sd(metadata$abun)
sum(metadata$abun)
#my data is very over-disperse - maybe quasi-poisson?
#do I need to estimate the dispersion factor?
#can't use gamma
plot(mean(metadata$abun) ~ var(metadata$abun))

m1 <- glm(abun ~ treatment + blooming + mean.Temp, family = poisson, data = metadata)
m2 <- glm(abun ~ treatment + blooming + mean.Temp + date, family = quasipoisson, data = metadata)
m3 <- glm(abun ~ treatment * mean.Temp + mean.Wind + percent.cover + rich, family = quasipoisson(link = "log"), data = metadata)
summary(m1)
summary(m2)
summary(m3)
ggplot(metadata, aes(mean.Solar, abun)) + geom_smooth(stat="smooth")


dotchart(metadata$abun)

anova(m1, m2, m3, test = "Chi")

m2 <- glm(abun ~ treatment + blooming + mean.Temp, family = quasipoisson, data = metadata)
m3 <- glm.nb(abun ~ treatment + blooming + mean.Temp, data = metadata)


m4 <- glmmPQL(abun ~ treatment + blooming + mean.Temp, random = ~1 |plant.id, family = quasipoisson(), data = metadata)
              
summary(m4)

metadata$repID <- paste(metadata$plant.id, metadata$treatment)

g3 <- glmer.nb(abun ~ treatment + blooming + (1|repID) , data = metadata)
summary(g3)
shapiro.test(g3)
g3.1 <- glmer.nb(abun ~ treatment * blooming +(1|repID), data = metadata)
summary(g3.1)
g3.2 <- glmer.nb(abun ~ treatment + blooming + mean.Temp + (1|repID), data = metadata)
g.3.3 <- glmer.nb(abun ~ treatment * blooming + mean.Temp +(1|repID), data = metadata)
shapiro.test(g3@residuals)

g3 <- glmer.nb(Species ~ treatment + blooming + (1|repID), data = metadata)
summary(g3)
car::Anova(g3, type = 2)


m4 <- glmmPQL(Simpson ~ treatment * blooming, random = ~1 |repID, family = quasipoisson(), data = metadata)
summary(m4)

shapiro.test(m4$residuals)
shapiro.test(metadata$Simpson)


summary(g3)
summary(g3.1)
summary(g.3.3)
anova(g3, g3.1, g3.2, g.3.3)

summary(g3.2)
summary(g3)
str(metadata)

g3 <- glmer.nb(H ~ treatment + blooming + (1|repID), data = metadata)
summary(g3)
car::Anova(g3, type = 2)

shapiro.test(metadata$Simpson)


#let's try a bit of ordination for no beetles
comm <- insects
comm <- decostand(comm, method = "total")
apply(comm, 1, sum)
#need to get rid of zero rows
comm <- mutate(comm, sum = rowSums())
sums <- rowSums(comm)
comm <- cbind(comm, sums)
comm <- filter(comm, sums >0)
comm <- dplyr::select(comm, -sums)
metadata <- filter(metadata, abun >0)
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity", hang = -1)
rect.hclust (comm.bc.clust, k = 6, border = 1:4)

beta <- vegdist(comm, binary = TRUE) #calculate beta diversity
mean(beta)
betadisper(beta, as.factor(metadata$blooming), type = "centroid")
betaFactor <- betadisper(beta, as.factor(metadata$blooming), type = "centroid")
plot(betaFactor,axes = c(1,2), cex = 0.7, hull = TRUE)
boxplot(betaFactor)
TukeyHSD(betaFactor, which = "group", ordered = FALSE,
         conf.level = 0.95)


metadata %>% group_by(., treatment, blooming) %>% summarise(., mean(H))
metadata %>% group_by(., treatment, blooming) %>% summarise(., mean(Simpson))
?betadisper


comm.rda <- rda(comm)
plot(comm.rda)
biplot(comm.rda)
spe.ch <- vegdist(comm, "euc") 
spe.ch.complete <- hclust(spe.ch, method = "wald")
plot(spe.ch.complete)
#I want to try k-means but need to normalize data first. Not appropriate for raw species data
spe.norm <- decostand(comm, "normalize")
spe.kmeans <- kmeans(spe.norm, centers = 6, nstart = 100)
plot(spe.kmeans)
table(kmeans$cluster)


#lets try a CCA
spe.ca <- cca(comm)
spe.ca
summary(spe.ca)
ev2 <- spe.ca$CA$eig
evplot(ev2)
biplot(spe.ca, scaling =1)
plot(spe.ca)

spe.rda <- rda(comm)
biplot(spe.rda)
comm <- decostand(comm, "hellinger")
env <- dplyr::select(metadata, mean.Solar, mean.Wind, mean.Temp, treatment, blooming, percent.cover, rich)
spe.rda <- rda(comm ~ ., env)
spe.rda
coef(spe.rda)
plot(spe.rda)
spe.sc <- scores(spe.rda, choices = 1:2, scaling = 1, display = "sp")
plot(spe.sc)
anova.cca(spe.rda, by = "axis", step = 1000)

spe.cca <- cca(comm ~., env)
summary(spe.cca)
plot(spe.cca, scaling = 1, display = c("sp", "cn"), labels = TRUE)
vif.cca(spe.cca)
a <- anova.cca(spe.cca, step = 1000)
summary(a)
