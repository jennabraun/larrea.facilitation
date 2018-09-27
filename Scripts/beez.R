library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)


insects <- read.csv("Clean Data/wide_yesbeetle.csv")
bees <- select(insects, 5:7,12,14,16,36,41,47,58,62,63,65,70,71,73,85,90,94)


metadata <- read.csv("Clean Data/pantraps_cov.csv", header = TRUE)
metadata <- filter(metadata, species != "buckhorn")
metadata$plant.id <- as.character(metadata$plant.id)
str(metadata)
metadata$uniID <- paste(metadata$date, metadata$plant.id, metadata$treatment)
row.names(metadata) <- metadata$uniID


beemeta <- metadata

beemeta$abun <- apply(bees, 1, sum)
#check for total
sum(beemeta$abun)
H <- diversity(bees)
simp <- diversity(bees, "simpson")
S <- specnumber(bees)
J <- H/log(S)
beemeta$H <- H
beemeta$Simpson <- simp
beemeta$Species <- S
beemeta$Even <- J


t.test(beemeta$Species ~ beemeta$blooming)

library(lme4)
m1 <- glmer.nb(Species ~ treatment + blooming + (1|plant.id), beemeta)
summary(m1)

car::Anova(m1, type = 2)

m2 <- glmer.nb(Species ~ treatment * blooming + (1|plant.id), beemeta)
summary(m2)

library(glmmTMB)

z1 <- glmmTMB(Species ~ treatment * blooming + (1|plant.id), data = beemeta, ziformula = ~1, family = nbinom2())

summary(z1)


AIC(m1, z1)

library(indicspecies)
env <- read.csv("Clean Data/metadata_yesbeetle.csv")
#row.names(insects) <- insects$X
#insects <- select(insects, -X)
insects$total <- rowSums(insects)
insects <- filter(insects, total != 0)
env <- metadata
row.names(env) <- env$uniID
all.equal(rownames(insects), rownames(env))


env$groups <- paste(env$blooming, env$treatment)
insectkm <- kmeans(insects, centers = 4)
groupskm = insectkm$cluster
groupskm

indval = multipatt(insects, env$groups, control = how(nperm=999))
summary(indval)


#ordination
library(vegan)
r1 <- rda(bees ~ treatment + blooming, beemeta)
plot(r1)
r1
anova.cca(r1, by = "terms")






bees$total <- rowSums(bees) 
bees <- filter(bees, total != 0)
beemeta <- filter(beemeta, abun != 0)
bees <- select(bees, -total)

a1 <- adonis(bees ~ treatment + blooming, beemeta, permutations = 999, method = "bray", strata = "plant.id")

summary(a1)
a1









with(env, levels(blooming))
scl <- 3 ## scaling = 3
colvec <- c("red2", "green4", "mediumblue")
plot(r1, type = "n", scaling = scl)
with(env, points(r1, display = "sites", col = colvec[blooming],
                      scaling = scl, pch = 21, bg = colvec[blooming]))
head(with(env, colvec[blooming]))
text(r1, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")
with(env,text(cca,display="bp"))



mds.fig <- ordiplot(r1, type = "none")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = env$blooming == 
         "pre")
points(mds.fig, "sites", pch = 19, col = "blue", select = env$blooming == 
         "post")
# add confidence ellipses around habitat types
ordiellipse(r1, env$blooming, conf = 0.95, label = TRUE)

plot(r1)
summary(r1)
anova(r1)
anova(r1, by = "term", step=200)

b1 <- betadiver(insects)
plot(b1)
summary(b1)

bdist <- vegdist(bees, method = "bray")
b2 <- betadisper(bdist, beemeta$blooming)
anova(b2)
summary(b2)

b2 <- betadisper(dist[[insects]], env$blooming)

?betapart
plot(bdist)
library(betapart)
b1 <- betapart.core.abund(insects)
summary(b1)
plot(b1 ~ env$blooming)
plot(b1)
?vignette
beta.multi.abund(b1, index.family = "bray")


data(dune.env)
z <- betadiver(insects, "z")
mod <- with(env, betadisper(z, blooming))
mod
plot(mod)
anova(mod)

count.rtu <- bees %>% group_by(blooming, highest.rtu) %>% count()
ggplot(count.rtu, aes(highest.rtu, n, fill = Flowering)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))



