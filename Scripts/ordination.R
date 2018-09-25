#ordination
library(vegan)
library(dplyr)
library(indicspecies)
library(ggvegan)

insects <- read.csv("Clean Data/wide_yesbeetle.csv")
env <- read.csv("Clean Data/metadata_yesbeetle.csv")
cov <- read.csv("Clean Data/video_cov.csv")

cov$flowering <- gsub("bloom", "post", cov$flowering)
cov$JID <- paste(cov$PlantID, cov$treatment, cov$flowering)
cov <- select(cov, JID, understory.richness, het.annual.floral.density)
#get rid of double testing 355, 356
cov <- cov[-c(355:356),]

row.names(insects) <- insects$X
insects <- select(insects, -X)
insects$total <- rowSums(insects)
insects <- filter(insects, total != 0)
#delete zero rows
env <- filter(env, uniID != "4.10.2017 275 open" & uniID!= "4.27.2017 297 open")
env$JID <- paste(env$plant.id, env$treatment, env$blooming)

#make sure community and env match up
all.equal(rownames(insects), rownames(env))
#sort into the same order
env <- env[rownames(insects), ]
#confirm - woo!
all.equal(rownames(insects), rownames(env))


env <- left_join(env, cov, by = "JID")

env <- filter(env, uniID != "4.10.2017 275 open" & uniID!= "4.27.2017 297 open")


insects <- select(insects, -total)
insects <- decostand(insects, "hellinger")

rda1 <- rda(insects ~ blooming + treatment + percent.cover + understory.richness + het.annual.floral.density, env, na = na.exclude)

anova.cca(rda1, by = "terms")
anova(rda1)
summary(rda1)
#plot(rda1, display = c("sp", "lc", "cn"))
#coef(rda1)

prda1 <-rda(insects, env$treatment, env$blooming,na = na.exclude)

anova.cca(prda1)
prda1

summary(prda1)
prda1
#ordihull(rda2, env$treatment)

#rda2 <- rda(insects ~ blooming + treatment, env, scale = TRUE, na = na.exclude)
#summary(rda2)

#anova(rda2, by = "terms")
#autoplot(rda2)


#indicator species analysis
env$groups <- paste(env$blooming, env$treatment)
indval = multipatt(insects, env$groups, control = how(nperm=999))
summary(indval)
plot(indval)

all <- cbind(insects, env)
pre.all <- filter(all, blooming == "pre")
post.all <- filter(all, blooming == "post")
pre.insects <- select(pre.all, 1:118)
pre.env <- select(pre.all, 119:141)
post.insects <- select(post.all, 1:118)
post.env <- select(post.all, 119:141)


pre.rda <- rda(pre.insects ~ treatment + percent.cover + understory.richness + het.annual.floral.density, pre.env, na = na.exclude)
pre.rda
anova.cca(pre.rda, by = "terms")
post.rda <- rda(post.insects ~ treatment + percent.cover + understory.richness + het.annual.floral.density, post.env, na = na.exclude)
post.rda
anova.cca(post.rda, by = "terms")
