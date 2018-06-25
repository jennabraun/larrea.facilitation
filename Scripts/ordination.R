#ordination
library(vegan)
library(dplyr)
insects <- read.csv("Clean Data/wide_yesbeetle.csv")
env <- read.csv("Clean Data/metadata_yesbeetle.csv")
row.names(insects) <- insects$X
insects <- select(insects, -X)
insects$total <- rowSums(insects)
insects <- filter(insects, total != 0)
env <- filter(env, uniID != "4.10.2017 275 open" & uniID!= "4.27.2017 297 open")
all.equal(rownames(insects), rownames(env))
#sort into the same order
metadata <- metadata[rownames(insects), ]
#confirm - woo!
all.equal(rownames(insects), rownames(metadata))

cca.1 <- cca(insects ~ env$treatment + env$blooming + env$mean.Solar)
insects <- decostand(insects, "hellinger")
cca1.plot <- plot(cca.1)
ordiplot(cca1.plot)
cca.1
summary(cca.1)





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
