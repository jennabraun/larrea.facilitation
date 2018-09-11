library(dplyr)
library(tidyr)
library(ggplot2)

#by rep
#need new simplified key to reflect species additions
long <- read.csv("Clean Data/pantraps_id.csv")
long$highest.rtu <- gsub(" ","", long$highest.rtu)

sum(long$Quantity)
long$Microsite <- gsub(" ","", long$Microsite)
long$uniID <- paste(long$Date, long$PlantID, long$Microsite)

bees <- filter(long, Family == "Halictidae" | Family == "Apidae" | Family == "Megachilidae" | Family == "Melittidae" | Family == "Andrenidae")



#long <- inner_join(long, sp.key, by = "highest.rtu")
#for pinned specimens need to collapse & add up the Quantity
long.fil <- dplyr::select(bees, uniID, highest.rtu, Quantity)
long.ag <- long.fil %>% group_by(uniID, highest.rtu) %>% summarise(Quantity = sum(Quantity)) 


wide <- long.ag %>% spread(highest.rtu, Quantity)
#need to replace all 0 in data frame
wide[is.na(wide)] <- 0

insects <- wide
insects <- as.data.frame(insects)
ungroup(insects)

row.names(insects) <- insects$uniID
insects <- dplyr::select(insects, -uniID)
#filter out beetles

metadata <- read.csv("Clean Data/pantraps_cov.csv", header = TRUE)
metadata <- filter(metadata, species != "buckhorn")
metadata$plant.id <- as.character(metadata$plant.id)
str(metadata)
metadata$uniID <- paste(metadata$date, metadata$plant.id, metadata$treatment)
row.names(metadata) <- metadata$uniID






env$abun <- apply(insects, 1, sum)
#check for total
sum(env$abun)
H <- diversity(insects)
simp <- diversity(insects, "simpson")
S <- specnumber(insects)
J <- H/log(S)
env$H <- H
env$Simpson <- simp
env$Species <- S
env$Even <- J


t.test(env$abun ~ env$blooming)



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
r1 <- rda(insects ~ treatment + blooming, env)
plot(r1)
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

bdist <- vegdist(insects, method = "bray")
b2 <- betadisper(bdist, env$blooming)
anova(b2)

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

count.rtu <- bees %>% group_by(Flowering, highest.rtu) %>% count()
ggplot(count.rtu, aes(highest.rtu, n, fill = Flowering)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
