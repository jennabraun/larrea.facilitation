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
