#indicator species analysis
library(indicspecies)
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
#metadata <- metadata[rownames(insects), ]
#confirm - woo!
#all.equal(rownames(insects), rownames(metadata))
env$groups <- paste(env$blooming, env$treatment)
insectkm <- kmeans(insects, centers = 4)
groupskm = insectkm$cluster
groupskm

indval = multipatt(insects, env$groups, control = how(nperm=999))
summary(indval)
