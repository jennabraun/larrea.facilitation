library(tidyr)
library(dplyr)
library(vegan)
library(lme4)
library(nlme)
library(ggplot2)


insects <- read.csv("pantraps_wide.csv", header = TRUE)
row.names(insects) <- insects$uniID
insects <- dplyr::select(insects, -X, -X., -uniID)
#filter out beetles
insects <- dplyr::select(insects, -Coleoptera)
metadata <- read.csv("Data/pantraps.cov.csv", header = TRUE)
metadata <- filter(metadata, species != "buckhorn")
metadata$plant.id <- as.character(metadata$plant.id)
str(metadata)
metadata$uniID <- paste(metadata$date, metadata$plant.id, metadata$treatment)
row.names(metadata) <- metadata$uniID


#check all ids in datasheets are found in the other
#zero.row <- anti_join(metadata, insects, by = "uniID")
#missing <- anti_join(insects, metadata, by = "uniID")
#write.csv(zero.row, "zeroreps.csv")
#check if insects and metadata are the same
all.equal(rownames(insects), rownames(metadata))
#sort into the same order
metadata <- metadata[rownames(insects), ]
#confirm - woo!
all.equal(rownames(insects), rownames(metadata))

#grab and wrangle weather stn data
weather <- read.csv("Data/panweather.csv")
str(weather)

weather.av <- weather %>% group_by(., date) %>% summarise(., mean.Solar = mean(Solar), mean.Wind = mean(Wind), mean.MaxWind = mean(Max), mean.Temp = mean(Air.Temperature))
metadata <- right_join(weather.av, metadata, by = "date")

metadata$abun <- apply(insects, 1, sum)
#check for total
sum(metadata$abun)
H <- diversity(insects)
simp <- diversity(insects, "simpson")
S <- specnumber(insects)
J <- H/log(S)
metadata$H <- H
metadata$Simpson <- simp
metadata$Species <- S
metadata$Even <- J

print(metadata$date)
summary(metadata)
count(metadata, date)

#no beetle output
write.csv(metadata, "Data/metadata_nobeetle.csv")
write.csv(insects, "Data/wide_nobeetle.csv")

metadata <- read.csv("Data/outputmetadata.csv")

#after this is modelling and should be moved to a separate script
boxplot(specnumber(insects) ~ metadata$treatment)
t.test(specnumber(insects) ~ metadata$treatment)

boxplot(specnumber(insects) ~ metadata$blooming)
t.test(specnumber(insects) ~ metadata$blooming)
plot(specaccum(insects), xlab = "# of samples", ylab = "# of species")
metadata$date <- as.ordered(metadata$date)

shapiro.test(specnumber(insects))
shapiro.test(metadata$abun)

m1 <- lm(metadata$abun ~ metadata$blooming + metadata$treatment + metadata$percent.cover + metadata$rich)
summary(m1)
anova(m1)

str(metadata)


m2 <- glm(data = metadata, family = poisson(link = "log"), abun ~ treatment * blooming + mean.Temp + mean.Solar + mean.Wind)
summary(m2)
m3 <- glm(data = metadata, family = poisson(link = "log"), abun ~ treatment * blooming + date)
summary(m3)
m4 <- glm(data = metadata, family = poisson(link = "log"), abun ~ treatment * blooming + mean.Temp)
AIC(m2, m3, m4)




metadata$repID <- paste(metadata$plant.id, metadata$treatment)

#is there a correlation between paired microsites?
cor.test()

m1 <- glmer(abun ~ blooming * treatment + (1|plant.id), family = poisson, data = metadata)
summary(m1)
AIC(m1)
anova(m1)

m2 <- glmer(abun ~ treatment + blooming + mean.Wind +date  + (1|plant.id), family = poisson, data = metadata )
summary(m2)
plot(m2)



m2 <- glmer(abun ~ treatment * blooming + mean.Temp  + (1|plant.id), family = poisson, data = metadata )
summary(m2)

m2 <- glmer(abun ~ treatment + blooming + (1|date) + (1|plant.id), family = poisson, data = metadata )
m2 <- glmer(abun ~ treatment + blooming + mean.Solar + (1|plant.id), family = poisson, data = metadata)
summary(m2)
m3 <- glmer(abun ~ treatment + blooming + date + (1|plant.id), family = poisson, data = metadata)

m3 <- glmer(abun ~ treatment + blooming + mean.Solar + (1|date), family = poisson, data = metadata)
summary(m3)

ggplot(data = metadata, aes(treatment, abun)) + geom_bar(stat = "identity") + facet_grid(~blooming)

m5 <- glmmPQL(H ~  blooming + treatment, random = ~1|plant.id, family = quasipoisson(link = "log"), data = metadata)
summary(m5)
?diversity
