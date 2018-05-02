#summary stats for video data
library(dplyr)
library(ggplot2)

se <- function(x) sd(x)/sqrt(length(x)) ## SE


byrep <- read.csv("byrep_cleaned.csv")
allbyrep <- read.csv("byrep_cleaned_all.csv")
byrtu <- read.csv("rtu_by_rep.csv")
byobs <- read.csv("byobs_cleaned.csv")
str(byrep)
count(byrep, flowering)
dates <- count(byrep, Date)
sum(dates$n)
#so I can reuse code, fix later
byrep <- allbyrep
sum(byrep$total.visits)

#visits per hour
byrep %>% group_by(., treatment, flowering) %>% summarise(mean(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(se(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(min(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(max(visits.per.hour))

byrep %>% group_by(., treatment) %>% summarise(mean(visits.per.hour))
byrep %>% group_by(., treatment) %>% summarise(sd(visits.per.hour))
byrep %>% group_by(., treatment) %>% summarise(se(visits.per.hour))
byrep %>% group_by(., treatment) %>% summarise(min(visits.per.hour))
byrep %>% group_by(., treatment) %>% summarise(max(visits.per.hour))

byrep %>% group_by(., flowering) %>% summarise(mean(visits.per.hour))
byrep %>% group_by(., flowering) %>% summarise(sd(visits.per.hour))
byrep %>% group_by(., flowering) %>% summarise(se(visits.per.hour))
byrep %>% group_by(., flowering) %>% summarise(min(visits.per.hour))
byrep %>% group_by(., flowering) %>% summarise(max(visits.per.hour))

byrep %>% group_by(., treatment, flowering) %>% summarise(median(visits.per.hour))     



#flowers per hour

byrep %>% group_by(., treatment, flowering) %>% summarise(mean(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(se(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(min(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(max(flowers.per.hour))

byrep %>% group_by(., treatment) %>% summarise(mean(flowers.per.hour))
byrep %>% group_by(., treatment) %>% summarise(sd(flowers.per.hour))
byrep %>% group_by(., treatment) %>% summarise(se(flowers.per.hour))
byrep %>% group_by(., treatment) %>% summarise(min(flowers.per.hour))
byrep %>% group_by(., treatment) %>% summarise(max(flowers.per.hour))

byrep %>% group_by(.,flowering) %>% summarise(mean(flowers.per.hour))
byrep %>% group_by(.,flowering) %>% summarise(sd(flowers.per.hour))
byrep %>% group_by(.,flowering) %>% summarise(se(flowers.per.hour))
byrep %>% group_by(.,flowering) %>% summarise(min(flowers.per.hour))
byrep %>% group_by(.,flowering) %>% summarise(max(flowers.per.hour))

#visits/flower/hour
byrep %>% group_by(., treatment, flowering) %>% summarise(mean(visits.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(visits.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(se(visits.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(min(visits.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(max(visits.flower.hr))

byrep %>% group_by(., treatment) %>% summarise(mean(visits.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(sd(visits.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(se(visits.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(min(visits.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(max(visits.flower.hr))

byrep %>% group_by(., flowering) %>% summarise(mean(visits.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(sd(visits.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(se(visits.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(min(visits.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(max(visits.flower.hr))


#flowers/flower/pr
byrep %>% group_by(., treatment, flowering) %>% summarise(mean(flowers.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(flowers.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(se(flowers.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(min(flowers.flower.hr))
byrep %>% group_by(., treatment, flowering) %>% summarise(max(flowers.flower.hr))

byrep %>% group_by(., treatment) %>% summarise(mean(flowers.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(sd(flowers.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(se(flowers.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(min(flowers.flower.hr))
byrep %>% group_by(., treatment) %>% summarise(max(flowers.flower.hr))

byrep %>% group_by(., flowering) %>% summarise(mean(flowers.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(sd(flowers.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(se(flowers.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(min(flowers.flower.hr))
byrep %>% group_by(., flowering) %>% summarise(max(flowers.flower.hr))

means <- byrep %>% group_by(., treatment, flowering) %>% summarise(mean = mean(flowers.pot))
sds <- byrep %>% group_by(., treatment, flowering) %>% summarise(sd = sd(flowers.pot))
ses <- byrep %>% group_by(., treatment, flowering) %>% summarise(se = se(flowers.pot))
mins <- byrep %>% group_by(., treatment, flowering) %>% summarise(min = min(flowers.pot))
maxs <- byrep %>% group_by(., treatment, flowering) %>% summarise(max = max(flowers.pot))
summary <- rbind(means, sds, ses, mins, maxs)

write.csv(summary, "summary.csv")


sizes <- read.csv("larreasizes.csv")
str(sizes)
sizesummary <- matrix(data = NA)
m.Width <- mean(sizes$width, na.rm = TRUE)
m.Height <- mean(sizes$height, na.rm = TRUE)
sd.Width <- sd(sizes$width, na.rm = TRUE)
sd.Height <- sd(sizes$height, na.rm = TRUE)
se.Width <- se(sizes$width)
se.Height <- se(sizes$height)
min.Width <- min(sizes$width, na.rm = TRUE)
min.Height <- min(sizes$height, na.rm = TRUE)
max.Width <- max(sizes$width, na.rm = TRUE)
max.Height <- max(sizes$height, na.rm = TRUE)


summary <- cbind(m.Width, sd.Width, se.Width, min.Width, max.Width, m.Height, sd.Height, se.Height, min.Height, max.Height)
?matrix



byobs <- read.csv("byobs_cleaned.csv")

ggplot(byrtu, aes(rtu, visits.per.hour)) + geom_bar(stat = "identity") + facet_grid(flowering~treatment)
ggplot(byrtu, aes(rtu, flowers.per.hour)) + geom_bar(stat = "identity") + facet_grid(flowering~treatment)
ggplot(byrtu, aes(rtu, flowers.per.hour)) + geom_boxplot() + facet_grid(flowering~treatment)


#mean number of flowers on larrea

cov <- read.csv("Data/Video/cov.csv")        
lar.fl <- cov %>% filter(., flowering == "bloom" & treatment == "shrub") 
mean(lar.fl$flowers.shrub)
sd(lar.fl$flowers.shrub)
min(lar.fl$flowers.shrub)
max(lar.fl$flowers.shrub)


#try to do a correlation between pans and visitation
pans <- read.csv("F:/School/Chapter 2/MojaveInsectDiversity/data/metadata_nobeetle.csv")
pans$joinID <- paste(pans$plant.id, pans$blooming, pans$treatment)
byrep$joinID <- paste(byrep$PlantID, byrep$flowering, byrep$treatment)
pansjoin <- left_join(pans, byrep, by = "joinID")

cor.test(pansjoin$abun, pansjoin$flowers.per.hour)
cor.test(pansjoin$abun, pansjoin$visits.per.hour)
cor.test(pansjoin$H, pansjoin$flowers.per.hour)
cor.test(pansjoin$H, pansjoin$visits.per.hour)


reps <- byrep
reps$flowering <- gsub("blooming", "post", reps$flowering)
reps$flowering <- gsub("bloom", "post", reps$flowering)
reps$repID <- paste(reps$PlantID, reps$flowering)
visitsjoin <- right_join(fl.count, reps, by = "repID")

cor.test(visitsjoin$per.hour, visitsjoin$visits.per.hour)
cor.test(visitsjoin$per.hour, visitsjoin$flowers.per.hour)

#length of flower visit
#need to join simplified rtu
rtu.key <- read.csv("rtus.csv")
rtu.data <- right_join(rtu.key, byobs, by = "highest.rtu")
times <- rtu.data %>% group_by(., microsite, flowering, rtu.ag) %>% summarise(mean(dec.total.time))
ggplot(rtu.data, aes(rtu.ag, dec.total.time)) + geom_boxplot() + facet_grid(microsite~flowering)

ggplot(means, aes(rtu, mean)) + geom_bar(aes(fill = treatment), stat = "identity", position = "dodge") + facet_grid(~flowering)

ggplot(means, aes(rtu, mean)) + geom_boxplot(aes(fill = treatment), position = "dodge") + facet_grid(~flowering)

sum(byrtu$visits.per.hour)
ggplot(byrtu, aes(visits.per.hour)) + geom_freqpoly(aes(fill = rtu)) + facet_grid(treatment~flowering) 

means <- byrtu %>% group_by(.,flowering,treatment, rtu) %>% summarise(., mean = mean(visits.per.hour))

ggplot(means, aes(flowering, mean, color = rtu)) + geom_bar() + facet_grid(~treatment)                                                   

chisq.test()
?wilcox.test
wilcox.test(pre$flowers.per.hour ~ pre$treatment)
wilcox.test(post$flowers.per.hour ~ post$treatment)
kruskal.test(pre$flowers.per.hour ~ pre$treatment)
kruskal.test(post$flowers.per.hour ~ post$treatment)

mean(byrep$understory.richness, na.rm = TRUE)
str(byrep)
byrep %>% group_by(., treatment) %>% summarise(mean(understory.richness, na.rm = TRUE))
byrep %>% group_by(., flowering, treatment) %>% summarise(mean(het.annual.floral.density, na.rm = TRUE))

pre <- byrep %>% filter(., flowering == "pre")
t.test(pre$het.annual.floral.density ~ pre$treatment)
t.test(pre$understory.richness ~ pre$treatment)
post <- byrep %>% filter(., flowering == "bloom")
t.test(post$het.annual.floral.density ~ post$treatment)
t.test(post$understory.richness ~ post$treatment)
t.test(byrep$understory.richness ~ byrep$flowering)
t.test(byrep$het.annual.floral.density ~ byrep$flowering)
?aov
a1 <- aov(byrep$visits.per.hour ~ byrep$flowering * byrep$treatment)
summary(a1)
TukeyHSD(a1)
byrep %>% group_by(., flowering, treatment) %>% summarise(mean(visits.per.hour, na.rm = TRUE))
