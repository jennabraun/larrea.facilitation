#summary stats for video data
library(dplyr)
library(ggplot2)

byrep <- read.csv("byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")
str(byrep)
count(byrep, flowering)
dates <- count(byrep, Date)
sum(dates$n)


byrep %>% group_by(., treatment, flowering) %>% summarise(mean(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(visits.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(median(visits.per.hour))     

byrep %>% group_by(., treatment, flowering) %>% summarise(mean(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(sd(flowers.per.hour))
byrep %>% group_by(., treatment, flowering) %>% summarise(median(flowers.per.hour))

byobs <- read.csv("byobs_cleaned.csv")

ggplot(byrtu, aes(rtu, visits.per.hour)) + geom_bar(stat = "identity") + facet_grid(flowering~treatment)
ggplot(byrtu, aes(rtu, flowers.per.hour)) + geom_bar(stat = "identity") + facet_grid(flowering~treatment)

#mean number of flowers on larrea

cov <- read.csv("Data/Video/cov.csv")        
lar.fl <- cov %>% filter(., flowering == "bloom" & treatment == "shrub") 
mean(lar.fl$flowers.shrub)
sd(lar.fl$flowers.shrub)
min(lar.fl$flowers.shrub)
max(lar.fl$flowers.shrub)