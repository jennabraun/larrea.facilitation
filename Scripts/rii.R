#RII for visitation


library(dplyr)
library(tidyr)
byrep <- read.csv("byrep_cleaned.csv")
pre <- filter(byrep, flowering == "pre")
pre <- dplyr::select(pre, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, visits.per.hour, flowers.per.hour)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
pre <- arrange(pre, PlantID)
pre <- filter(pre, PlantID != 266 & PlantID != 275 & PlantID != 276 & PlantID != 196)

rii.data <- rii(pre, c(1,7), 8:9)

mean(rii.data$visits.per.hour)
mean(rii.data$flowers.per.hour)

rii.data %>% group_by(., )


post <- filter(byrep, flowering =="bloom")
post <- dplyr::select(post, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, visits.per.hour, flowers.per.hour)
post <- arrange(post, PlantID)
post <- filter(post, PlantID != 198)
post <- distinct(post)
rii.data2 <- rii(post, c(1,7), 8:9)
mean(rii.data2$visits.per.hour)
mean(rii.data2$flowers.per.hour)

all.match <- rbind(pre, post)
all.match <- arrange(all.match, PlantID)
all.match <- filter(all.match, PlantID != 196)
t.test(all.match$visits.per.hour ~ all.match$flowering)
?t.test

all.rii <- rbind(rii.data, rii.data2)

shapiro.test(all.rii$flowers.per.hour)
t.test(all.rii$visits.per.hour ~ all.rii$flowering)


t.test(all.rii$flowers.per.hour)
t.test(rii.data$visits.per.hour)
t.test(rii.data2$visits.per.hour)

boxplot(all.rii$flowers.per.hour ~ all.rii$flowering)

a1 <- aov(all.rii$flowers.per.hour ~ all.rii$flowering)
summary(a1)

#code needs fixing

shrub <- filter(byrep, treatment == "shrub")
shrub <- dplyr::select(shrub, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, visits.per.hour, flowers.per.hour)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
shrub <- arrange(shrub, PlantID)
shrub <- filter(shrub, PlantID != 196 & PlantID != "new1" & PlantID != 303 & PlantID != 299)
shrub <- subset(shrub[-43,])


open <- filter(byrep, treatment == "open")
open <- dplyr::select(open, Date, mean.Solar, mean.Wind, mean.Temp, PlantID, treatment, flowering, visits.per.hour, flowers.per.hour)
#split into 2 dataframes, rename the visitation columns and then rbind
#there are 2 unmatched reps
open <- arrange(open, PlantID)
open <- filter(open, PlantID != 266 & PlantID != "new1" & PlantID != 303 & PlantID != 299 & PlantID != 275 & PlantID != 276)
open <- subset(open[-43,])

open <- distinct(open)

rii.shrub <- rii(shrub, c(1,7), 8:9)
mean(rii.shrub$visits.per.hour)

rii.open <- rii(open, c(1,7), 8:9)
mean(rii.open$visits.per.hour)

t.test(rii.shrub$visits.per.hour)
t.test(rii.open$visits.per.hour)
t.test(rii.shrub$flowers.per.hour, rii.open$flowers.per.hour)


rii <- function(x, j, var)
{
  #parse out shrub and open
  s1 <- subset(x, treatment == "shrub", select=var)
  o1 <- subset(x, treatment == "open", select=var)
  return1 <- (s1 - o1) / (s1+o1)  # Rii formula
  # attach factors
  x1 <- x[seq(1, nrow(x), by = 2),]
  return2 <- cbind(x1[j], return1)
  return2[is.na(return2)] <- 0
  print(return2)
}

rii <- function(x, j, var)
{
  #parse out shrub and open
  s1 <- subset(x, treatment == "blooming", select=var)
  o1 <- subset(x, treatment == "pre", select=var)
  return1 <- (s1 - o1) / (s1+o1)  # Rii formula
  # attach factors
  x1 <- x[seq(1, nrow(x), by = 2),]
  return2 <- cbind(x1[j], return1)
  return2[is.na(return2)] <- 0
  print(return2)
}
