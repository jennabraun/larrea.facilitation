library(dplyr)
library(ggplot2)
library(lubridate)

vids <- read.csv("Data/Visitation/videos.csv")
IDlist <- read.csv("Data/Visitation/repID.csv")

str(vids)


vids$Rep <- gsub('\\s+', "", vids$Rep)
vids$flowering <-gsub("post", "bloom", vids$flowering)
vids$flowering <-gsub("Bloom", "bloom", vids$flowering)

#subset out floral visits only
#vids$flowers.visits <- as.numeric(vids$flowers.visits)
vids$uniID <- paste(vids$Rep, vids$video.date)
vids$uniID <- gsub('\\s+', "", vids$uniID)

flr <- filter(vids, flowers.visits != 0) 
summary(flr)
str(flr)
flr$unique.fl.visited <- as.numeric(flr$unique.fl.visited)
flr$pos.total.time <- as.POSIXct(strptime(flr$total.time, "%H:%M:%S"))
flr$dec.total.time <- (hour(flr$pos.total.time) * 3600 + minute(flr$pos.total.time) * 60 + second(flr$pos.total.time)) / 3600
flr <- mutate(flr, prop.fl.visited = flowers.visits/flower.fov, prop.un.fl.visited = unique.fl.visited/flower.fov)

#count(flr, Rep)
#count(flr, uniID)
#make fill ID list
IDlist$uniID <- paste(IDlist$Rep, IDlist$Day)
IDlist$uniID <- gsub('\\s+', "", IDlist$uniID)

#exclude certain reps
IDlist <- filter(IDlist, Exclude != "Y")

#count visits per rep
counts <- flr %>% group_by(uniID) %>% summarise(total.visits = n()) 

#add reps with zero visits
zeros <- anti_join(IDlist, counts, by = "uniID")
all.data <- bind_rows(zeros, counts)
all.data <- select(all.data, uniID, total.visits)

#replace NAs with zeros
all.data$total.visits[is.na(all.data$total.visits)] <- 0

#join video length
all.data <- IDlist %>% select(Length, uniID) %>% right_join(all.data,., by = "uniID")

#join covariates
cov <- read.csv("data/cov.csv")
cov$uniID <- paste(cov$Cam, cov$Date)
cov$uniID <- gsub('\\s+', "", cov$uniID)
all.data <- right_join(cov, all.data, by = "uniID")


#convert to decimal time to standardize visits
all.data$pos.Length <- as.POSIXct(strptime(all.data$Length, "%H:%M:%S"))
all.data$dec.Length <- (hour(all.data$pos.Length) * 3600 + minute(all.data$pos.Length) * 60 + second(all.data$pos.Length)) / 3600


#flower visits per hour
all.data <- mutate(all.data, visits.per.hour = total.visits/dec.Length)
all.data$flowers.pot <- as.numeric(all.data$flowers.pot)

#merge into four groups
all.data$t.group <- paste(all.data$treatment, all.data$flowering)


str(all.data)
str(flr)

#visit length 
mean <- flr %>% group_by(uniID) %>% summarise(mean.visit.length = mean(dec.total.time), mean.fl.visits.per.visit = mean(flowers.visits), mean.un.visits.per.visit = mean(unique.fl.visited, mean.prop = mean(prop.fl.visited), mean.un.prop = mean(mean.un.prop))) 

all.data <- right_join(mean, all.data, by = "uniID")


write.csv(all.data, "byrep_cleaned.csv")
write.csv(flr, "byobs_cleaned.csv")









