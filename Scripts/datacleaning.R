library(lubridate)

# Data wrangling and clean up
This section creates several .csv files that get used later for stats


### Video data clean up
This section takes the raw datas, wrangles it into three formats, adds covariates and outputs the cleaned data as .csv files which re then sourced in later scripts
```{r, video data wrangling}
#data wrangling for video data

#import datasheets
vids <- read.csv("Clean Data/videos_clean.csv")
IDlist <- read.csv("Clean Data/video_repID.csv")
str(vids)

#clean up factor names
vids$Rep <- gsub('\\s+', "", vids$Rep)
vids$microsite <- gsub('\\s+', "", vids$microsite)
vids$flowering <- gsub('\\s+', "", vids$flowering)
vids$flowering <-gsub("post", "bloom", vids$flowering)
vids$flowering <-gsub("Bloom", "bloom", vids$flowering)
vids$microsite <-gsub("Shrub", "shrub", vids$microsite)

#check for correc rep info
vids$id.check <- paste(vids$video.date, vids$plant.id, vids$Rep, vids$microsite, vids$flowering)


#need to know how many videos are empty
id.counts <- count(vids, id.check)
sum(id.counts$n)


#subset out floral visits only ie instances where an insect touched a flower. Other insect uses were extracted from videos but not included in analyses

vids$uniID <- paste(vids$Rep, vids$video.date)
vids$uniID <- gsub('\\s+', "", vids$uniID)

#subset flower visits where insect flies on
flr <- filter(vids, flowers.visits != 0 & flies.on == "Y") 
summary(flr)
str(flr)

#convert time to seconds then decimal time
flr$pos.total.time <- as.POSIXct(strptime(flr$total.time, "%H:%M:%S"))
flr$dec.total.time <- (hour(flr$pos.total.time) * 3600 + minute(flr$pos.total.time) * 60 + second(flr$pos.total.time)) / 3600
flr <- mutate(flr, prop.fl.visited = flowers.visits/flower.fov, prop.un.fl.visited = unique.fl.visited/flower.fov)

counts <- count(flr, uniID)
sum(counts$n)

#count(flr, Rep)
#count(flr, uniID)
#make fill ID list
IDlist$uniID <- paste(IDlist$Rep, IDlist$Day)
IDlist$uniID <- gsub('\\s+', "", IDlist$uniID)

#exclude certain reps. The excluded reps are those where the battery ran out or some disturbance occurred
IDlist <- filter(IDlist, Exclude != "Y")

#count visits per rep
counts <- flr %>% group_by(uniID) %>% summarise(total.visits = n()) 
sum(counts$total.visits)

#count the total number of flowers visited per video
count.fl <- flr %>% group_by(uniID) %>% summarise(total.flowers = sum(flowers.visits)) 

sum(count.fl$total.flowers)

#add reps with zero visits. Need to included videos that received no visits

zeros <- anti_join(IDlist, counts, by = "uniID")
all.data <- bind_rows(zeros, counts)
all.data <- dplyr::select(all.data, uniID, total.visits)

#replace NAs with zeros
all.data$total.visits[is.na(all.data$total.visits)] <- 0
sum(all.data$total.visits)

#join video length
all.data <- IDlist %>% dplyr::select(Length, uniID) %>% right_join(all.data,., by = "uniID")

#join covariates
cov <- read.csv("Clean Data/video_cov.csv")
str(cov)
cov$uniID <- paste(cov$Cam, cov$Date)
cov$uniID <- gsub('\\s+', "", cov$uniID)
all.data <- right_join(cov, all.data, by = "uniID")
sum(all.data$total.visits)

#convert to decimal time to standardize visits
all.data$pos.Length <- as.POSIXct(strptime(all.data$Length, "%H:%M:%S"))
all.data$dec.Length <- (hour(all.data$pos.Length) * 3600 + minute(all.data$pos.Length) * 60 + second(all.data$pos.Length)) / 3600


#plant visits per hour
all.data <- mutate(all.data, visits.per.hour = total.visits/dec.Length)
sum(all.data$total.visits)

#number of flowers visited per hour
all.data <- left_join(all.data, count.fl, by = "uniID")
all.data$total.flowers[is.na(all.data$total.flowers)] <- 0
all.data <- mutate(all.data, flowers.per.hour = total.flowers/dec.Length)
sum(all.data$total.visits)

#add weather data
weather <- read.csv("Clean Data/video_weather.csv")
weather.av <- weather %>% group_by(., Date) %>% summarise(., mean.Solar = mean(Solar), mean.Wind = mean(Wind), mean.MaxWind = mean(Max), mean.Temp = mean(Air.Temperature))
all.data <- right_join(weather.av, all.data, by = "Date")
sum(all.data$total.visits)

#data aggregrated to replicate
write.csv(all.data, "Output Data/byrep_cleaned.csv")
#data left as individual observations
write.csv(flr, "Output Data/byobs_cleaned.csv")


#I also want a dataframe grouped by rtu: Bees, syrphids, bombyliids, leps and others.

#rtu.key is a table to classify the different rtu in datafile into categories

rtu.key <- read.csv("Clean Data/video_rtu_key.csv")
rtu.data <- left_join(rtu.key, flr, by = "highest.rtu")
rtu.data <- dplyr::select(rtu.data, rtu.ag, plant.id, microsite, flowering, video.date, video.length, flower.fov, total.time, flowers.visits, unique.fl.visited, uniID)                    
count.rtu.fl <- rtu.data %>% group_by(uniID, rtu.ag) %>% summarise(total.flowers = sum(flowers.visits), total.visits = n()) 

#add zero rows
#zeros.rtu <- anti_join(IDlist, count.rtu.fl, by = "uniID")
#all.rtu.data <- bind_rows(zeros.rtu, count.rtu.fl)
#all.rtu.data <- dplyr::select(all.rtu.data, uniID, rtu.ag, total.visits, total.flowers)

#need to add the zero count rtu.
#spread, fill in zeros, then gather back
all.rtu.data <- count.rtu.fl
id <- as.data.frame(IDlist$uniID)
id$uniID <- id$`IDlist$uniID`
id <- dplyr::select(id, uniID)
id$syrphid <- 1
id$bee <- 2
id$other <- 3
id$bombylid <- 4
id$lep <- 5
id$honeybee <- 6
id <- gather(id, "rtu", "count", 2:7)
count(id, rtu)

id$join <- paste(id$uniID, id$rtu)
id$test <- 1
all.rtu.data$join <- paste(all.rtu.data$uniID, all.rtu.data$rtu.ag)

test <- dplyr::full_join(id, all.rtu.data, by = "join")

all.rtu <- dplyr::select(test, uniID.x, rtu, total.visits, total.flowers)
all.rtu$total.visits[is.na(all.rtu$total.visits)] <- 0
all.rtu$total.flowers[is.na(all.rtu$total.flowers)] <- 0

all.rtu <- dplyr::rename(all.rtu, uniID = uniID.x)

#join video length
all.rtu <- IDlist %>% dplyr::select(Length, uniID) %>% right_join(all.rtu,., by = "uniID")

#join covariates
all.rtu <- right_join(cov, all.rtu, by = "uniID")

#convert to decimal time to standardize visits
all.rtu$pos.Length <- as.POSIXct(strptime(all.rtu$Length, "%H:%M:%S"))
all.rtu$dec.Length <- (hour(all.rtu$pos.Length) * 3600 + minute(all.rtu$pos.Length) * 60 + second(all.rtu$pos.Length)) / 3600

#plant visits per hour
all.rtu <- mutate(all.rtu, visits.per.hour = total.visits/dec.Length)
all.rtu <- mutate(all.rtu, flowers.per.hour = total.flowers/dec.Length)

all.rtu <- dplyr::select(all.rtu, -SecondaryID, -Cam)

#output rtu level observations (ag. by replicate)
write.csv(all.rtu, "Output Data/rtu_by_rep.csv")
```


### Pan trap data wrangling
This sections takes the long format ID sheet (pantrap_id.csv), converts it into wide format that vegan can use, and outputs the cleaned data as .csv to be sourced in later scripts

```{r, pan trap wrangling}

#by rep
#need new simplified key to reflect species additions
long <- read.csv("Clean Data/pantraps_ID.csv")
long$highest.rtu <- gsub(" ","", long$highest.rtu)

sum(long$Quantity)
#sp.key <- read.csv("Clean Data/species_key.csv")
long$Microsite <- gsub(" ","", long$Microsite)
long$uniID <- paste(long$Date, long$PlantID, long$Microsite)
#long <- inner_join(long, sp.key, by = "highest.rtu")
#for pinned specimens need to collapse & add up the Quantity
long.fil <- dplyr::select(long, uniID, highest.rtu, Quantity)
long.ag <- long.fil %>% group_by(uniID, highest.rtu) %>% summarise(Quantity = sum(Quantity)) 
sum(long.ag$Quantity)

counts <- long.ag %>% group_by(highest.rtu) %>% summarise(Quantity = sum(Quantity))
count(long.ag, highest.rtu)

#count number of specimens of each rtu and write to a table
sp_by_rep <- long %>% group_by(highest.rtu) %>% summarise(Quantity = sum(Quantity))
write.csv(sp_by_rep, "Output Data/pans_sp_by_rep.csv")

#output in long format
write.csv(long.ag, "Output Data/pantraps_long.csv")

#spread into wide format for vegan calculations
wide <- long.ag %>% spread(highest.rtu, Quantity)

#not including damaged specimens in analyses
wide <- dplyr::select(wide, -damaged, -destroyed)

#need to replace all 0 in data frame
wide[is.na(wide)] <- 0


write.csv(wide, "Output Data/pantraps_wide.csv")
```
