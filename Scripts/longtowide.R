#converts pan trap data from a long to a wide format for ordinations and using vegan to calculate diversity indices

library(dplyr)
library(tidyr)

#by rep
#need new simplified key to reflect species additions
long <- read.csv("Raw Data/pantraps_ID.csv")
long$highest.rtu <- gsub(" ","", long$highest.rtu)


sp <- count(long, highest.rtu)
write.csv(sp, "sp_key_pans.csv")

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

sp_by_rep <- long %>% group_by(Flowering, highest.rtu) %>% summarise(Quantity = sum(Quantity))


#sum(long.ag$Quantity)
write.csv(long.ag, "pantraps_long.csv")

wide <- long.ag %>% spread(highest.rtu, Quantity)
#wide$unknown <- wide$"?"
wide <- dplyr::select(wide, -damaged, -destroyed)

#counts <- count(long.fil, sp.clean)
#write.csv(counts, "species_coding.csv")

#species.counts <- long.ag %>% group_by(sp.clean) %>% summarise(Quantity = sum(Quantity))


#need to replace all 0 in data frame
wide[is.na(wide)] <- 0

write.csv(wide, "pantraps_wide.csv")






