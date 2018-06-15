#long to wide
library(dplyr)
library(tidyr)

#by rep
long <- read.csv("F:/School/Chapter 2/MojaveInsectDiversity/Data/id.csv")
sp.key <- read.csv("species_key.csv")
long$Microsite <- gsub(" ","", long$Microsite)
long$uniID <- paste(long$Date, long$PlantID, long$Microsite)
long <- inner_join(long, sp.key, by = "highest.rtu")
#for pinned specimens need to collapse & add up the Quantity
long.fil <- dplyr::select(long, uniID, sp.clean, Quantity)
long.ag <- long.fil %>% group_by(uniID, sp.clean) %>% summarise(Quantity = sum(Quantity)) 


#sum(long.ag$Quantity)
write.csv(long.ag, "pantraps_long.csv")

wide <- long.ag %>% spread(sp.clean, Quantity)
wide$unknown <- wide$"?"
wide <- dplyr::select(wide, -wide$"?")

#counts <- count(long.fil, sp.clean)
#write.csv(counts, "species_coding.csv")

#species.counts <- long.ag %>% group_by(sp.clean) %>% summarise(Quantity = sum(Quantity))


#need to replace all 0 in data frame
wide[is.na(wide)] <- 0

write.csv(wide, "pantraps_wide.csv")

#only pollinators
long.simp <- dplyr::select(long, uniID, sp.simp, Quantity)
simp.ag <- long.simp %>% group_by(uniID, sp.simp) %>% summarise(Quantity = sum(Quantity)) 
write.csv(long.simp, "pantraps_long_simp.csv")

wide.simp <- simp.ag %>% spread(sp.simp, Quantity)
wide.simp$unknown <- wide.simp$"?"
wide.simp <- dplyr::select(wide.simp, -wide.simp$"?")
wide.simp[is.na(wide.simp)] <- 0

write.csv(wide.simp, "pantraps_wide_simp.csv")




