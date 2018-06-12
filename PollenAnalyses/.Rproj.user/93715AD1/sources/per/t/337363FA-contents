#other EDA and graphs
library(ads)
library(ggplot2)
library(dplyr)
library(sp)
library(rgdal)
library(raster)
library(maptools)
library(spatstat)
library(spdep)
library(stringr)

t6.sp <- readOGR("Output/T6_J.shp")
t5.sp <- readOGR("Output/T5_J.shp")
t4.sp <- readOGR("Output/T4_J.shp")

t6data <- t6.sp@data
t5data <- t5.sp@data
t4data <- t4.sp@data

t6.ppp <- as(t6.sp, "ppp")
nm6 <- unmark(t6.ppp)
t5.ppp <- as(t5.sp, "ppp")
t4.ppp <- as(t4.sp, "ppp")

t6data$easting <- t6data$x_final
t6data$northing <- t6data$y_final

t6data <- dplyr::select(t6data, sp_simp, Type, Width, Length, Height)
t5data <- dplyr::select(t5data, sp_simp, Type, Width, Length, Height)
t4data <- dplyr::select(t4data, sp_simp, Type, Width, Length, Height)

all <- rbind(t6data, t5data, t4data)

all$sp_simp <- gsub("u2", "noID", all$sp_simp)


species <- count(all, sp_simp)
typs <- count(all, Type)
typs
all.sp.count <- count(all, sp_simp) %>% mutate(., proportion = n/1702)
summary <- all %>% group_by(Transect, sp_simp) %>% summarise(mean.width = mean(Width), mean.height = mean(Height), var.Width = var(Width), var.height = var(Height), sd.width = sd(Width), sd.Height = sd(Height), min.width = min(Width), min.height = min(Height))

all$Width <- as.numeric(all$Width)
all$Height <- as.numeric(all$Height)
ggplot(data = all, aes(reorder(sp_simp, Width), Width)) + geom_boxplot() + theme_Publication()


ggplot(data = all, aes(reorder(sp_simp, Width), Width)) + geom_boxplot() + theme_Publication() + scale_x_discrete(labels=c("Eriogonum \n fasciculatum", "Cylindropuntia \n echinacarpa", "Scutellaria Mexicana", "Ericameria cooperi", "Unidentified", "Menodora spinescens", "Ambrosia dumosa", "Acamptopappus \n sphaerocephalus", "DenseMorphospecies", "Cylindropuntia \n ramosissima", "Thamnosma montana", "Cylindropuntia \n acanthacarpa", "Ambrosia salsola", "Pale grey \n morphospecies", "Ephedra sp.", "Yucca schidigera","Larrea tridentata"))+ theme(axis.text.x = element_text(angle = 90, size = 16, hjust = 1, vjust = 0.5), axis.text.y = element_text(size =16)) + xlab("Species") + ylab("Width (cm)") 



ggplot(all.sp.count, aes(reorder(sp_simp, -n), n)) + geom_bar(stat = "identity", color = "black") + ylab("Count") + xlab("Species") + theme_Publication() +  scale_x_discrete(labels=c("Acamptopappus \n sphaerocephalus", "Ephedra sp.", "Ambrosia salsola","Larrea tridentata", "Thamnosma montana", "Cylindropuntia \n echinacarpa", "Unidentified", "Eriogonum \n fasciculatum", "Cylindropuntia \n ramosissima", "Scutellaria Mexicana", "Menodora spinescens", "Pale grey \n morphospecies", "Yucca schidigera", "Cylindropuntia \n acanthacarpa", "Ericameria cooperi", "DenseMorphospecies", "Ambrosia dumosa"))  + theme(axis.text.x = element_text(angle = 90, size = 16, hjust = 1, vjust = 0.5), axis.text.y = element_text(size =16))


windows()

par(mfrow = c(1,3))
den <- density(t6.ppp)
plot(den, zlim = c(0,1), legend = FALSE)
den <- density(t5.ppp)
plot(den, zlim = c(0,1))
den <- density(t4.ppp)
plot(den, zlim = c(0,1))

summary(den)       
par(mfrow = c(1,1))


plot(t6.ppp, which.marks ="Width", maxsize = 800)
plot(t5.ppp, which.marks ="Width")
plot(t4.ppp, which.marks ="Width")


t6type <- t6.ppp
marks.data6 <- as.data.frame(marks(t6type))
marks.data6 <- marks.data6 %>% dplyr::select(Type) 
marks(t6type) <- marks.data6

t5type <- t5.ppp
marks.data5 <- as.data.frame(marks(t5type))
marks.data5 <- marks.data5 %>% dplyr::select(Type) 
marks(t5type) <- marks.data5

t4type <- t4.ppp
marks.data4 <- as.data.frame(marks(t4type))
marks.data4 <- marks.data4 %>% dplyr::select(Type) 
marks(t4type) <- marks.data4


g6 <- Gmulti(t6type, marks(t6type == "cactus"), marks(t6type == "shrub"))
plot(g6)
g6.inhom <-GmultiInhom(t6type, marks(t6type == "cactus"), marks(t6type == "shrub"))
g5 <- Gmulti(t5type, marks(t5type == "cactus"), marks(t5type == "shrub"))
plot(g5)
g4 <- Gmulti(t4type, marks(t4type == "cactus"), marks(t4type == "shrub"))
plot(g4)



lambda6 <- density(t6type, bw.ppl)shr
labmda6I <- density(t6type$marks$shrub)
lambda5 <- density(t5type, bw.ppl)
lambda4 <- density(t4type, bw.ppl)


plot(envelope(t6type, Gcross, i ="cactus", j = "shrub", nsim = 99, correction = "best"), main = NULL)
plot(envelope(t5type, Gcross, i ="cactus", j = "shrub", nsim = 99, correction = "best"), main = NULL)  
plot(envelope(t4type, Gcross, i ="cactus", j = "shrub", nsim = 99, correction = "best"), main = NULL)


plot(envelope(t6.ppp, Gest, nsim = 99, correction = "best"), main = NULL)
plot(envelope(t5.ppp, Gest, nsim = 99, correction = "best"), main = NULL)
plot(envelope(t4.ppp, Gest, nsim = 99, correction = "best"),  main = NULL)

windows()
plot(envelope(t6type, Gcross, i ="shrub", j = "cactus", nsim = 19))
plot(envelope(t5type, Gcross, i ="shrub", j = "cactus", nsim = 19))  
plot(envelope(t4type, Gcross, i ="shrub", j = "cactus", nsim = 19))

par(mfrow = c(1,1))


K6 <- Kcross.inhom(t6type, "cactus", "shrub", envelope = TRUE)
K6 <- Kinhom(t6type, lambda6)
plot(K6)
plot(envelope(t6type, Kest))
K4 <- Kinhom(t4type, lambda4)
plot(K4)

boot <- lohboot(t6type, Kest)
plot(boot)

plot(envelope(t6type, Kinhom, i = "cactus", j="shrub", lambda = lambda6, nsim = 19, correction = "best"))
plot(envelope(t6type, Kest,lambda = lambda6, nsim = 19, correction = "best"))

plot(envelope(t5type, Kcross, i = "cactus", j="shrub", lambda = lambda5, nsim = 19))
plot(envelope(t4type, Kcross, i = "cactus", j="shrub", lambda = lambda4, nsim = 19))
plot(envelope(t6type, K, i = "cactus", j="shrub", lambda = lambda6, nsim = 19, correction = "best"))



plot(envelope(t6type, Kcross, i = "shrub", j="cactus", lambda = lambda6, nsim = 19), xlim = c(0,5))
plot(envelope(t5type, Kcross, i = "shrub", j="cactus", lambda = lambda5, nsim = 19), xlim = c(0,2))
plot(envelope(t4type, Kcross, i = "shrub", j="cactus", lambda = lambda4, nsim = 19), xlim = c(0,2))


#larrea and ambrosia salsola
t6species <- t6.ppp
marks.data6 <- as.data.frame(marks(t6species))
marks.data6 <- marks.data6 %>% dplyr::select(sp_simp) 
marks(t6species) <- marks.data6

t5species <- t5.ppp
marks.data5 <- as.data.frame(marks(t5species))
marks.data5 <- marks.data5 %>% dplyr::select(sp_simp) 
marks(t5species) <- marks.data5

t4species <- t4.ppp
marks.data4 <- as.data.frame(marks(t4species))
marks.data4 <- marks.data4 %>% dplyr::select(sp_simp) 
marks(t4species) <- marks.data4

g6 <- Gcross(t6species, "amb.sal", "amb.sal")
plot(g6)
g6 <- Gcross(t6species, "ephedra", "ephedra")
plot(g6)

g5 <- Gcross(t5species, "amb.sal", "amb.sal")
plot(g5)
g5 <- Gcross(t5species, "ephedra", "ephedra")
plot(g5)

g4 <- Gcross(t4species, "amb.sal", "amb.sal")
plot(g4)
g4 <- Gcross(t4species, "ephedra", "ephedra")
plot(g4)




g6rev <- Gcross(t6species, "amb.sal", "lar.tri")
plot(g6rev)


g6rev <- Gmulti(t6type, marks(t6type == "amb.sal"), marks(t6type == "lar.tri"))
plot(g6rev)

g5 <- Gmulti(t5type, marks(t5type == "cactus"), marks(t5type == "shrub"))
plot(g5)
g4 <- Gmulti(t4type, marks(t4type == "cactus"), marks(t4type == "shrub"))
plot(g4)
