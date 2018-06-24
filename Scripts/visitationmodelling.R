#visitation models and stats
library(dplyr)
library(ggplot2)
library(lme4)

byobs <- read.csv("byobs_cleaned.csv")
byrep <- read.csv("byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")
byrep$repID <- paste(byrep$PlantID, byrep$treatment)
byrep$treatment <- relevel(byrep$treatment, "open")
byrep$flowering <- relevel(byrep$flowering, "pre")

byrtu$repID <- paste(byrtu$PlantID, byrep$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")

count(byrep, total.flowers)


fm1 <- glmer.nb(total.flowers ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)
summary(m1)


diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
any(diag.vals < 1e-6) # FALSE

fm1.restart <- update(fm1, start=pars)
summary(fm1.restart)
shapiro.test(residuals(fm1.restart))
overdisp_fun(fm1.restart)
plot(residuals(fm1.restart)~predict(fm1.restart))
car::Anova(fm1.restart, type = 2)


m2 <- glmer.nb(total.flowers ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)
m2.restart <- update(m2, start=1)
summary(m2)

m3 <- glmer.nb(total.visits ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)
m3.restart <- update(m3, start=pars)
summary(m3.restart)
car::Anova(m3.restart, type = 2)




devfun <- update(fm1, devFunOnly=TRUE)
if (isLMM(fm1)) {
  pars <- getME(fm1,"theta")
} else {
  ## GLMM: requires both random and fixed parameters
  pars <- getME(fm1, c("theta","fixef"))
}
if (require("numDeriv")) {
  cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
  cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
  cat("scaled gradient:\n")
  print(scgrad <- solve(chol(hess), grad))
}
## compare with internal calculations:
fm1@optinfo$derivs


m3 <- glmer.nb(total.visits ~ treatment + (1|repID/PlantID), data = byrep)
summary(m3)
overdisp_fun(m3)



shapiro.test(residuals(m3))

#rtu stats
byrtu$repID <- paste(byrtu$PlantID, byrtu$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")

m4 <- glmer.nb(total.flowers ~ treatment * rtu * flowering + flowers.pot +  offset(log(dec.Length)) + (1|repID), data = byrtu)
m4.restart <- update(m4, start=3)
summary(m4.restart)
library(lsmeans)
lsmeans(m4.restart, pairwise~rtu*treatment*blooming)
summary(m4)

#visitation
rtu.key <- read.csv("Clean Data/video_rtu_key.csv")
rtu.data <- left_join(rtu.key, byobs, by = "highest.rtu")
rtu.data <- dplyr::select(rtu.data, rtu.ag, plant.id, microsite, flowering, video.date, video.length, flower.fov, total.time, flowers.visits, unique.fl.visited, uniID, dec.total.time)                    
#count.rtu.fl <- rtu.data %>% group_by(uniID, rtu.ag) %>% summarise(total.flowers = sum(flowers.visits), total.visits = n()) 

ggplot(rtu.data, aes(rtu.ag, dec.total.time)) + geom_boxplot() + facet_grid(flowering~microsite)

ggplot(rtu.data, aes(microsite, dec.total.time)) + geom_boxplot() + facet_grid(~flowering)
ggplot(rtu.data, aes(flowering, dec.total.time)) + geom_boxplot() + facet_grid(~microsite)

ggplot(rtu.data, aes(dec.total.time)) + geom_freqpoly()

rtu.data$repID <- paste(rtu.data$plant.id, rtu.data$microsite)
rtu.data$microscite <-as.factor(rtu.data$microsite)
rtu.data$microsite <- relevel(rtu.data$microsite, "open")
rtu.data$flowering <- relevel(rtu.data$flowering, "pre")

m1 <- glmer(dec.total.time ~ flowering + (1|repID), family = Gamma(link = "inverse"), data = rtu.data)
summary(m1)
m2 <- glmer(dec.total.time ~ microsite + (1|repID), family = Gamma(link = "inverse"), data = rtu.data)
summary(m2)
m3 <- m1 <- glmer(dec.total.time ~ flowering + microsite + (1|repID), family = Gamma(link = "inverse"), data = rtu.data)
summary(m3)
m4 <- glmer(dec.total.time ~ flowering + microsite + (1|repID), family = Gamma(link = "inverse"), data = rtu.data)
summary(m4)

rtu.data <- mutate(rtu.data, prop.visited = unique.fl.visited/flower.fov)

ggplot(rtu.data, aes(prop.visited)) + geom_freqpoly()
m1 <- glmer(prop.visited ~ flowering*rtu.ag + (1|repID), family = Gamma(link = "inverse"), data = rtu.data)
summary(m1)

m2 <- glmer.nb(total.flowers ~ flowers.shrub+ flowers.pot + offset(log(dec.Length)) +(1|repID), data = flowering)

flowering <- filter(byrep, flowering == "bloom")
summary(m2)
