#pollination facilitation models and stats

library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
library(sjPlot)
library(jtools)
source(system.file("utils", "allFit.R", package="lme4"))
library(glmmTMB)
library(tidyr)
#source("Scripts/videowrangling.R")
byobs <- read.csv("byobs_cleaned.csv")
byrep <- read.csv("byrep_cleaned.csv")
byrtu <- read.csv("rtu_by_rep.csv")

byrep$repID <- paste(byrep$PlantID, byrep$treatment)
byrep$treatment <- relevel(byrep$treatment, "open")
byrep$flowering <- relevel(byrep$flowering, "pre")

byrtu$repID <- paste(byrtu$PlantID, byrep$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")


sum(byrep$total.flowers)
sum(byrep$total.visits)
#base hypothesis testing model
fm1 <- glmer.nb(total.flowers ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

#check model problems
diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
any(diag.vals < 1e-6) # FALSE

#recompute Hessian
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

#restart model from different point
fm1.restart <- update(fm1, start=pars)
#no errors
all <- allFit(fm1.restart)
summary(fm1.restart)
#3 models converged
AIC(fm1, fm1.restart)

#figure
plot_model(fm1.restart, type = "pred", terms = c("flowering", "treatment"))


cat_plot(fm1.restart, "flowering", "treatment")

lsmeans(fm1.restart, pairwise~treatment|flowering)


summary(fm1.restart)
shapiro.test(residuals(fm1.restart))
#not too unnormal - also NBD for negative binomial
overdisp_fun(fm1.restart)
#not overdispersed
plot(residuals(fm1.restart)~predict(fm1.restart))
car::Anova(fm1.restart, type = 2)
m1 <- fm1.restart

m2 <- glmer.nb(total.flowers ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)
fm1 <- m2

#recomputing
diag.vals <- getME(fm1,"theta")[getME(fm1,"lower") == 0]
any(diag.vals < 1e-6) # FALSE

#recompute Hessian
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


#restart model from different point
fm1.restart <- update(fm1, start=pars)

#still issues
all <- allFit(fm1.restart)
summary(all)
#3 models converged
#bobyqa is the real MVP

m2 <- all
summary(m2)
summary(fm1.restart)
#estimates are the same as the all fit output

m3 <- glmer.nb(total.visits ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m3

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


m3.restart <- update(fm1, start=pars)
#all <- allFit(m3.restart)
#summary(all)

summary(m3.restart)
car::Anova(m3.restart, type = 2)
overdisp_fun(m3.restart)
shapiro.test(residuals(m3.restart))

m5 <- glmer.nb(total.visits ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m5

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

m5.restart <- update(fm1, start=pars)
#all <- allFit(m3.restart)
#summary(all)

summary(m5.restart)



#rtu stats
byrtu$repID <- paste(byrtu$PlantID, byrtu$treatment)
byrtu$treatment <- relevel(byrtu$treatment, "open")
byrtu$flowering <- relevel(byrtu$flowering, "pre")


m4 <- glmer.nb(total.flowers ~ treatment * rtu + flowering + flowers.pot +  offset(log(dec.Length)) + (1|repID), data = byrtu)
m4.restart <- update(m4, start=3)

summary(m4.restart)

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

ggplot(rtu.data, aes(dec.total.time)) + geom_density()

rtu.data$repID <- paste(rtu.data$plant.id, rtu.data$microsite)
rtu.data$microsite <-as.factor(rtu.data$microsite)
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

ggplot(rtu.data, aes(dec.total.time, color = flowering)) + geom_density()

m1 <- glmer(dec.total.time ~ flowering + (1|repID), family = Gamma(link = "log"), data = rtu.data)
summary(m1)
shapiro.test(resid(m1))
car::Anova(m1, type = 2)


m1 <- glmmTMB(dec.total.time ~ microsite + flowering + (1|repID), ziformula=~0,family="Gamma", data = rtu.data)







filter(rtu.data, dec.total.time<1.5) %>% ggplot(aes(microsite, dec.total.time, fill = rtu.ag)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + scale_fill_brewer(palette= "Spectral") + theme_Publication() + xlab("Microsite") + ylab("Duration of visit (decimal time)") + labs(fill="") + theme(legend.text = element_text(size = 16))

ggplot(rtu.data, aes(flowering, dec.total.time, fill = rtu.ag)) + geom_boxplot()

                                                                
bees <-filter(byrtu, rtu == "bee" | rtu == "honeybee")  
bees <- select(bees, uniID, everything())
bees <- select(bees, -total.visits, -visits.per.hour, -flowers.per.hour, -X)
bees <- spread(bees,rtu, total.flowers)
bees <- mutate(bees, total.flowers = bee + honeybee) %>% select(-bee, -honeybee)                                                                
                                                                
                                                                
                                                                                                                                
bees %>% group_by(., treatment, flowering) %>% summarise(n = mean(total.flowers))                                                                
                                                                
sum(bees$total.flowers)  
sum(bees$total.visits)

m2 <- glmmTMB(total.flowers ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), ziformula=~0,family="poisson", data = bees)                                                                
summary(m2)                                                               

m2 <- glmer(total.flowers ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), family="poisson"(link="log"), data = bees)

overdisp_fun(m2)
summary(m2)
shapiro.test(resid(m2))
cat_plot(m2, pred = flowering, modx = treatment)
ls <- lsmeans(m2, pairwise~flowering|treatment)
summary(lsm, type = "response")
lsm <- lsmeans(m2, pairwise~treatment*flowering)
summary(lsm)
plot(ls, by = "factor2", intervals = TRUE, type = "response")
lsmip(lsm, treatment ~ flowering, type = "response")

car::Anova(m2, type = 3)


m2 <- glmer.nb(total.flowers ~ treatment * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = bees)
summary(m2)
#recompute Hessian
fm1 <- m2
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
fm1.restart <- update(fm1, start=pars)
summary(fm1.restart)
all <- allFit(fm1.restart)
summary(all)


#date tests
ggplot(byrtu, aes(rtu, total.visits, fill = Date, color= flowering)) + geom_bar(stat = "identity", position = "dodge")

#s1 <- glmmTMB(total.flowers ~ flowering +  flowers.shrub + flowers.pot + treatment + offset(log(dec.Length)) + (1|repID), ziformula=~0,family="nbinom2", data = byrep)      
#summary(s1)
