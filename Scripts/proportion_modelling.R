#observation level modelling

library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
library(sjPlot)
library(jtools)
source(system.file("utils", "allFit.R", package="lme4"))
library(glmmTMB)
library(tidyr)
library(MASS)

byobs <- read.csv("byobs_cleaned.csv")

#observation level rtu data
rtu.key <- read.csv("Clean Data/video_rtu_key.csv")
rtu.data <- left_join(rtu.key, byobs, by = "highest.rtu")
rtu.data <- dplyr::select(rtu.data, rtu.ag, plant.id, microsite, flowering, video.date, video.length, flower.fov, total.time, flowers.visits, unique.fl.visited, uniID, dec.total.time)                    
rtu.data$repID <- paste(rtu.data$plant.id, rtu.data$microsite)
rtu.data$microsite <-as.factor(rtu.data$microsite)
rtu.data$microsite <- relevel(rtu.data$microsite, "open")
rtu.data$flowering <- relevel(rtu.data$flowering, "pre")

#count.rtu.fl <- rtu.data %>% group_by(uniID, rtu.ag) %>% summarise(total.flowers = sum(flowers.visits), total.visits = n()) 



ggplot(rtu.data, aes(microsite, dec.total.time)) + geom_boxplot() + facet_grid(~flowering)
ggplot(rtu.data, aes(flowering, dec.total.time)) + geom_boxplot() + facet_grid(~microsite)

ggplot(rtu.data, aes(dec.total.time)) + geom_density()

l1 <- lmer((dec.total.time)^(1/3) ~ flowering + microsite + (1|repID), data = rtu.data)
shapiro.test(resid(l1))

AIC(l1, m1)

m1 <- glmer(dec.total.time ~ flowering + microsite + flower.fov + (1|repID), family = Gamma(link = "log"), data = rtu.data)

#m1 <- glmer(dec.total.time ~ flowering * microsite + flower.fov + (1|repID), family = Gamma(link = "log"), data = rtu.data)

car::Anova(m1, type =2)

summary(m1)
shapiro.test(resid(m1))
plot(resid(m1))
plot(resid(m1)~predict(m1))


m2 <- glmer(dec.total.time ~  (1|repID), family = Gamma(link = "log"), data = rtu.data)
anova(m1, m2)


t1 <- glmmTMB(dec.total.time ~ flowering * rtu.ag  + (1|repID), family = Gamma(link = "log"), data = rtu.data)
summary(t1)
fixef(t1)

ggplot(rtu.data, aes(microsite, dec.total.time, fill = rtu.ag)) + geom_boxplot() + facet_grid(~flowering)


bee <- filter(rtu.data, rtu.ag == "bee")
l1 <- lmer(log(dec.total.time) ~  flowering + (1|repID), data = bee)
shapiro.test(resid(l1))
plot(resid(l1)~predict(l1))
qq(resid(l1))

l2 <- lmer(log(dec.total.time) ~   (1|repID), data = bee)

anova(l1, l2)

car::Anova(l1, type = 2)
summary(l1)

shapiro.test(bee$dec.total.time)
ggplot(bee, aes(dec.total.time)) + geom_density()
summary(m4)

other <- filter(rtu.data, rtu.ag == "other")
l1 <- lmer(log(dec.total.time) ~  flowering + (1|repID), data = other)
l2 <- lmer(log(dec.total.time) ~  (1|repID), data = other)
shapiro.test(resid(l1))
plot(resid(l1)~predict(l1))
qq(resid(l1))

summary(l1)
car::Anova(l1)

anova(l1, l2)






rtu.data <- mutate(rtu.data, prop.visited = unique.fl.visited/flower.fov)

ggplot(rtu.data, aes(prop.visited, color = flowering)) + geom_density()

m1 <- glmer(prop.visited ~ flowering * microsite + (1|repID), family = Gamma(link = "log"), data = rtu.data)
summary(m1)
shapiro.test(resid(m1))
car::Anova(m1, type = 2)

fm1 <- m1

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

fm1.restart <- update(fm1, start=pars)
all <- allFit(fm1.restart)
summary(all)

m1 <- all$bobyqa

car::Anova(m1, type = 3)

summary(m1)

lsmeans(m1, pairwise~flowering*microsite)

shapiro.test(rtu.data$prop.visited)


m1 <- glmer(prop.visited ~ flowering * rtu.ag + (1|repID), family = Gamma(link = "log"), data = rtu.data)

fm1 <- m1

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

fm1.restart <- update(fm1, start=pars)
all <- allFit(fm1.restart)
summary(all)

m2 <- all$bobyqa
summary(m2)
lsmeans(m2, pairwise~flowering*rtu.ag)

m2null <- glmer(prop.visited ~  (1|repID), family = Gamma(link = "log"), data = rtu.data)

anova(m2, m2null)
ggplot(rtu.data, aes(microsite, prop.visited)) + geom_boxplot() + facet_grid(~flowering)


#now RTU microsite

m3 <- glmer(prop.visited ~ microsite * rtu.ag + (1|repID), family = Gamma(link = "log"), data = rtu.data)

fm1 <- m3

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

fm1.restart <- update(fm1, start=pars)
all <- allFit(fm1.restart)
summary(all)


m3 <- all$bobyqa
summary(m3)
lsmeans(m3, pairwise~microsite*rtu.ag)


anova(m3, m2null)








m1 <- glmmTMB(prop.visited ~ microsite * flowering* + (1|repID), ziformula=~0,family="Gamma", data = rtu.data)


bee <- filter(rtu.data, rtu.ag == "bee")
shapiro.test(bee$prop.visited)


l1 <- lmer(log(prop.visited) ~  microsite *flowering + (1|repID), data = bee)
shapiro.test(resid(l1))
plot(resid(l1)~predict(l1))
qqnorm(resid(l1))
summary(l1)

lsmeans(l1, pairwise~flowering+microsite)


l2 <- lmer(log(dec.total.time) ~   (1|repID), data = bee)

anova(l1, l2)



ggplot(rtu.data, aes(microsite, prop.visited, fill = rtu.ag)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + scale_fill_brewer(palette= "Spectral") + theme_Publication() + xlab("Microsite") + ylab("Proportion of flowers visited") + labs(fill="") + theme(legend.text = element_text(size = 16)) + scale_fill_manual("", values = c("bee" ="#D53E4F", "bombylid"= "#FC8D59", "honeybee"= "#FEE08B", "lep"= "#D3D3D3", "other"= "#99D594", "syrphid" = "#3288BD"), labels = c("Solitary Bee", "Bombyliidae", "Honeybee", "Lepidoptera", "Other", "Syrphidae"))





filter(rtu.data, dec.total.time<1.5) %>% ggplot(aes(microsite, dec.total.time, fill = rtu.ag)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + scale_fill_brewer(palette= "Spectral") + theme_Publication() + xlab("Microsite") + ylab("Duration of visit (decimal time)") + labs(fill="") + theme(legend.text = element_text(size = 16)) + scale_fill_manual("", values = c("bee" ="#D53E4F", "bombylid"= "#FC8D59", "honeybee"= "#FEE08B", "lep"= "#D3D3D3", "other"= "#99D594", "syrphid" = "#3288BD"), labels = c("Solitary Bee", "Bombyliidae", "Honeybee", "Lepidoptera", "Other", "Syrphidae"))





ggplot(rtu.data, aes(flowering, dec.total.time, fill = rtu.ag)) + geom_boxplot()