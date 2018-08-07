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



mean(byrep$dec.Length)

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
##no errors
#all <- allFit(fm1.restart)
summary(fm1.restart)
#3 models converged
AIC(fm1, fm1.restart)




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

#lsmeans(fm1.restart, pairwise~treatment*flowering)
AIC(m1, m2)
#extract the fit that works so can use AIC WOOHOO!
m2 <- all$bobyqa

summary(m2)


m1.1 <- glmer.nb(total.flowers ~ treatment  + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m1.1

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

m1.1 <- fm1.restart

m1.2 <- glmer.nb(total.flowers ~ flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m1.2
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

summary(fm1.restart)
all <- allFit(fm1.restart)
summary(all)

m1.2 <- all$bobyqa


null <- glmer.nb(total.flowers ~ flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- null

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





m1.1 <- fm1.restart

m1.2 <- glmer.nb(total.flowers ~ flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m1.2
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


all <- allFit(fm1.restart)
summary(all)
null <- all$bobyqa




anova(null, m1,m2)
AIC(m1,m2, m1.1, m1.2)
summary(m2)

car::Anova(m2, type = 3)


## modeling total visits

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
m3 <- m3.restart

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

m5 <- m5.restart

m6 <- glmer.nb(total.visits ~ treatment + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)



m7 <- glmer.nb(total.visits ~ flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- m7 
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

m7.restart <- update(fm1, start=pars)

all <- allFit(m7.restart)
summary(all)
#go nelder_mead!!
m7 <- all$Nelder_Mead



null <- glmer.nb(total.visits ~ flowers.pot + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- null 
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

m7.restart <- update(fm1, start=pars)
null <- m7.restart


anova(m3,m5, null)

anova(m7, m3)


summary(m5)
car::Anova(m5, type = 3)





#date tests
ggplot(byrtu, aes(rtu, total.visits, fill = Date, color= flowering)) + geom_bar(stat = "identity", position = "dodge")

m1 <- glmmTMB(total.flowers ~ flowering  + flowers.pot + treatment + offset(log(dec.Length)) + (1|repID), ziformula=~0,family="nbinom2", data = byrtu)      

m2 <- glmmTMB(total.flowers ~ flowering * rtu + treatment + flowers.pot + offset(log(dec.Length)) + (1|repID), ziformula=~0,family="poisson", data = byrtu) 



AIC(m1,m2)
summary(m1)
summary(m2)
car::Anova(m1, m2)
anova(m1,m2)
lsmeans(m2, pairwise~rtu*flowering)

ggplot(byrep, aes(treatment, flowers.per.hour)) + geom_boxplot()
ggplot(byrep, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~treatment)





#means with error bars


dfwc_between <- summarySE(data=byrep, measurevar="flowers.per.hour", groupvars="treatment", na.rm=FALSE, conf.interval=.95)
dfwc_between


ggplot(dfwc_between, aes(x=treatment, y=flowers.per.hour, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=flowers.per.hour-ci, ymax=flowers.per.hour+ci), colour="red") +
  geom_errorbar(width=.1, aes(ymin=flowers.per.hour-ci, ymax=flowers.per.hour+ci), dfwc_between) +
  geom_point(shape=21, size=3, fill="white") 

  ylim(ymin,ymax)

library(plyr)
library(dplyr)

 
  
#within subject
datac <- summarySEwithin(byrep, measurevar="flowers.per.hour", withinvars=c("flowering"),betweenvars = "treatment", idvar="repID")
datac


# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dfwc_between <- summarySE(data=dfw_long, measurevar="value", groupvars="condition", na.rm=FALSE, conf.interval=.95)
dfwc_between


# Show the between-S CI's in red, and the within-S CI's in black
ggplot(dfwc_between, aes(x=condition, y=value, group=1)) +
  geom_line() +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), colour="red") +
  geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), data=dfwc) +
  geom_point(shape=21, size=3, fill="white") +
  ylim(ymin,ymax)


ggplot(dfw_long, aes(x=condition, y=value, colour=subject, group=subject)) +
  geom_line() + geom_point(shape=21, fill="white") + 
  ylim(ymin,ymax)


tgc <- summarySE(byrep, measurevar="flowers.per.hour", groupvars=c("treatment","flowering"))
tgc

ggplot(tgc, aes(x=flowering, y=flowers.per.hour, colour=treatment)) + 
  geom_errorbar(aes(ymin=flowers.per.hour-se, ymax=+se), width=.1) +
  geom_line() +
  geom_point() + theme_Publication() + ylab("Total flower visits per hour") + xlab("Bloom Period") +guides(color=guide_legend("Microsite:")) +
  
  
  
  
  
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))




guides(color=guide_legend("Microsite:".values = c("open" = black, "shrub" = red)))

plot <- plot_model(fm1.restart)
plot + xlab("Predictor") + ggtitle("Reponse:Total flower visits")

m2 <- glmmTMB(total.flowers ~ treatment + rtu * flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), ziformula=~0,family="nbinom2", data = byrtu) 
summary(m2)
plot_model(m2)

shapiro.test(byrep$flowers.pot)
ggplot(byrep, aes(flowers.pot)) + geom_density()
kruskal.test(byrep$flowers.pot ~ byrep$flowering)




#testing importance of covariates
base <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(base)

c1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + understory.richness+ offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c1)
c1.1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + understory.richness+ offset(log(dec.Length)) + (1|repID), family = "nbinom1", data = byrep)
AIC(c1, c1.1, base)


summary(c1.1)


c2 <- glmer.nb(total.flowers ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), data = byrep)

fm1 <- c2
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

m7.restart <- update(fm1, start=pars)
all <- allFit(m7.restart)

summary(all)













c2 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = nbinom2(link = "log"), data = byrep)
summary(c2)
car::Anova(c2, Type = 2)
anova(c2, c2)
anova(c2)

c2.0 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot  + offset(log(dec.Length)) + (1|repID), family = nbinom2(link = "log"), data = byrep)
anova(c2, c2.0)

c2.v <- glmmTMB(total.visits ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = nbinom2(link = "log"), data = byrep)
summary(c2.v)

library(MASS)


glmmPQL(total.visits~flowering + treatment + flowers.pot + het.annual.floral.density + offset(log(dec.Length)), random = ~1|repID, family="neg.binomial", data = byrep)





c2.1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = "nbinom1", data = byrep)
AIC(c2, c2.1, base)
summary(c2)

c3 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.shrub.blooming.neighbours + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c3)

c2.v <- glmmTMB(total.visits ~ treatment + flowering + flowers.pot + het.shrub.blooming.neighbours + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c2.v)
