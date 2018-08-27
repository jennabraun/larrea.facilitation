#pollination facilitation models and stats

library(dplyr)
library(ggplot2)
library(lme4)
library(lsmeans)
source(system.file("utils", "allFit.R", package="lme4"))
library(glmmTMB)
library(tidyr)
#source("Scripts/videowrangling.R")

byrep <- read.csv("byrep_cleaned.csv")
byrep$repID <- paste(byrep$PlantID, byrep$treatment)
byrep$treatment <- relevel(byrep$treatment, "open")
byrep$flowering <- relevel(byrep$flowering, "pre")



mean(byrep$dec.Length)
#should be 925 and 697 - not 930 and 701.

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

#interactive model

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

all.null <- allFit(fm1.restart)
summary(all.null)
null <- all.null$bobyqa



anova(null, m1,m2)
AIC(m1,m2, null)
summary(m2)
car::Anova(m2, type = 3)


## modeling total visits

#base additive model
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

#main hypothesis interactive model
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

#null model
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

null.restart <- update(fm1, start=pars)
null <- null.restart


anova(m3,m5, null)
AIC(m3, m5, null)
car::Anova(m3, type = 2)





ggplot(byrep, aes(treatment, flowers.per.hour)) + geom_boxplot()
ggplot(byrep, aes(flowering, flowers.per.hour)) + geom_boxplot() + facet_grid(~treatment)



#testing importance of covariates
#using TMB because glmer will not coverge. 
base <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(base)

c1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + understory.richness+ offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c1)
c1.1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + understory.richness+ offset(log(dec.Length)) + (1|repID), family = "nbinom1", data = byrep)
summary(c1.1)
AIC(c1, c1.1)

#nbinom2 is better

c2 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = nbinom2(link = "log"), data = byrep)
summary(c2)



c2.1 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = "nbinom1", data = byrep)
AIC(c2, c2.1)
summary(c2)


c2.v <- glmmTMB(total.visits ~ treatment + flowering + flowers.pot + het.annual.floral.density + offset(log(dec.Length)) + (1|repID), family = nbinom2(link = "log"), data = byrep)
summary(c2.v)


c3 <- glmmTMB(total.flowers ~ treatment + flowering + flowers.pot + het.shrub.blooming.neighbours + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c3)

c2.v <- glmmTMB(total.visits ~ treatment + flowering + flowers.pot + het.shrub.blooming.neighbours + offset(log(dec.Length)) + (1|repID), family = "nbinom2", data = byrep)
summary(c2.v)
