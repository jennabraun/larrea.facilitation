#stats for veg
#glmms for vegation covariates: percent.cover, annual richness, annual bloom density, blooming shrub and cactus density 

library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(sjPlot)
library(lsmeans)
library(devfun)

se <- function(x) sd(x)/sqrt(length(x)) ## SE
source(system.file("utils", "allFit.R", package="lme4"))
visits <- read.csv("byrep_cleaned.csv")
metadata <- read.csv("Clean Data/metadata_nobeetle.csv")

#only care about the veg
visits <- dplyr::select(visits, uniID, treatment, flowering, het.shrub.blooming.neighbours, het.cactus.blooming.neighbours, understory.richness, het.annual.floral.density, PlantID)
visits$flowering <- relevel(visits$flowering, "pre")
metadata$blooming <- relevel(metadata$blooming, "pre")

metadata <- dplyr::select(metadata, uniID, plant.id, treatment, blooming, percent.cover, date)
metadata$repID <- paste(metadata$plant.id, metadata$treatment)
visits$repID <- paste(visits$PlantID, visits$treatment)


#percent.cover

shapiro.test(metadata$percent.cover)
mean(metadata$percent.cover)
sd(metadata$percent.cover)
ggplot(metadata, aes(percent.cover)) + geom_density()

g1.nb <- glmer.nb(percent.cover ~ treatment + blooming + (1|repID), data = metadata)
g1.nb1 <- glmer.nb(percent.cover ~ treatment * blooming + (1|repID), data = metadata)
g1.nb.null <- glmer.nb(percent.cover ~ 1 + (1|repID), data = metadata)

g1.ps <- glmer(percent.cover ~ treatment * blooming + (1|repID), family = poisson, data = metadata)
summary(g1.ps)
overdisp_fun(g1.ps)
summary(g1.nb)
summary(g1.nb1)
summary(g1.nb.null)

AIC(g1.ps, g1.nb1)

anova(g1.nb, g1.nb1, g1.nb.null)
AIC(g1.nb, g1.nb1, g1.nb.null)
car::Anova(g1.nb, type = 2)
car::Anova(g1.nb1, type = 3)

shapiro.test(residuals(g1.nb1))
overdisp_fun(g1.nb1)

sjt.glmer(g1.nb1)

ph <- lsmeans(g1.nb1, pairwise~treatment*blooming, adjust="tukey")
summary(ph)

ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + facet_grid(~blooming)

plot(fitted(g1.nb1), residuals(g1.nb1), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(g1.nb1), residuals(g1.nb1)))

ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + facet_grid(~blooming, labeller=labeller(blooming = labels)) + theme_Publication() + xlab("Microsite") + ylab("Percent Annual Cover")


#richness

mean(visits$understory.richness, na.rm = TRUE)
sd(visits$understory.richness, na.rm = TRUE)

shapiro.test(visits$understory.richness)
ggplot(visits, aes(understory.richness)) + geom_density()

g2 <- glmer(understory.richness ~ treatment + flowering + (1|repID), family = poisson(link="log"), data = visits)
g2.2 <- glmer(understory.richness ~ treatment * flowering + (1|repID), family = poisson(link="log"), data = visits)
g2.null <- glmer(understory.richness ~ 1 + (1|repID), family = poisson(link="log"), data = visits)

summary(g2.null)
summary(g2)
summary(g2.2)
anova(g2, g2.null, g2.2, test = "Chisq")
AIC(g2, g2.null, g2.2)
car::Anova(g2, type = 2)

shapiro.test(residuals(g2))
overdisp_fun(g2)

sjt.glmer(g2)

ggplot(visits, aes(treatment, understory.richness)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + theme_Publication() + xlab("Microsite") + ylab("Annual Species Richness")




plot(fitted(g2), residuals(g2), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(g2), residuals(g2)))

#floral density
mean(visits$het.annual.floral.density, na.rm = TRUE)
sd(visits$het.annual.floral.density, na.rm = TRUE)

ggplot(visits, aes(het.annual.floral.density)) + geom_density()

g3.nb <- glmer.nb(het.annual.floral.density ~ flowering * treatment + (1|repID), data = visits)
#model breaks. Recompute gradient & Hessian
fm1 <- g3.nb
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
fm1.restart <- update(fm1, start=pars)
#restart
summary(fm1.restart)
all <- allFit(fm1.restart)
summary(all)
g3.nb <- fm1.restart

g3.nb.1 <- glmer.nb(het.annual.floral.density ~ flowering + treatment + (1|repID), data = visits)
g3.nb.null <- glmer.nb(het.annual.floral.density ~ 1 + (1|repID), data = visits)
summary(g3.nb)
summary(g3.nb.1)
summary(g3.nb.null)
anova(g3.nb, g3.nb.null, g3.nb.1, test = "Chisq")
car::Anova(g3.nb.1, type = 2)
AIC(g3.nb, g3.nb.1, g3.nb.null)

shapiro.test(residuals(g3.nb.1))
overdisp_fun(g3.nb.1)
sjt.glmer(g3.nb.1)

ggplot(visits, aes(treatment, het.annual.floral.density)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + theme_Publication() + xlab("Microsite") + ylab("Heterospecific Annual Floral Density")




#blooming shrubs
mean(visits$het.shrub.blooming.neighbours, na.rm = TRUE)
sd(visits$het.shrub.blooming.neighbours, na.rm = TRUE)

ggplot(visits, aes(het.shrub.blooming.neighbours)) + geom_density(kernel = "gaussian")

g4.nb <- glmer.nb(het.shrub.blooming.neighbours ~ flowering * treatment + (1|repID), data = visits)

#model breaks. Recompute gradient & Hessian
fm1 <- g4.nb

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

#restart model from optimum
fm1.restart <- update(fm1, start=pars)
summary(fm1.restart)

#test all optimizers
all.fm1 <- allFit(fm1.restart)
summary(all.fm1)
#most optimizer run on model yielding same coefficients

g4.nb.1 <- glmer.nb(het.shrub.blooming.neighbours ~ flowering + treatment + (1|repID), data = visits)
g4.nb.null <- glmer.nb(het.shrub.blooming.neighbours ~ 1 + (1|repID), data = visits)
summary(g4.nb)
summary(g4.nb.1)
summary(g4.nb.null)
anova(g4.nb, g4.nb.null, g4.nb.1, test = "Chisq")
car::Anova(g4.nb.1, type = 2)
AIC(g4.nb, g4.nb.1, g4.nb.null)

shapiro.test(residuals(g4.nb.1))
overdisp_fun(g4.nb.1)
sjt.glmer(g4.nb.1)

#blooming cactus
mean(visits$het.cactus.blooming.neighbours, na.rm = TRUE)
sd(visits$het.cactus.blooming.neighbours, na.rm = TRUE)

ggplot(visits, aes(het.cactus.blooming.neighbours)) + geom_density(kernel = "gaussian")

g5.nb <- glmer.nb(het.cactus.blooming.neighbours ~ flowering * treatment + (1|repID), data = visits)

fm1 <- g5.nb

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

#restart model from optimum
fm1.restart <- update(fm1, start=pars)
summary(fm1.restart)

#test all optimizers
all.fm1 <- allFit(fm1.restart)
summary(all.fm1)

#3 optimizers worked, insignificant interaction anyways
#HA ok this error was because this data is somehow not overdispersed

#g5.nb.1 <- glmer.nb(het.cactus.blooming.neighbours ~ flowering + treatment + (1|repID), data = visits)

g5.ps <- glmer(het.cactus.blooming.neighbours ~ flowering * treatment + (1|repID), family = poisson(link = "log"), data = visits)
g5.ps.1 <- glmer(het.cactus.blooming.neighbours ~ flowering + treatment + (1|repID), family = poisson(link = "log"), data = visits)
g5.ps.null <- glmer(het.cactus.blooming.neighbours ~ 1 + (1|repID), family = poisson(link ="log"), data = visits)


ggplot(visits, aes(treatment, het.shrub.blooming.neighbours)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + theme_Publication() + xlab("Microsite") + ylab("Blooming Shrub Density within 2m")

ggplot(visits, aes(treatment, het.cactus.blooming.neighbours)) + geom_boxplot() + facet_grid(~flowering, labeller=labeller(flowering = labels)) + theme_Publication() + xlab("Microsite") + ylab("Blooming Cactus Density within 2m")

summary(g5.ps)
summary(g5.ps.1)
summary(g5.ps.null)
anova(g5.ps, g5.ps.null, g5.ps.1, test = "Chisq")
car::Anova(g5.ps.1, type = 2)
AIC(g5.ps, g5.ps.1, g5.ps.null)

shapiro.test(residuals(g5.ps.1))
overdisp_fun(g5.ps.1)
sjt.glmer(g5.ps.1)



