#models for pan trap data (abundance, diversity)
library(dplyr)
library(lme4)
library(ggplot2)
library(lsmeans)
library(vegan)
source(system.file("utils", "allFit.R", package="lme4"))
source("Scripts/functions.R")

#Melyrid's excluded
metadata <- read.csv("Clean Data/metadata_nobeetle.csv")
metadata$repID <- paste(metadata$plant.id, metadata$treatment)
metadata$blooming <- relevel(metadata$blooming, "pre")
sum(metadata$abun)
str(metadata)

ggplot(data = metadata, aes(abun)) + geom_freqpoly()
mean(metadata$abun)
sd(metadata$abun)
shapiro.test(metadata$abun)
ggplot(data = metadata, aes(treatment, abun)) + geom_boxplot() + facet_grid(~blooming)

m1 <- glmer.nb(abun ~ treatment * blooming + (1|repID), data = metadata)
summary(m1)
lsmeans(m1, pairwise~blooming*treatment)
car::Anova(m1, type = 3)



ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + facet_grid(~blooming)

m2 <- glmer.nb(abun ~ treatment + blooming + (1|repID), data = metadata)
summary(m2)

m3 <- glmer.nb(abun ~ (1|repID), data = metadata)
anova(m1, m2, m3, test = "Chisq")
AIC(m1, m2)


summary(m1)

m4 <- glmer.nb(abun ~  treatment + blooming + (1|repID), data = metadata)
summary(m4)


#with beetles
beetle <- read.csv("Clean Data/metadata_yesbeetle.csv")
beetle$repID <- paste(beetle$plant.id, beetle$treatment)
beetle$blooming <- relevel(beetle$blooming, "pre")
sum(beetle$abun)
str(beetle)
sum(beetle$H)

ggplot(data = beetle, aes(abun)) + geom_freqpoly()
mean(beetle$abun)
sd(beetle$abun)

m1 <- glmer.nb(abun ~ blooming + treatment + (1|repID), data = beetle)
summary(m1)
car::Anova(m1, type = 2)
cat_plot(m1, pred = blooming, modx = treatment)

m2 <- glmer.nb(abun ~ blooming * treatment + (1|repID), data = beetle)
summary(m2)

m3 <- glmer.nb(abun ~  (1|repID), data = beetle)
summary(m3)


anova(m1, m2, m3)
AIC(m1, m2, m3)

summary(m1)
car::Anova(m1, Type = 2)
#poisson not awesome
m2 <- glmer(abun ~ treatment + blooming + (1|repID), family = poisson(link="log"), data = beetle)
anova(m1, m2, test = "Chisq")
AIC(m1, m2)






#only beetles
onlybeetle <- read.csv("Clean Data/metadata_onlybeetle.csv")
onlybeetle$repID <- paste(onlybeetle$plant.id, onlybeetle$treatment)
onlybeetle$blooming <- relevel(onlybeetle$blooming, "pre")
sum(onlybeetle$abun)
str(beetle)

ggplot(data = onlybeetle, aes(abun)) + geom_freqpoly()
mean(onlybeetle$abun)
sd(onlybeetle$abun)

m1 <- glmer.nb(abun ~ blooming * treatment + (1|repID), data = onlybeetle)

fm1 <- m1
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

m1 <- fm1.restart


summary(m1)
car::Anova(m1, type = 3)
lsmeans(m1, pairwise~blooming*treatment)



m2 <- glmer.nb(abun ~ blooming + treatment + (1|repID), data = onlybeetle)

fm1 <- m2
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

m2 <- fm1.restart
summary(m2)

m3 <- glmer.nb(abun ~ (1|repID), data = onlybeetle)

# cant do this not nested anova(m1, m2, m3, m4, test = "Chisq")

AIC(m1, m2, m3)
anova(m1, m2, m3)

car::Anova(m1, type = 3)
summary(m1)


car::Anova(m2, type = 2)
AIC(m1)
anova(m1)
overdisp_fun(m3)

ggplot(metadata, aes(blooming, abun)) + geom_boxplot() + facet_grid(~treatment)


lsmeans(m1, pairwise~blooming*treatment)


#rescale percent cover
beetle$cover <- beetle$percent.cover/100

#Species Richness
#all insects

shapiro.test(beetle$Species)
#it's so close to normal I'm going to try linear models

s1 <- glmer(Species ~ blooming + treatment + (1|repID), family = poisson, data = beetle)
#s2 <- glmer.nb(Species ~ blooming + treatment + (1|repID), data = beetle)
s2 <- glmer(Species ~ blooming * treatment + (1|repID), family = poisson, data = beetle)
s3 <- glmer(Species ~ (1|repID), family = poisson, data = beetle)
AIC(s1, s2)

anova(s1, s2, s3)

overdisp_fun(s1)

summary(s1)
car::Anova(s1, Type = 2)


ggplot(beetle, aes(blooming, Species)) + geom_boxplot() + facet_grid(~treatment)
ggplot(beetle, aes(treatment, Species)) + geom_boxplot() + facet_grid(~blooming)





#species accumulation
insects <- read.csv("Clean Data/wide_yesbeetle.csv")
row.names(insects) <- insects$X
insects <- select(insects, -X)
plot(specaccum(insects), xlab = "# of samples", ylab = "# of species")









