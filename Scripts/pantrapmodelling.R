#models for pan trap data (abundance, diversity)
library(dplyr)
library(lme4)
library(ggplot2)
library(lsmeans)
library(vegan)
source(system.file("utils", "allFit.R", package="lme4"))

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
lsmeans(m1, pairwise~blooming|treatment)
car::Anova(m1, type = 3)

cat_plot(m1, pred = treatment, modx = blooming, plot.points = TRUE)
summ(m1)
plot_summs(m1)
effect_plot(m1, pred = treatment)


ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + facet_grid(~blooming)

m2 <- glmer.nb(abun ~ treatment + blooming + (1|repID), data = nobeetle)
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
m2.restart <- update(fm1, start=pars)
all <- allFit(m2.restart)
ss <- summary(all)
ss$which.OK

m3 <- glmer.nb(abun ~ (1|repID), data = metadata)
anova(m1, m2, m3, test = "Chisq")
AIC(m1, m2, m3)


m4 <- glmer.nb(abun ~  treatment + blooming + (1|repID), data = metadata)
summary(m4)


#with beetles
beetle <- read.csv("Clean Data/metadata_yesbeetle.csv")
beetle$repID <- paste(beetle$plant.id, beetle$treatment)
beetle$blooming <- relevel(beetle$blooming, "pre")
sum(beetle$abun)
str(beetle)

ggplot(data = beetle, aes(abun)) + geom_freqpoly()
mean(beetle$abun)
sd(beetle$abun)

m1 <- glmer.nb(abun ~ blooming + treatment + (1|repID), data = beetle)
summary(m1)
car::Anova(m1, type = 2)
cat_plot(m1, pred = blooming, modx = treatment)


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
summary(m1)
car::Anova(m1, type = 3)

m2 <- glmer(abun ~ treatment + blooming + (1|repID), family = poisson(link="log"), data = onlybeetle)

m3 <- glmer(abun ~ treatment * blooming + (1|repID), family = poisson(link="log"), data = onlybeetle)

m4 <- glmer.nb(abun ~ blooming + treatment + (1|repID), data = onlybeetle)
AIC(m1, m3)
# cant do this not nested anova(m1, m2, m3, m4, test = "Chisq")

AIC(m1, m2)
car::Anova(m1, type = 3)
car::Anova(m2, type = 2)
AIC(m1)
anova(m1)
overdisp_fun(m3)

ggplot(metadata, aes(blooming, abun)) + geom_boxplot() + facet_grid(~treatment)

#rescale percent cover
metadata$cover <- metadata$percent.cover/100

#Species Richness
#all insects

shapiro.test(beetle$Species)
#it's so close to normal I'm going to try linear models

m1 <- lmer(Species ~ treatment + blooming + (1|repID), data = beetle)
car::Anova(m1, type = 2)
summary(m1)
plot(m1)
shapiro.test(residuals(m1))
#linear probably ok
cat_plot(m1, pred = treatment, modx = blooming)
plot(resid(m1))

m2 <- lmer(Species ~ treatment * blooming + (1|repID), data = beetle)
summary(m2)
car::Anova(m2, type = 3)
ggplot(beetle, aes(blooming, Species)) + geom_boxplot() + facet_grid(~treatment)
ggplot(beetle, aes(treatment, Species)) + geom_boxplot() + facet_grid(~blooming)



ggplot(data.frame(eta=predict(m1,type="link"),pearson=residuals(m1,type="pearson")),
      aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

ggplot(data.frame(x1=beetle$blooming,pearson=residuals(m1,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()
fixef(m1)
lmcoefs[1:3]
qqnorm(residuals(m1))


#species accumulation
insects <- read.csv("Clean Data/wide_yesbeetle.csv")
row.names(insects) <- insects$X
insects <- select(insects, -X)
plot(specaccum(insects), xlab = "# of samples", ylab = "# of species")

metadata$date <- as.ordered(metadata$date)

shapiro.test(metadata$H)
shapiro.test(metadata$logH)
metadata$logH <- log(metadata$H)
metadata$sqrH <- sqrt(metadata$H)

shapiro.test(metadata$logH)
shapiro.test(metadata$sqrH)

ggplot(metadata, aes(sqrH)) + geom_density()
       
       
#m4 <- glmer.nb(H ~ blooming + treatment + (1|repID), data = beetle)
#not sure what distribution to use
ggplot(beetle, aes(H)) + geom_density()
ggplot(beetle, aes(treatment, Species)) + geom_boxplot() + facet_grid(~blooming)
ggplot(beetle, aes(treatment, Even)) + geom_boxplot()
ggplot(beetle, aes(treatment, Simpson)) + geom_boxplot()
summary(m4)


simulate(m1)



#syrphids
pans <- read.csv("pantraps_long.csv")
syr <- filter(pans, highest.rtu == "Scaeva/Eupeodes")
ggplot(syr, aes(Quantity)) + geom_bar(stat = "identity", position = "dodge")




