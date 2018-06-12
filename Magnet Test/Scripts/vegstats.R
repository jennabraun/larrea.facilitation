#stats for veg
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(MASS)
library(sjPlot)
library(multcomp)
??multcomp

se <- function(x) sd(x)/sqrt(length(x)) ## SE
visits <- read.csv("byrep_cleaned.csv")
metadata <- read.csv("F:/School/Chapter 2/MojaveInsectDiversity/Data/outputmetadata.csv")

#only care about the veg
visits <- dplyr::select(visits, uniID, treatment, flowering, het.shrub.blooming.neighbours, het.cactus.blooming.neighbours, understory.richness, het.annual.floral.density, PlantID)
visits$flowering <- relevel(visits$flowering, "pre")
metadata$blooming <- relevel(metadata$blooming, "pre")

metadata <- dplyr::select(metadata, uniID, plant.id, treatment, blooming, percent.cover, date)
metadata$repID <- paste(metadata$plant.id, metadata$treatment)
visits$repID <- paste(visits$PlantID, visits$treatment)
shapiro.test(metadata$percent.cover)
mean(metadata$percent.cover)
sd(metadata$percent.cover)




?glmer.nb
g1.nb <- glmer.nb(percent.cover ~ treatment + blooming + (1|repID), data = metadata)
g1.nb1 <- glmer.nb(percent.cover ~ treatment * blooming + (1|repID), data = metadata)
g1.nb.null <- glmer.nb(percent.cover ~ 1 + (1|repID), data = metadata)


summary(g1.nb)
summary(g1.nb1)
summary(g1.nb.null)

anova(g1.nb, g1.nb1, g1.nb.null)
car::Anova(g1.nb, type = 2)
car::Anova(g1.nb1, type = 3)

g1.nb2 <- glmer.nb(percent.cover ~ treatment + blooming + (1|repID) + (1|date), data = metadata)
g1.nb3 <- glmer.nb(percent.cover ~ blooming + (1|repID) + (1|date), data = metadata)
g1.nb.null <- glmer.nb(percent.cover ~ 1 + (1|repID), data = metadata)

sjt.glmer(g1.nb1)

ph <- glht(g1.nb1)
summary(ph)

ggplot(metadata, aes(treatment, percent.cover)) + geom_boxplot() + facet_grid(~blooming)



overdisp_fun(g1.nb.null)


summary(g1.nb)
summary(g1.nb1)
summary(g1.nb.null)
summary(g1.nb2)

shapiro.test(visits$understory.richness)
ggplot(visits, aes(understory.richness)) + geom_density(kernel = "gaussian")

plot(g1.nb2)
anova(g1.nb, g1.nb.null, g1.nb1, g1.nb2, g1.nb3, test = "Chisq")

anova(g1, g1.nb, test = "Chisq")
getME(g1.nb, "glmer.nb.theta")

plot(g1.nb)
plot(g1.nb.null)
plot(g1.q)
plot(metadata$percent.cover)

plot(fitted(g1.nb), residuals(g1.nb), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(g1.nb), residuals(g1.nb)))

#richness

mean(visits$understory.richness, na.rm = TRUE)
sd(visits$understory.richness, na.rm = TRUE)
g2 <- glmer(understory.richness ~ treatment + flowering + (1|PlantID), family = poisson(link="log"), data = visits)
g2.2 <- glmer(understory.richness ~ treatment * flowering + (1|PlantID), family = poisson(link="log"), data = visits)
g2.null <- glmer(understory.richness ~ 1 + (1|PlantID), family = poisson(link="log"), data = visits)
summary(g2.null)
summary(g2)
summary(g2.2)
anova(g2, g2.null, g2.2, test = "Chisq")
car::Anova(g2, type = 2)

#floral density
mean(visits$het.annual.floral.density, na.rm = TRUE)
sd(visits$het.annual.floral.density, na.rm = TRUE)

ggplot(visits, aes(het.annual.floral.density)) + geom_density(kernel = "gaussian")

g3.nb <- glmer.nb(het.annual.floral.density ~ flowering * treatment + (1|repID), data = visits)
g3.nb.1 <- glmer.nb(het.annual.floral.density ~ flowering + treatment + (1|repID), data = visits)
g3.nb.null <- glmer.nb(het.annual.floral.density ~ 1 + (1|repID), data = visits)
summary(g3.nb)
summary(g3.nb.null)
anova(g3.nb, g3.nb.null, g3.nb.1, test = "Chisq")
car::Anova(g3.nb.1, type = 2)

test <- glmer(percent.cover ~ treatment + (1|repID), family = poisson(link ="log"), data = metadata)

g2 <- glmer(percent.cover ~ treatment + blooming + (1|repID), family = poisson(link="log"), data = metadata)








overdisp_fun(test)

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

