library(lme4)
library(ggplot2)

metadata <- read.csv("Clean Data/metadata_nobeetle.csv")
metadata$repID <- paste(metadata$plant.id, metadata$treatment)
metadata$blooming <- relevel(metadata$blooming, "pre")
sum(metadata$abun)
str(metadata)

ggplot(data = metadata, aes(abun)) + geom_freqpoly()
mean(metadata$abun)
sd(metadata$abun)

m1 <- glmer.nb(abun ~ blooming * treatment + (1|repID), data = metadata)
summary(m1)
lsmeans(m1, pairwise~treatment|blooming)
car::Anova(m1, type = 3)
ggplot(metadata, aes(treatment, abun)) + geom_boxplot() + facet_grid(~blooming)


m2 <- glmer.nb(abun ~ treatment + blooming + (1|repID), data = metadata)
anova(m1, m2, test = "Chisq")
AIC(m1, m2)


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

m2 <- glmer(abun ~ treatment + blooming + (1|repID), family = poisson(link="log"), data = beetle)
anova(m1, m2, test = "Chisq")
AIC(m1, m2)

#only beetles
onlybeetle <- read.csv("Clean Data/metadata_onlybeetle.csv")
onlybeetle$repID <- paste(onlybeetle$plant.id, onlybeetle$treatment)
onlybeetle$blooming <- relevel(onlybeetle$blooming, "pre")
sum(beetle$abun)
str(beetle)

ggplot(data = onlybeetle, aes(abun)) + geom_freqpoly()
mean(onlybeetle$abun)
sd(onlybeetle$abun)

m1 <- glmer.nb(abun ~ blooming * treatment + (1|repID), data = onlybeetle)
summary(m1)
car::Anova(m1, type = 2)

m2 <- glmer(abun ~ treatment + blooming + (1|repID), family = poisson(link="log"), data = onlybeetle)

m3 <- glmer(abun ~ treatment * blooming + (1|repID), family = poisson(link="log"), data = onlybeetle)

m4 <- glmer.nb(abun ~ blooming + treatment + (1|repID), data = onlybeetle)

anova(m1, m2, m3, m4, test = "Chisq")

AIC(m1, m2)



diag.vals <- getME(m2,"theta")[getME(m2,"lower") == 0]
any(diag.vals < 1e-6) # FALSE
fm1.restart <- update(m2, start=1)
summary(fm1.restart)
summary(m2)
overdisp_fun(fm1.restart)

library(optimx)

source(system.file("utils", "allFit.R", package="lme4"))
m2.all <- allFit(m2)
ss <- summary(m2.all)
ss$ fixef               ## extract fixed effects
ss$ llik                ## log-likelihoods
ss$ sdcor               ## SDs and correlations
ss$ theta               ## Cholesky factors
ss$ which.OK            ## which fits worked

car::Anova(m1, type = 3)
car::Anova(m2, type = 2)
AIC(m1)
anova(m1)
overdisp_fun(m2)

ggplot(metadata, aes(blooming, abun)) + geom_boxplot() + facet_grid(~treatment)

#rescale percent cover
metadata$cover <- metadata$percent.cover/100

m1 <- glmer(abun ~ blooming + (1|repID), family = poisson(link = "log"), data = metadata)
m2 <- glmer(abun ~ treatment + (1|repID), family = poisson(link = "log"), data = metadata)
m3 <- glmer(abun ~ treatment + blooming + (1|repID), family = poisson(link = "log"), data = metadata)
m4 <- glmer(abun ~ treatment * blooming + (1|repID), family = poisson(link = "log"), data = metadata)
m5 <- glmer(abun ~ (1|repID), family = poisson(link = "log"), data = metadata)
overdisp_fun(m1)
anova(m1, m2, m3, m4, m5, test = "Chisq")
AIC(m1, m2, m3, m4, m5)
summary(m4)
car::Anova(m4, type = 3)
car::Anova(m2, type = 2)

ggplot(data = beetle, aes(Species)) + geom_freqpoly(bins = 20)
shapiro.test(beetle$Species)

outlier_values <- boxplot.stats(beetle$Species)$out  # outlier values.
boxplot(beetle$Species, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

m1 <- lmer(Species ~ treatment + blooming + (1|repID), data = beetle)
car::Anova(m1, type = 2)
summary(m1)
plot(m1)
shapiro.test(m1)
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
shapiro.test(residuals(m1))
