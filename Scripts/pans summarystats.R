#this script is for summary stats for pantraps

metadata <- read.csv("Data/outputmetadata.csv")



str(metadata)
metadata %>% group_by(blooming) %>% summarise(., sum(abun))
sum(metadata$abun)
#have to add zero rows to take means - done
metadata %>% group_by(blooming, treatment) %>% summarise(., mean(Even))




abun.normal <- fitdist(metadata$abun, "norm", method = c("mle", "mme", "qme", "mge"))
abun.poisson <- fitdist(metadata$abun, "pois", method = c("mle", "mme", "qme", "mge"))
abun.gamma <- fitdist(metadata$abun, "gamma", method = "mge")
summary(abun.poisson)
summary(abun.normal)
shapiro.test(metadata$abun)

plotdist(metadata$abun, "norm", para=list(mean=mean(metadata$abun), sd=sd(metadata$abun)))
plotdist(metadata$abun, "gamma", para=list(lambda = mean(metadata$abun)))
descdist(metadata$abun)
plotdist(metadata$abun)

?family
ggplot(metadata, aes(abun)) + geom_density()
plot(metadata$abun, type="h")
quantile(metadata$abun)
