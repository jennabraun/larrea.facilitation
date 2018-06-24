#Calculates reference plant species mean pollen diameter
library(ggplot2)
library(dplyr)

data <- read.csv("Clean Data/pollen_measurements.csv")
summary(data)

ggplot(data, aes(reorder(species, - X.2), y = X.2)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1, vjust = 0.5))

summary <- data %>% group_by(species) %>% summarise(mean = mean(X.2), sd = sd(X.2))
summary <- mutate(summary, se = sd/sqrt(320))                                                                

                  