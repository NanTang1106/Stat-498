library(ggplot2)
setwd("E:/Desktop/STAT 498/Homework/FinalProject")

#import data
CA.drought <- read.table("CADrought.txt", header = T, sep = ",")
CA.rain <- read.table("CARain.txt", header = T, sep = ",")
WA.drought <- read.table("WADrought.txt", header = T, sep = ",")
WA.rain <- read.table("WARain.txt", header = T, sep = ",")

CA.drought <- cbind(CA.drought, State = rep("CA", 123))
CA.rain <- cbind(CA.rain, State = rep("CA", 123))
WA.drought <- cbind(WA.drought, State = rep("WA", 123))
WA.rain <- cbind(WA.rain, State = rep("WA", 123))

#combine two states' drought and percipitation values respectively
drought.comp <- data.frame(rbind(CA.drought, WA.drought))
rain.comp <- data.frame(rbind(CA.rain, WA.rain))

#Histogram of drought comparison.
ggplot(data = drought.comp) + 
  geom_histogram(mapping = aes(x = Value, fill = State), binwidth = 0.2, position = "dodge") +
  labs(
    title = "Drought in WA and CA",
    x ="Annually Average Palmer Drought Index",
    y ="Count",
    color = "State Abbreviation"
  )

#mean difference in palmer index
drought.mean.diff <- mean(CA.drought$Value) - mean(WA.drought$Value)

#Permutation test of 1000 simulations.
simulations <- 1000
drought.value <- drought.comp$Value
drought.perm <- numeric(simulations)
state.perm <- rep(c("CA", "WA"), c(123, 123))
for(i in 1 : simulations) {
  sample.state <- sample(state.perm, replace = F)
  drought.perm[i] = mean(drought.value[sample.state == "CA"]) - 
    mean(drought.value[sample.state == "WA"])
}
drought.pvalue <- (sum(drought.perm > abs(drought.mean.diff)) + 
                     sum(drought.perm < -abs(drought.mean.diff))) / simulations
drought.pvalue ##0.612

#Histogram of precipitation comparison.
ggplot(data = rain.comp) + 
  geom_histogram(mapping = aes(x = Value, fill = State), binwidth = 0.6, position = "dodge") +
  labs(
    title = "Precipitation in WA and CA",
    x ="Annual Average Precipitation (mm)",
    y ="Count",
    color = "State Abbreviation"
  )

#mean differenc in precipitation
percip.mean.diff <- mean(CA.rain$Value) - mean(WA.rain$Value)

#Permutation test of 1000 simulations
simulations <- 1000
percip.value <- rain.comp$Value
percip.perm <- numeric(simulations)
state.perm <- rep(c("WA", "CA"), c(123, 123))
for(i in 1 : simulations) {
  sample.state <- sample(state.perm, replace = F)
  percip.perm[i] = mean(percip.value[sample.state == "CA"]) - 
    mean(percip.value[sample.state == "WA"])
}
percip.pvalue <- (sum(percip.perm > abs(percip.mean.diff)) + 
                    sum(percip.mean.diff < -abs(percip.mean.diff))) / simulations
percip.pvalue ##0

