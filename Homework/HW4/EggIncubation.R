fivec <- c(1156.2, 1145.8, 1156.2, 1176.6, 1196.8, 1116.3, 1156.2, 1135.6)
tenc <- c(1153.8, 1132.6, 1146.7, 1203.7, 1160.8, 1180.9, 1146.7, 1153.8)
dayseas <- c(1167.5, 1167.5, 1133.3, 1151.4, 1121.2, 1142.9, 1121.2, 1121.2)
doubledaily <- c(1184.8, 1193.2, 1176.3, 1201.5, 1159.4, 1167.8, 1150.8, 1150.8)
flipflop <- c(1175.3, 1182.3, 1182.3, 1222.0, 1168.1, NA, 1144.3, 1125.3)

sample.size <- 8
mean5 <- mean(fivec)
meanDouble <- mean(doubledaily)
diff.mean <- mean5 - meanDouble
diff.sd <- sqrt((var(fivec) + var(doubledaily)) / 2)
diff.se <- diff.sd / sqrt(sample.size / 2)

#Using se and 1.96 standard approximation to calculate the confidence interval for difference between two sample means.
diff.CI <- c(diff.mean - diff.se * 1.96, diff.mean + diff.se * 1.96)


#Apply a bootstrap method to calculate confidence interval of diference between means.
simulations <- 1000
diff.value <- numeric(simulations)
for (i in 1 : simulations) {
  sample5 <- sample(fivec, length(fivec), replace = T)
  sampleDouble <- sample(doubledaily, length(doubledaily), replace = T)
  diff.value[i] <- mean(sample5) - mean(sampleDouble)
}
diff.Boot.CI <- quantile(diff.value, prob = c(0.025, 0.975))


#Apply permutation test on these two data set.
days <- c(fivec, doubledaily)
treatment <- rep(c("fivec", "double"), c(8, 8))
diff.treatment <- numeric(simulations) 
for (i in 1 : simulations) {
  sample.treat <- sample(treatment, replace = F)
  diff.treatment[i] <- mean(days[sample.treat == "fivec"]) - mean(days[sample.treat == "double"])
}
extre.rate <- (sum(diff.treatment >= abs(diff.mean)) + sum(diff.treatment <= -abs(diff.mean))) / simulations

diff.perm.CI <- quantile(diff.treatment, prob = c(0.025, 0.975))


#Use bootstrapping to calculate the confidence interval for difference between mean of doubledaily
#and the combined mean of fivec and tenc.
diff.value2 <- numeric(simulations)
for (i in 1 : simulations) {
  sampleDouble <- sample(doubledaily, length(doubledaily), replace = T)
  sampleComb <- sample(c(fivec, tenc), length(fivec) * 2, replace = T)
  diff.value2[i] <- mean(sampleDouble) - mean(sampleComb)
}
diff.Boot.CI2 <- quantile(diff.value2, probs = c(0.025, 0.975))
