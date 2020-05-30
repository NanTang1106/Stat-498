#Generate two samples with same value of mean and standard deviation.
groupA <- rnorm(n = 100, mean = 180, sd = 40)
groupB <- rnorm(n = 100, mean = 180, sd = 40)

#Drawing two histograms
par(mfow = c(2, 1), mar = c(4,4,2,2))
cBreaks <- seq(-1000, 1000, by = 25)
hist(groupA, breaks = cBreaks, xlim = c(50, 350), col = "gray", main = "")
hist(groupB, breaks = cBreaks, xlim = c(50, 350), col = "gray", main = "")

#Apply two sample t-test on two samples
t.test(groupA, groupB)

groupA <- rnorm(10000, 181, 40)
groupB <- rnorm(10000, 179, 40)

par(mfrow = c(2, 1), mar = c(4,4,2,2))
cBreaks <- seq(-1000, 1000, by = 25)
hist(groupA, breaks = cBreaks, xlim = c(50, 350), col = "gray", main = "")
hist(groupB, breaks = cBreaks, xlim = c(50, 350), col = "gray", main = "")

t.test(groupA, groupB)

#Simulate 1000 experiments.
p_values <- numeric(1000)
for(i in 1 : 1000) {
  groupA <- rnorm(n = 10000, mean = 181, sd = 40)
  groupB <- rnorm(n = 10000, mean = 179, sd = 40)
  p_values[i] <- t.test(groupA, groupB)$p.value
}

sum(p_values < 0.05)

#Simulate 1000 experiments with two samples with largely different mean values and relately small sample size. 
p_values <- numeric(1000)
for(i in 1 : 1000) {
  groupA <- rnorm(n = 20, mean = 190, sd = 40)
  groupB <- rnorm(n = 20, mean = 170, sd = 40)
  p_values[i] <- t.test(groupA, groupB)$p.value
}

sum(p_values < 0.05)

#Generate 1000 experiments for samples of various sizes.
simulations <- 1000
sample.size <- c(seq(10, 100, by = 10),200, 500)
p_values <- array(NA, dim = c(simulations, length(sample.size)))
for(i in 1 : simulations) {
  for(j in 1 : length(sample.size)) {
    groupA <- rnorm(sample.size[j], mean = 190, sd = 40)
    groupB <- rnorm(sample.size[j], mean = 170, sd = 40)
    p_values[i,j] <- t.test(groupA, groupB)$p.value
  }
}

prop.sum <- numeric(length(sample.size))
for (i in 1 : length(sample.size)) {
  prop.sum[i] <- sum(p_values[, i] < 0.05) / simulations
}
prop.sum

par(mfrow = c(1, 1))
barplot(prop.sum, ylim = 0:1, xlab = "Sample Size", ylab = "Proportion",
        names.arg = sample.size, col = "skyblue", main = "Proportion of P-Value < 0.05")

prop.sum <- numeric(length(sample.size))
for (i in 1 : length(sample.size)) {
  prop.sum[i] <- sum(p_values[, i] <= 0.01) / simulations
}
prop.sum

par(mfrow = c(1, 1))
barplot(prop.sum, ylim = 0:1, xlab = "Sample Size", ylab = "Proportion",
        names.arg = sample.size, col = "skyblue", main = "Proportion of P-Value < 0.01")


cDiff <- rep(NA, 10000)
p_values <- numeric(10000)

for(i in 1 :10000) {
  groupA <- rnorm(5, 180, 40)
  groupB <- rnorm(5, 185, 40)
  cDiff[i] <- mean(groupB) - mean(groupA)
  p_values[i] <- t.test(groupA, groupB)$p.value
}

hist(cDiff, main = "", col = "gray")
hist(cDiff[p_values <= 0.05], breaks = 20, main = "", col = "gray")











