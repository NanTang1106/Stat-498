#Simulate 100 sets with x, y variable, each of them has a sample size of 20.
#Plot samples with p-values < 0.05, and calculate the rate of statistically significant samples.
simulations <- 100
signifSum <- 0
par(mfrow = c(3, 3)) #set up a 3 * 3 grid.

for(i in 1 : simulations) {
  y <- rnorm(n = 20, mean = 50, sd = 3)
  x <- rnorm(n = 20, mean = 10, sd = 1)
  reg <- lm(y~x)
  pVal <- summary(reg)$coefficients[2,4]
  if (pVal < 0.05) {
    plot(x, y, pch = 16, col = "skyblue")
    abline(reg, lty = 1, col = "skyblue4")
    signifSum <- signifSum + 1
  }
}
signifRate <- signifSum / simulations
signifRate
##Result is 0.05


par(mfrow = c(3, 3))
signifSum <- 0

for(i in 1 : simulations) {
  y <- rnorm(n = 20, mean = 50, sd = 3)
  x <- rnorm(n = 20, mean = 10, sd = 1)
  reg <- lm(y~x)
  pVal <- summary(reg)$coefficients[2,4]
  if (pVal < 0.1) {
    plot(x, y, pch = 16, col = "skyblue")
    abline(reg, lty = 1, col = "skyblue4")
    signifSum <- signifSum + 1
  }
}
signifRate <- signifSum / simulations
signifRate
##Result is 0.09


#Simulate 1000 sets with x, y, which both come from independent normal distributions.
#Remove largest residual from each sample.
#Calculate the rate of statistically significant samples after being removed of largest residual.
signifSum <- 0
simulations <- 1000

for (i in 1 : simulations) {
  y <- rnorm(n = 20, mean = 50, sd = 3)
  x <- rnorm(n = 20, mean = 10, sd = 1)
  reg <- lm(y~x)
  largestRes <- which.max(abs(resid(reg)))
  revised_x <- x[-largestRes]
  revised_y <- y[-largestRes]
  revisedReg <- lm(revised_y~revised_x)
  pVal <- summary(revisedReg)$coefficients[2,4]
  if (pVal < 0.05) {
    signifSum <- signifSum + 1
  }
}
signifRate <- signifSum / simulations
signifRate
##Result is 0.096


signifSum <- 0
for (i in 1 : simulations) {
  y <- rnorm(n = 20, mean = 50, sd = 3)
  x <- rnorm(n = 20, mean = 10, sd = 1)
  reg <- lm(y~x)
  largestRes <- which.max(abs(resid(reg)))
  revised_x <- x[-largestRes]
  revised_y <- y[-largestRes]
  revisedReg <- lm(revised_y~revised_x)
  pVal <- summary(revisedReg)$coefficients[2,4]
  if (pVal < 0.1) {
    signifSum <- signifSum + 1
  }
}
signifRate <- signifSum / simulations
signifRate
##Result is 0.177


#Compare AIC results for null model and linear model
simulations <- 1000
nullSum <- 0
for (i in 1 : 1000) {
  y <- rnorm(n = 20, mean = 50, sd = 3)
  x <- rnorm(n = 20, mean = 10, sd = 1)
  nullModel <- lm(y~1)
  reg <- lm(y~x)
  getAIC <- AIC(nullModel, reg)
  if (getAIC[1, 2] < getAIC[2, 2]) {
    nullSum <- nullSum + 1
  }
}
nullSumRate <- nullSum / simulations
nullSumRate
##Result is 0.834