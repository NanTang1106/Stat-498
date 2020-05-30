library(ggplot2)
library(forecast)
setwd("E:/Desktop/STAT 498/Homework/FinalProject")

#import data
droughtData <- read.table("CAdrought.txt", header = TRUE, sep = ",")

#Translate palmer index into square root of it.
data.sqrttrans <- function(x) {
    if (x > 0) {
      return(x = sqrt(x))
    } else {
      return(x = -sqrt(abs(x)))
    }
}
revise <- numeric()
for(i in 1:nrow(droughtData)) {
  revise <- c(revise, data.sqrttrans(droughtData$Value[i]))
}
droughtDataRevise <- cbind(droughtData, revise)

#Plot linear and smoothed regression line of Square root of Palmer index value to time. 
ggplot(data = droughtDataRevise) + 
  geom_line(mapping = aes(x = Date, y = revise), color = "#999999") + 
  geom_smooth(mapping = aes(x = Date, y = revise), color = "#00CC33", method = "lm") +
  geom_smooth(mapping = aes(x = Date, y = revise), color = "#6699FF") +
  labs(
    title = "Time - Square Root of Palmer Drought Index",
    x ="Yearly 1895/12 - 2017/12",
    y ="Square Root of Palmer Drought Index"
  )

#Apply time series model on Palmer index.
droughtTseries <- ts(droughtData[,2])

#check if drought data is stationary
drought.adf <- adf.test(droughtTseries) ##p-value = 0.01, reject null hypothesis, alternative is stationary.
drought.kpss <- kpss.test(droughtTseries) ##p-value = 0.07 
drought.stl <- stl(droughtTseries, s.window = "periodic") ##error no seasonality

#Apply arima(1,0,0)
drought.arima <- auto.arima(droughtTseries) ##se = 0.0845

#Predict future trend of Palmer index in ten years. 
drought.forecast <- forecast(drought.arima)
plot(forecast(drought.arima))

autoplot(droughtTseries) 
autoplot(drought.forecast)
