library(ggplot2)
library(forecast)
library(tseries)

setwd("E:/Desktop/STAT 498/Homework/FinalProject")

drought.data <- read.table("Data_Set_Drought_ARIMA.csv", sep = ",", header = T)

droughtTseries <- ts(drought.data$CA.drought)

drought.adf <- adf.test(droughtTseries) #p-value = 0
drought.kpss <- kpss.test(droughtTseries) #0.078
drought.stl <- stl(droughtTseries, s.window = "periodic") 

drought.arima <- auto.arima(droughtTseries, xreg = drought.data$Date)
drought.forecast <- forecast(drought.arima)
