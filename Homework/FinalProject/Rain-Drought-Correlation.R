library(ggplot2)
library(forecast)

TempData <- read.table("E:/Desktop/STAT 498/Homework/FinalProject/CAtemp.txt", header = TRUE, sep = ",")
droughtData <- read.table("E:/Desktop/STAT 498/Homework/FinalProject/CAdrought.txt", header = TRUE, sep = ",")

Tempeture <- TempData$Value
Drought <- droughtData$Value

rain.drou.cor <- cor(Tempeture, Drought)

drought.temp.data <- data.frame(Tempeture, Drought)

ggplot(data = drought.temp.data) + 
  geom_smooth(mapping = aes(x = Tempeture, y = Drought)) +
  geom_point(mapping = aes(x = Tempeture, y = Drought)) +
  labs(
    title = "Drought - Temperature (F) Regression Line 1895/12 - 2017/12",
    x ="Temperature (F)",
    y ="Palmer Drought Index"
  )
