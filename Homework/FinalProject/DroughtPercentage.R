library(ggplot2)
setwd("E:/Desktop/STAT 498/Homework/FinalProject")

#import data
drought.percent <- read.table("PercentD0-D4CAORWA.csv", sep = ",", header = T)
CA.drought.percent <- drought.percent[drought.percent$StateAbbreviation == "CA",]
OR.drought.percent <- drought.percent[drought.percent$StateAbbreviation == "OR",]
WA.drought.percent <- drought.percent[drought.percent$StateAbbreviation == "WA",]

ggplot(data = drought.percent) +
  geom_smooth(mapping = aes(x = ReleaseDate, y = D0, color = StateAbbreviation)) +
  labs(
    title = "D0(Adnormally Drought) Percent Area in West Coast",
    x ="Weekly 2000/1/4 - 2018/5/15",
    y ="Percent Area (%)",
    color = "State Abbreviation"
  )
  
ggplot(data = drought.percent) +
  geom_smooth(mapping = aes(x = ReleaseDate, y = D1, color = StateAbbreviation)) +
  labs(
    title = "D1(Moderate Drought) Percent Area in West Coast",
    x ="Weekly 2000/1/4 - 2018/5/15",
    y ="Percent Area (%)",
    color = "State Abbreviation"
  )

ggplot(data = drought.percent) +
  geom_smooth(mapping = aes(x = ReleaseDate, y = D2, color = StateAbbreviation)) +
  labs(
    title = "D2(Sever Drought) Percent Area in West Coast",
    x ="Weekly 2000/1/4 - 2018/5/15",
    y ="Percent Area (%)",
    color = "State Abbreviation"
  )

ggplot(data = drought.percent) +
  geom_smooth(mapping = aes(x = ReleaseDate, y = D3, color = StateAbbreviation)) +
  labs(
    title = "D3(Extreme Drought) Percent Area in West Coast",
    x ="Weekly 2000/1/4 - 2018/5/15",
    y ="Percent Area (%)",
    color = "State Abbreviation"
  )

ggplot(data = drought.percent) +
  geom_smooth(mapping = aes(x = ReleaseDate, y = D4, color = StateAbbreviation)) +
  labs(
    title = "D4(Exceptional Drought) Percent Area in West Coast",
    x ="Weekly 2000/1/4 - 2018/5/15",
    y ="Percent Area (%)",
    color = "State Abbreviation"
  )


