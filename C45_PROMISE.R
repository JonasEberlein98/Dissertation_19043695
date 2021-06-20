library(ranger)
library(tidymodels)
library(readr)
library(RWeka)
library(readxl)
library(caTools)
data <- read_excel("~/Desktop/data.xlsx")
data <- 
                         data %>% 
                                                       mutate(bugbinary = factor(bugbinary))
spl = sample.split(data$bugbinary, SplitRatio = 0.7)
spl = sample.split(data$bugbinary, SplitRatio = 0.7)
dataTrain = subset(data, spl==TRUE)
dataTest = subset(data, spl==FALSE)
trainJ48 <- J48(bugbinary~., dataTrain)
dataTest.pred <- predict(trainJ48, newdata = dataTest)
table(dataTest$bugbinary, dataTest.pred)
precision(data, dataTest$bugbinary, dataTest.pred)


