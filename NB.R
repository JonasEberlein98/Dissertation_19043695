data <- 
  data %>% 
  mutate(bugbinary = factor(bugbinary))
spl = sample.split(data$bugbinary, SplitRatio = 0.7)
dataTrain = subset(data, spl==TRUE)
dataTest = subset(data, spl==FALSE)
x = dataTrain[,80]
y = dataTrain$bugbinary
model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))
Predict <- predict(model,newdata = dataTest)
confusionMatrix(Predict, dataTest$bugbinary)
