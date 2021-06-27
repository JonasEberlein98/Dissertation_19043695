library(caret)
library(tidymodels)
library(e1071)

#model1
#mutate to factor
data <- 
  data %>% 
  mutate(bugbinary = factor(bugbinary))
#split data
spl = sample.split(data$bugbinary, SplitRatio = 0.7)
dataTrain = subset(data, spl==TRUE)
dataTest = subset(data, spl==FALSE)
#naive bayes
nb = naiveBayes(bugbinary ~ ., data = dataTrain)
nb_trn_pred = predict(nb, dataTrain)
nb_tst_pred = predict(nb, dataTest)

#function to attain error
calc_class_err <- function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(predicted = nb_trn_pred, actual = dataTrain$bugbinary)
calc_class_err(predicted = nb_tst_pred, actual = dataTest$bugbinary)

# performance metrics
precision(data, dataTest$bugbinary, nb_tst_pred) #does not work
f_meas(data, dataTest$bugbinary, nb_tst_pred) #works
recall(data, dataTest$bugbinary, nb_tst_pred) #does not work
mcc(data, dataTest$bugbinary, nb_tst_pred) #works
roc_auc(dataTest, dataTest$bugbinary, 1) #works

#model2
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
