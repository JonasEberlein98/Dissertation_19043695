library(tidymodels)
library(tidyverse)
library(ECoL)
library(discrim)
library(e1071)
library("Hmisc")

#ONLY RUN ONCE!!!! 
#create table to gather the complexity metrics
tab <- matrix(nrow = 1, ncol = 26)
tab <- data.frame(tab)
tab = tab[-c(1),]
#ONLY RUN ONCE!!!! 
#create table to gather the performance metrics
tab2 <- matrix(nrow = 1, ncol = 18)
tab2 <- data.frame(tab2)
tab2 = tab2[-c(1),]

#Run every time
#import data and call the dataset 'data'

#calculate the complexity metrics and record them in a new row in table
overlap <- overlapping(bugbinary~., data, measures= "all")
neighbor <- neighborhood(bugbinary~., data, measures= "all")
lin <- linearity(bugbinary~., data, measures= "all")
dim <- dimensionality(bugbinary~., data, measures= "all")
bal <- balance(bugbinary~., data, measures= "all")
net <- network(bugbinary~., data, measures= "all")
corr <- correlation(bugbinary~., data, measures= "all")

tab[nrow(tab)+1,] = c(nrow(tab)+1,overlap$F1[1],overlap$F1v[1],overlap$F2[1], overlap$F3[1],overlap$F4[1],neighbor$N1[1],neighbor$N2[1],neighbor$N3[1],neighbor$N4[1],neighbor$T1[1],neighbor$LSC[1],lin$L1[1],lin$L2[1],lin$L3[1],dim[1],dim[2],dim[3],bal$C1,bal$C2,net$Density,net$ClsCoef,net$Hubs[1],corr$C2[1],corr$C3[1],corr$C4[1])

#code to train and test classifiers + calculate performance metrics

data <- 
  data %>% 
  mutate(bugbinary = factor(bugbinary))

set.seed(713)
trainIndex <- initial_split(data, strata = bugbinary)
trainingSet <- training(trainIndex)
testSet <- testing(trainIndex)

dataRecipe <- recipe(bugbinary ~ ., data = trainingSet) # %>% step_range(all_predictors(), -all_nominal(), min = 0, max = 1)

trainingSet_processed <- dataRecipe %>%
  prep(trainingSet) %>%
  bake(trainingSet)

testSet_processed <- dataRecipe %>%
  prep(testSet) %>%
  bake(testSet)

C50Model <- decision_tree() %>%
  set_mode("classification") %>%
  set_engine("C5.0")
NBModel <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR")
NNModel <- mlp() %>%
  set_mode("classification") %>%
  set_engine("nnet")

C50Fit <- fit(C50Model, bugbinary ~ ., data = trainingSet_processed)
NBFit <- fit(NBModel, bugbinary ~ ., data = trainingSet_processed)
NNFit <- fit(NNModel, bugbinary ~ ., data = trainingSet_processed)

#yardstick to evaluate performance
set.seed(713)
#create workflow
data_wf <- workflow() %>%
  add_formula(bugbinary ~ .)
#roc_auc has to be done seperately with the collect_metrics() function as it gave an error
metricsSet <- metric_set(yardstick::precision, yardstick::f_meas , yardstick::recall , yardstick::mcc , yardstick::accuracy)
# data frame that binds predictions to test set
#C50
C50classWithPredictions <- testSet_processed %>%
  dplyr::select(bugbinary) %>%
  bind_cols(predict(C50Fit, testSet_processed))
C50performance1 <- metricsSet(C50classWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
C50_final <- data_wf %>%
  add_model(C50Model) %>%
  last_fit(trainIndex)
C50performance2 <- collect_metrics(C50_final)
#NB
NBclassWithPredictions <- testSet_processed %>%
  dplyr::select(bugbinary) %>%
  bind_cols(predict(NBFit, testSet_processed))
NBperformance1 <- metricsSet(NBclassWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
NB_final <- data_wf %>%
  add_model(NBModel) %>%
  last_fit(trainIndex)
NBperformance2 <- collect_metrics(NB_final)
#NN
NNclassWithPredictions <- testSet_processed %>%
  dplyr::select(bugbinary) %>%
  bind_cols(predict(NNFit, testSet_processed))
NNperformance1 <- metricsSet(NNclassWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
NN_final <- data_wf %>%
  add_model(NNModel) %>%
  last_fit(trainIndex)
NNperformance2 <- collect_metrics(NN_final)

#store performance metrics in table

tab2[nrow(tab2)+1,] = c(nrow(tab2)+1,C50performance1[1,3], C50performance1[2,3], C50performance1[3,3], C50performance1[4,3], C50performance2[2,3], C50performance1[5,3], NBperformance1[1,3], NBperformance1[2,3], NBperformance1[3,3], NBperformance1[4,3], NBperformance2[2,3], NBperformance1[5,3], NNperformance1[1,3], NNperformance1[2,3], NNperformance1[3,3], NNperformance1[4,3], NNperformance2[2,3], NNperformance1[5,3])

#ONLY AFTER >4 rows in both tables have been filled
#merge tables
tab3 <- merge(tab, tab2, by="X1")
#person correlation
res2 <- rcorr(as.matrix(tab3))