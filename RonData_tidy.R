# response variable = Class

library(tidymodels)
library(tidyverse)
library(ECoL)
library(discrim)
library(e1071)

overlapping(bugbinary~., data, measures= "all")
neighborhood(bugbinary~., data, measures= "all")
linearity(bugbinary~., data, measures= "all")
dimensionality(bugbinary~., data, measures= "all")
balance(bugbinary~., data, measures= "all")
network(bugbinary~., data, measures= "all")
correlation(bugbinary~., data, measures= "all")
smoothness(bugbinary~., data, measures= "all")

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
metricsSet(C50classWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
C50_final <- data_wf %>%
  add_model(C50Model) %>%
  last_fit(trainIndex)
collect_metrics(C50_final)
#NB
NBclassWithPredictions <- testSet_processed %>%
  dplyr::select(bugbinary) %>%
  bind_cols(predict(NBFit, testSet_processed))
metricsSet(NBclassWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
NB_final <- data_wf %>%
  add_model(NBModel) %>%
  last_fit(trainIndex)
collect_metrics(NB_final)
#NN
NNclassWithPredictions <- testSet_processed %>%
  dplyr::select(bugbinary) %>%
  bind_cols(predict(NNFit, testSet_processed))
metricsSet(NNclassWithPredictions, truth = bugbinary, estimate = .pred_class, event_level = "first")
NN_final <- data_wf %>%
  add_model(NNModel) %>%
  last_fit(trainIndex)
collect_metrics(NN_final)



